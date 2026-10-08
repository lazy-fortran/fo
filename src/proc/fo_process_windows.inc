/* Native Windows process authority. Included only by fo_process.c on _WIN32.
   Windows 10 / Server 2016 or later supplies atomic job assignment at spawn. */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0a00
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <io.h>
#include <limits.h>
#ifndef ESTALE
#define ESTALE 116 /* Exact process birth no longer matches the owned identity. */
#endif

extern wchar_t *fx_win32_utf16(const char *utf8);
extern wchar_t *fx_win32_command_line(const char *packed, int packed_len, int n_args);
extern int fx_win32_job_tree_poll(HANDLE job, void **tracker);
extern int fx_win32_job_tree_drain(HANDLE job, void **tracker, DWORD timeout_ms);
extern void fx_win32_job_tree_free(void **tracker);
extern int fo_gremlin_process_matches(int pid, const char *start);

struct fo_windows_process {
    DWORD pid;
    uint64_t birth;
    HANDLE process, job;
    void *tracked;
    struct fo_windows_process *next;
};
static struct fo_windows_process *windows_processes;
static INIT_ONCE process_init = INIT_ONCE_STATIC_INIT;
static CRITICAL_SECTION process_lock;
static HANDLE scope_job;
static HANDLE scope_guardian, scope_stop;
static char *scope_directory;
static int heartbeats_suppressed;

static BOOL CALLBACK initialize_process_lock(PINIT_ONCE once, PVOID arg, PVOID *context) {
    (void)once; (void)arg; (void)context;
    InitializeCriticalSection(&process_lock);
    return TRUE;
}
static void lock_processes(void) {
    InitOnceExecuteOnce(&process_init, initialize_process_lock, NULL, NULL);
    EnterCriticalSection(&process_lock);
}
static void unlock_processes(void) { LeaveCriticalSection(&process_lock); }

static int windows_error(DWORD error) {
    switch (error) {
    case ERROR_SUCCESS: return 0;
    case ERROR_FILE_NOT_FOUND: case ERROR_PATH_NOT_FOUND: return ENOENT;
    case ERROR_ACCESS_DENIED: case ERROR_PRIVILEGE_NOT_HELD: return EACCES;
    case ERROR_NOT_ENOUGH_MEMORY: case ERROR_OUTOFMEMORY: return ENOMEM;
    case ERROR_INVALID_PARAMETER: case ERROR_BAD_ARGUMENTS: return EINVAL;
    case ERROR_NOT_SUPPORTED: case ERROR_CALL_NOT_IMPLEMENTED: return ENOTSUP;
    case ERROR_ALREADY_EXISTS: case ERROR_FILE_EXISTS: return EEXIST;
    case ERROR_INVALID_HANDLE: return EBADF;
    default: return EIO;
    }
}
static uint64_t filetime_value(FILETIME value) {
    return ((uint64_t)value.dwHighDateTime << 32) | value.dwLowDateTime;
}
static uint64_t handle_birth(HANDLE process) {
    FILETIME created, exited, kernel, user;
    if (!GetProcessTimes(process, &created, &exited, &kernel, &user)) return 0;
    return filetime_value(created);
}
static int utf8_copy(const wchar_t *source, char *output, int capacity) {
    if (!output || capacity <= 0) return EINVAL;
    int length = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, source, -1,
        output, capacity, NULL, NULL);
    if (!length) { output[0] = 0; return windows_error(GetLastError()); }
    return 0;
}
static const wchar_t *environment_equals(const wchar_t *entry) {
    return wcschr(entry + (entry[0] == L'='), L'=');
}
static int environment_same_key(const wchar_t *left, const wchar_t *right) {
    const wchar_t *a = environment_equals(left), *b = environment_equals(right);
    if (!a || !b || a - left != b - right) return 0;
    return CompareStringOrdinal(left, (int)(a - left), right, (int)(b - right), TRUE)
        == CSTR_EQUAL;
}
static int environment_compare(const void *a, const void *b) {
    const wchar_t *left = *(wchar_t *const *)a, *right = *(wchar_t *const *)b;
    return CompareStringOrdinal(left, -1, right, -1, TRUE) - CSTR_EQUAL;
}
static void environment_free(wchar_t **entries, size_t count) {
    for (size_t i = 0; i < count; ++i) free(entries[i]);
    free(entries);
}
static wchar_t *child_environment(const char *overrides, char *const override_list[]) {
    wchar_t *inherited = GetEnvironmentStringsW();
    wchar_t **entries = NULL, *block = NULL;
    size_t count = 0, capacity = 32, length = 1;
    if (!inherited) { errno = windows_error(GetLastError()); return NULL; }
    entries = calloc(capacity, sizeof(*entries));
    if (!entries) goto failed;
    for (const wchar_t *p = inherited; *p; p += wcslen(p) + 1) {
        if (count == capacity) {
            capacity *= 2;
            wchar_t **grown = realloc(entries, capacity * sizeof(*entries));
            if (!grown) goto failed;
            entries = grown;
        }
        entries[count] = _wcsdup(p);
        if (!entries[count]) goto failed;
        ++count;
    }
    FreeEnvironmentStringsW(inherited); inherited = NULL;
    size_t override_index = 0;
    for (const char *p = override_list ? override_list[0] : overrides; p && *p;) {
        const char *ending = override_list ? NULL : strchr(p, ';');
        size_t used = ending ? (size_t)(ending - p) : strlen(p);
        if (used) {
            char *raw = malloc(used + 1);
            if (!raw) goto failed;
            memcpy(raw, p, used); raw[used] = 0;
            wchar_t *value = fx_win32_utf16(raw); free(raw);
            if (!value) goto failed;
            const wchar_t *equals = environment_equals(value);
            if (!equals || equals == value) { free(value); errno = EINVAL; goto failed; }
            size_t slot;
            for (slot = 0; slot < count; ++slot)
                if (environment_same_key(entries[slot], value)) break;
            if (slot == count && count == capacity) {
                capacity *= 2;
                wchar_t **grown = realloc(entries, capacity * sizeof(*entries));
                if (!grown) { free(value); goto failed; }
                entries = grown;
            }
            if (slot < count) free(entries[slot]); else ++count;
            entries[slot] = value;
        }
        p = override_list ? override_list[++override_index] : (ending ? ending + 1 : NULL);
    }
    qsort(entries, count, sizeof(*entries), environment_compare);
    for (size_t i = 0; i < count; ++i) length += wcslen(entries[i]) + 1;
    if (length == 1) length = 2;
    block = calloc(length, sizeof(*block));
    if (!block) goto failed;
    wchar_t *cursor = block;
    for (size_t i = 0; i < count; ++i) {
        size_t used = wcslen(entries[i]) + 1;
        memcpy(cursor, entries[i], used * sizeof(*cursor)); cursor += used;
    }
    environment_free(entries, count);
    return block;
failed:
    if (inherited) FreeEnvironmentStringsW(inherited);
    environment_free(entries, count);
    if (!errno) errno = ENOMEM;
    free(block); return NULL;
}
static HANDLE make_job(const wchar_t *name, int *error) {
    HANDLE job = CreateJobObjectW(NULL, name);
    DWORD status = GetLastError();
    if (!job) { *error = windows_error(status); return NULL; }
    if (name && status == ERROR_ALREADY_EXISTS) {
        CloseHandle(job); *error = EEXIST; return NULL;
    }
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = {0};
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation,
        &limits, sizeof(limits))) {
        *error = windows_error(GetLastError()); CloseHandle(job); return NULL;
    }
    *error = 0; return job;
}
static HANDLE child_stream(DWORD standard, int writing) {
    HANDLE source = GetStdHandle(standard), target = NULL;
    if (source && source != INVALID_HANDLE_VALUE &&
        DuplicateHandle(GetCurrentProcess(), source, GetCurrentProcess(), &target,
            0, TRUE, DUPLICATE_SAME_ACCESS)) return target;
    SECURITY_ATTRIBUTES attributes = {sizeof(attributes), NULL, TRUE};
    target = CreateFileW(L"NUL", writing ? GENERIC_WRITE : GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, &attributes, OPEN_EXISTING, 0, NULL);
    return target == INVALID_HANDLE_VALUE ? NULL : target;
}
static HANDLE output_stream(const char *path, int append) {
    if (!path || !*path) return child_stream(STD_OUTPUT_HANDLE, 1);
    wchar_t *wide = fx_win32_utf16(!strcmp(path, "/dev/null") ? "NUL" : path);
    if (!wide) return NULL;
    SECURITY_ATTRIBUTES attributes = {sizeof(attributes), NULL, TRUE};
    HANDLE stream = CreateFileW(wide, GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, &attributes,
        append ? OPEN_ALWAYS : CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    free(wide);
    if (stream == INVALID_HANDLE_VALUE) { errno = windows_error(GetLastError()); return NULL; }
    if (append) {
        LARGE_INTEGER zero = {0};
        if (!SetFilePointerEx(stream, zero, NULL, FILE_END)) {
            errno = windows_error(GetLastError()); CloseHandle(stream); return NULL;
        }
    }
    return stream;
}

static wchar_t *resolve_executable(const char *path, const wchar_t *cwd) {
    wchar_t *wide = fx_win32_utf16(path), *resolved = NULL;
    if (!wide) return NULL;
    if (!wcschr(wide, L'/') && !wcschr(wide, L'\\') && !wcschr(wide, L':')) {
        DWORD used = SearchPathW(NULL, wide, L".exe", 0, NULL, NULL);
        if (used) {
            resolved = calloc((size_t)used + 1, sizeof(*resolved));
            if (resolved && !SearchPathW(NULL, wide, L".exe", used + 1, resolved, NULL)) {
                free(resolved); resolved = NULL;
            }
        }
    } else {
        wchar_t *candidate = wide;
        int relative = wide[0] != L'/' && wide[0] != L'\\' &&
            !(wcslen(wide) > 1 && wide[1] == L':');
        if (relative && cwd) {
            size_t used = wcslen(cwd) + wcslen(wide) + 2;
            candidate = calloc(used, sizeof(*candidate));
            if (candidate) swprintf(candidate, used, L"%ls\\%ls", cwd, wide);
        }
        if (candidate) {
            DWORD used = GetFullPathNameW(candidate, 0, NULL, NULL);
            if (used) {
                resolved = calloc((size_t)used + 1, sizeof(*resolved));
                if (resolved && !GetFullPathNameW(candidate, used + 1, resolved, NULL)) {
                    free(resolved); resolved = NULL;
                }
            }
        }
        if (candidate != wide) free(candidate);
    }
    free(wide);
    if (!resolved) errno = ENOENT;
    return resolved;
}

static int spawn_windows(const char *cwd, const char *packed, int packed_len, int n_args,
    const char *log_file, int append, const char *overrides, char *const override_list[],
    const int redirected[3], struct fo_windows_process **result) {
    STARTUPINFOEXW startup = {0};
    PROCESS_INFORMATION child = {0};
    HANDLE inherited[3] = {NULL}, jobs[2] = {NULL}, scope_copy = NULL;
    size_t streams = 0, job_count = 0;
    SIZE_T attribute_size = 0;
    int error = 0, attributes_initialized = 0;
    wchar_t *command = fx_win32_command_line(packed, packed_len, n_args);
    wchar_t *directory = NULL, *executable = NULL, *environment = NULL;
    struct fo_windows_process *item = calloc(1, sizeof(*item));
    *result = NULL;
    if (!command || !item) { error = command ? ENOMEM : errno; goto finished; }
    if (cwd && *cwd) {
        directory = fx_win32_utf16(cwd);
        if (!directory) { error = errno; goto finished; }
    }
    executable = resolve_executable(packed, directory);
    if (!executable) { error = errno; goto finished; }
    environment = child_environment(overrides, override_list);
    if (!environment) { error = errno; goto finished; }
    item->job = make_job(NULL, &error);
    if (!item->job) goto finished;
    lock_processes();
    if (scope_job && (!scope_guardian || WaitForSingleObject(scope_guardian, 0) != WAIT_TIMEOUT))
        error = EIO;
    if (!error && scope_job && !DuplicateHandle(GetCurrentProcess(), scope_job, GetCurrentProcess(),
        &scope_copy, 0, FALSE, DUPLICATE_SAME_ACCESS)) error = windows_error(GetLastError());
    unlock_processes();
    if (error) goto finished;
    if (scope_copy) jobs[job_count++] = scope_copy;
    jobs[job_count++] = item->job;
    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    if (redirected) {
        for (size_t index = 0; index < 3; ++index) {
            intptr_t handle = _get_osfhandle(redirected[index]);
            if (handle == -1 || !DuplicateHandle(GetCurrentProcess(), (HANDLE)handle,
                GetCurrentProcess(), &inherited[index], 0, TRUE, DUPLICATE_SAME_ACCESS)) {
                error = handle == -1 ? EBADF : windows_error(GetLastError()); goto finished;
            }
            ++streams;
        }
        startup.StartupInfo.hStdInput = inherited[0];
        startup.StartupInfo.hStdOutput = inherited[1];
        startup.StartupInfo.hStdError = inherited[2];
    } else {
        inherited[streams++] = child_stream(STD_INPUT_HANDLE, 0);
        startup.StartupInfo.hStdInput = inherited[0];
        inherited[streams++] = output_stream(log_file, append);
        startup.StartupInfo.hStdOutput = inherited[1];
        if (log_file && *log_file) startup.StartupInfo.hStdError = inherited[1];
        else {
            inherited[streams++] = child_stream(STD_ERROR_HANDLE, 1);
            startup.StartupInfo.hStdError = inherited[2];
        }
    }
    for (size_t i = 0; i < streams; ++i) {
        if (!inherited[i]) { error = errno ? errno : windows_error(GetLastError()); goto finished; }
    }
    InitializeProcThreadAttributeList(NULL, 2, 0, &attribute_size);
    startup.lpAttributeList = malloc(attribute_size);
    if (!startup.lpAttributeList) { error = ENOMEM; goto finished; }
    if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 2, 0, &attribute_size)) {
        error = windows_error(GetLastError()); goto finished;
    }
    attributes_initialized = 1;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0,
        PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited, streams * sizeof(HANDLE), NULL, NULL) ||
        !UpdateProcThreadAttribute(startup.lpAttributeList, 0,
        PROC_THREAD_ATTRIBUTE_JOB_LIST, jobs, job_count * sizeof(HANDLE), NULL, NULL)) {
        error = windows_error(GetLastError()); goto finished;
    }
    if (!CreateProcessW(executable, command, NULL, NULL, TRUE,
        EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT | CREATE_NEW_PROCESS_GROUP,
        environment, directory, &startup.StartupInfo, &child)) {
        error = windows_error(GetLastError()); goto finished;
    }
    item->process = child.hProcess;
    item->pid = child.dwProcessId;
    item->birth = handle_birth(item->process);
    CloseHandle(child.hThread);
    if (!item->birth || item->pid > INT_MAX) {
        error = EIO;
        TerminateJobObject(item->job, 125);
        WaitForSingleObject(item->process, 10000);
        goto finished;
    }
    *result = item; item = NULL;
finished:
    if (attributes_initialized) DeleteProcThreadAttributeList(startup.lpAttributeList);
    free(startup.lpAttributeList);
    for (size_t i = 0; i < streams; ++i) if (inherited[i]) CloseHandle(inherited[i]);
    if (scope_copy) CloseHandle(scope_copy);
    if (item) {
        if (item->process) CloseHandle(item->process);
        if (item->job) CloseHandle(item->job);
        fx_win32_job_tree_free(&item->tracked);
        free(item);
    }
    free(command); free(directory); free(executable); free(environment);
    return error;
}

static int job_accounting(HANDLE job, DWORD *active, long long *cpu_ms) {
    JOBOBJECT_BASIC_ACCOUNTING_INFORMATION information;
    if (!QueryInformationJobObject(job, JobObjectBasicAccountingInformation,
        &information, sizeof(information), NULL)) return windows_error(GetLastError());
    if (active) *active = information.ActiveProcesses;
    if (cpu_ms) *cpu_ms = (information.TotalKernelTime.QuadPart +
        information.TotalUserTime.QuadPart) / 10000;
    return 0;
}
static int drain_job(struct fo_windows_process *item, int graceful) {
    DWORD active = 0;
    int error = job_accounting(item->job, &active, NULL);
    if (!error) error = fx_win32_job_tree_poll(item->job, &item->tracked);
    if (error) return error;
    if (graceful && active) {
        /* Only this child process group receives CTRL_BREAK. Job termination
           follows the existing bounded grace, including no-console children. */
        GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, item->pid);
        ULONGLONG deadline = GetTickCount64() + 2000;
        do {
            Sleep(20);
            error = job_accounting(item->job, &active, NULL);
            if (!error) error = fx_win32_job_tree_poll(item->job, &item->tracked);
            if (error) return error;
            if (!active) break;
        } while (GetTickCount64() < deadline);
    }
    return fx_win32_job_tree_drain(item->job, &item->tracked, 10000);
}
static void forget_windows_process(struct fo_windows_process *item) {
    CloseHandle(item->process); CloseHandle(item->job);
    fx_win32_job_tree_free(&item->tracked);
    free(item);
}
static int verify_windows_process(struct fo_windows_process *item) {
    return handle_birth(item->process) == item->birth ? 0 : ESTALE;
}
static int heartbeat_seconds(void) {
    if (heartbeats_suppressed) return 0;
    const char *value = getenv("FO_HEARTBEAT_SECONDS");
    if (!value || !*value) return 10;
    int seconds = atoi(value);
    return seconds >= 0 ? seconds : 10;
}
static void emit_windows_heartbeat(const char *log) {
    const char *message = "fo: command still running; output is captured\n";
    fputs(message, stderr); fflush(stderr);
    if (!log || !*log || !strcmp(log, "/dev/null") || !strcmp(log, "NUL")) return;
    wchar_t *wide = fx_win32_utf16(log);
    if (!wide) return;
    FILE *output = _wfopen(wide, L"ab"); free(wide);
    if (output) { fputs(message, output); fclose(output); }
}
static int run_windows(const char *cwd, const char *packed, int packed_len, int n_args,
    const char *log, int append, int cpu_budget_s, int wall_budget_s, int heartbeat_s,
    const char *environment, int *timeout_kind, long long *cpu_ms, long long *wall_ms) {
    struct fo_windows_process *item;
    ULONGLONG began = GetTickCount64(), heartbeat = began;
    int error = spawn_windows(cwd, packed, packed_len, n_args, log, append, environment, NULL, NULL, &item);
    *timeout_kind = 0; *cpu_ms = -1; *wall_ms = 0;
    if (error) return error;
    if (heartbeat_s < 0) heartbeat_s = heartbeat_seconds();
    DWORD code = 0;
    for (;;) {
        error = fx_win32_job_tree_poll(item->job, &item->tracked);
        if (error) break;
        DWORD wait = WaitForSingleObject(item->process, 20);
        *wall_ms = (long long)(GetTickCount64() - began);
        error = job_accounting(item->job, NULL, cpu_ms);
        if (error) break;
        if (wait == WAIT_OBJECT_0) {
            if (!GetExitCodeProcess(item->process, &code)) error = windows_error(GetLastError());
            break;
        }
        if (wait == WAIT_FAILED) { error = windows_error(GetLastError()); break; }
        if (cpu_budget_s > 0 && *cpu_ms >= (long long)cpu_budget_s * 1000) {
            *timeout_kind = 1; code = 124; break;
        }
        if (wall_budget_s > 0 && *wall_ms >= (long long)wall_budget_s * 1000) {
            *timeout_kind = 2; code = 124; break;
        }
        if (heartbeat_s > 0 && GetTickCount64() - heartbeat >= (ULONGLONG)heartbeat_s * 1000) {
            emit_windows_heartbeat(log); heartbeat = GetTickCount64();
        }
    }
    int drainage = drain_job(item, *timeout_kind == 0);
    if (!error) error = drainage;
    int accounting = job_accounting(item->job, NULL, cpu_ms);
    if (!error) error = accounting;
    *wall_ms = (long long)(GetTickCount64() - began);
    forget_windows_process(item);
    return error ? error : (int)code;
}

void fo_c_run_argv_budget(const char *cwd, const char *args, int args_len, int n_args,
    const char *log, int append, int cpu_budget_s, int timeout_s, int heartbeat_s,
    const char *environment, int *exitcode, int *timeout_kind,
    long long *cpu_ms, long long *wall_ms) {
    *exitcode = run_windows(cwd, args, args_len, n_args, log, append, cpu_budget_s,
        timeout_s, heartbeat_s, environment, timeout_kind, cpu_ms, wall_ms);
}
void fo_c_run_argv_logged(const char *cwd, const char *args, int args_len, int n_args,
    const char *log, int append, int timeout_s, int heartbeat_s, const char *environment,
    int *exitcode) {
    int kind;
    long long cpu, wall;
    *exitcode = run_windows(cwd, args, args_len, n_args, log, append, 0, timeout_s,
        heartbeat_s, environment, &kind, &cpu, &wall);
}
void fo_c_run_logged(const char *cwd, const char *image, const char *log, int append,
    int timeout_s, const char *environment, int *exitcode) {
    if (!image || !*image || strlen(image) >= INT_MAX) { *exitcode = EINVAL; return; }
    fo_c_run_argv_logged(cwd, image, (int)strlen(image) + 1, 1, log, append,
        timeout_s, -1, environment, exitcode);
}
void fo_c_start_argv_logged(const char *cwd, const char *args, int args_len, int n_args,
    const char *log, const char *environment, int *pid, int *exitcode) {
    struct fo_windows_process *item;
    *pid = 0;
    *exitcode = spawn_windows(cwd, args, args_len, n_args, log, 0, environment, NULL, NULL, &item);
    if (*exitcode) return;
    lock_processes();
    item->next = windows_processes; windows_processes = item;
    *pid = (int)item->pid;
    unlock_processes();
}
/* Native harness and adapter entry: the same job ownership and registry as
   logged commands, with only the three requested CRT descriptor handles copied. */
int fo_c_start_redirected_async(const char *cwd, char *const argv[],
    char *const overrides[], int input, int output, int diagnostic) {
    if (!argv || !argv[0] || !*argv[0]) return -EINVAL;
    size_t size = 0;
    int count = 0;
    for (; argv[count]; ++count) {
        size_t length = strlen(argv[count]) + 1;
        if (length > INT_MAX || size > INT_MAX - length || count == INT_MAX - 1) return -EOVERFLOW;
        size += length;
    }
    char *packed = malloc(size);
    if (!packed) return -ENOMEM;
    size_t offset = 0;
    for (int index = 0; index < count; ++index) {
        size_t length = strlen(argv[index]) + 1;
        memcpy(packed + offset, argv[index], length); offset += length;
    }
    struct fo_windows_process *item;
    int descriptors[3] = {input, output, diagnostic};
    int error = spawn_windows(cwd, packed, (int)size, count, NULL, 0, NULL,
        overrides, descriptors, &item);
    free(packed);
    if (error) return -error;
    lock_processes();
    item->next = windows_processes; windows_processes = item;
    int pid = (int)item->pid;
    unlock_processes(); return pid;
}
void fo_c_poll_pid(int pid, int *done, int *exitcode) {
    *done = 0; *exitcode = 0;
    lock_processes();
    struct fo_windows_process **slot = &windows_processes;
    while (*slot && (*slot)->pid != (DWORD)pid) slot = &(*slot)->next;
    struct fo_windows_process *item = *slot;
    if (!item) { *done = 1; *exitcode = ESRCH; unlock_processes(); return; }
    *exitcode = verify_windows_process(item);
    if (!*exitcode) *exitcode = fx_win32_job_tree_poll(item->job, &item->tracked);
    if (!*exitcode) {
        DWORD wait = WaitForSingleObject(item->process, 0), code;
        if (wait == WAIT_OBJECT_0) {
            if (!GetExitCodeProcess(item->process, &code)) *exitcode = windows_error(GetLastError());
            else {
                *exitcode = drain_job(item, 1);
                if (!*exitcode) {
                    *done = 1; *exitcode = (int)code;
                    *slot = item->next; forget_windows_process(item);
                }
            }
        } else if (wait == WAIT_FAILED) *exitcode = windows_error(GetLastError());
    }
    unlock_processes();
}
void fo_c_cancel_pid(int pid, int *exitcode) {
    lock_processes();
    struct fo_windows_process **slot = &windows_processes;
    while (*slot && (*slot)->pid != (DWORD)pid) slot = &(*slot)->next;
    struct fo_windows_process *item = *slot;
    if (!item) { *exitcode = ESRCH; unlock_processes(); return; }
    *exitcode = verify_windows_process(item);
    if (!*exitcode) *exitcode = drain_job(item, 1);
    if (!*exitcode) { *slot = item->next; forget_windows_process(item); }
    unlock_processes();
}
void fo_c_suppress_heartbeats(int suppress) { heartbeats_suppressed = suppress != 0; }
void fo_c_detect_nproc(int *nproc) {
    DWORD count = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
    *nproc = count && count <= INT_MAX ? (int)count : 1;
}
void fo_c_setenv_default(const char *name, const char *value) {
    wchar_t *wide_name = fx_win32_utf16(name), *wide_value = fx_win32_utf16(value);
    if (wide_name && wide_value) {
        SetLastError(ERROR_SUCCESS);
        DWORD length = GetEnvironmentVariableW(wide_name, NULL, 0);
        if (!length && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
            if (SetEnvironmentVariableW(wide_name, wide_value)) _putenv_s(name, value);
        }
    }
    free(wide_name); free(wide_value);
}
void fo_c_configure_openmp(void) { fo_c_setenv_default("OMP_WAIT_POLICY", "PASSIVE"); }
int fo_c_self_executable(char *out, int capacity) {
    wchar_t *image = calloc(32768, sizeof(*image));
    if (!image) return -1;
    DWORD length = GetModuleFileNameW(NULL, image, 32768);
    int result = !length || length >= 32768 ? EOVERFLOW : utf8_copy(image, out, capacity);
    free(image); return result ? -1 : 0;
}
void fo_c_exit(int code) { fflush(NULL); exit(code); }
void fo_c_getpid(int *pid) { *pid = (int)GetCurrentProcessId(); }
void fo_c_getcwd(char *path, int capacity, int *exitcode) {
    DWORD length = GetCurrentDirectoryW(0, NULL);
    wchar_t *directory = length ? calloc(length, sizeof(*directory)) : NULL;
    *exitcode = directory ? 0 : (length ? ENOMEM : windows_error(GetLastError()));
    if (!*exitcode && !GetCurrentDirectoryW(length, directory))
        *exitcode = windows_error(GetLastError());
    if (!*exitcode) *exitcode = utf8_copy(directory, path, capacity);
    if (*exitcode && path && capacity > 0) path[0] = 0;
    free(directory);
}
void fo_c_start_fo_check(const char *project, const char *mode, const char *output,
    int *pid, int *exitcode) {
    char image[32768], packed[32832];
    *pid = 0;
    if (!project || !*project || !output || !*output || fo_c_self_executable(image, sizeof(image))) {
        *exitcode = EINVAL; return;
    }
    const char *detail = mode && !strcmp(mode, "full") ? "--json=full" : "--json=compact";
    size_t first = strlen(image) + 1, last = strlen(detail) + 1;
    memcpy(packed, image, first);
    memcpy(packed + first, "check", 6);
    memcpy(packed + first + 6, detail, last);
    fo_c_start_argv_logged(project, packed, (int)(first + 6 + last), 3,
        output, NULL, pid, exitcode);
}
int fo_c_isatty(int fd) { return _isatty(fd) ? 1 : 0; }
void fo_c_write_stderr(const char *buffer, int count) {
    if (!buffer || count <= 0) return;
    while (count > 0) {
        int written = _write(2, buffer, (unsigned)count);
        if (written <= 0) return;
        count -= written; buffer += written;
    }
}

static int scope_name(const char *directory, int owner, const char *birth,
    wchar_t *name, size_t capacity) {
    if (!directory || !*directory || owner <= 0 || !birth || !*birth) return EINVAL;
    char *end;
    errno = 0;
    unsigned long long parsed = strtoull(birth, &end, 10);
    if (errno || *end || !parsed || birth[0] == '-' || birth[0] == '+') return EINVAL;
    wchar_t *wide = fx_win32_utf16(directory);
    if (!wide) return errno;
    HANDLE handle = CreateFileW(wide, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return windows_error(GetLastError());
    BY_HANDLE_FILE_INFORMATION identity;
    BOOL success = GetFileInformationByHandle(handle, &identity);
    DWORD failure = GetLastError(); CloseHandle(handle);
    if (!success) return windows_error(failure);
    if (!(identity.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) return ENOTDIR;
    uint64_t file = ((uint64_t)identity.nFileIndexHigh << 32) | identity.nFileIndexLow;
    int used = swprintf(name, capacity, L"Local\\FoProcessScope-%d-%llu-%08lx-%016llx", owner, parsed,
        (unsigned long)identity.dwVolumeSerialNumber, (unsigned long long)file);
    return used < 0 || (size_t)used >= capacity ? EOVERFLOW : 0;
}

static wchar_t *scope_receipt_path(const char *directory, const wchar_t *name) {
    wchar_t *wide = fx_win32_utf16(directory);
    if (!wide) return NULL;
    size_t length = wcslen(wide) + wcslen(name) + 16;
    wchar_t *path = calloc(length, sizeof(*path));
    if (path) swprintf(path, length, L"%ls/%ls.guard", wide, name + 6);
    free(wide); return path;
}
static int write_scope_receipt(const wchar_t *path, const char *bytes) {
    size_t size = wcslen(path) + 64;
    wchar_t *temporary = calloc(size, sizeof(*temporary));
    if (!temporary) return ENOMEM;
    swprintf(temporary, size, L"%ls.tmp-%lu", path, (unsigned long)GetCurrentProcessId());
    HANDLE file = CreateFileW(temporary, GENERIC_WRITE, 0, NULL, CREATE_NEW,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_WRITE_THROUGH, NULL);
    if (file == INVALID_HANDLE_VALUE) { free(temporary); return windows_error(GetLastError()); }
    DWORD written = 0, length = (DWORD)strlen(bytes);
    int error = 0;
    if (!WriteFile(file, bytes, length, &written, NULL) || written != length || !FlushFileBuffers(file))
        error = windows_error(GetLastError());
    CloseHandle(file);
    if (!error && !MoveFileExW(temporary, path, MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH))
        error = windows_error(GetLastError());
    if (error) DeleteFileW(temporary);
    free(temporary); return error;
}
static int read_scope_receipt(const wchar_t *path) {
    HANDLE file = CreateFileW(path, GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE) return GetLastError() == ERROR_FILE_NOT_FOUND ? 0 : -1;
    char bytes[32] = {0}; DWORD read = 0;
    BOOL success = ReadFile(file, bytes, sizeof(bytes), &read, NULL); CloseHandle(file);
    if (!success) return -1;
    if (read == 6 && !memcmp(bytes, "armed\n", 6)) return 1;
    if (read == 8 && !memcmp(bytes, "drained\n", 8)) return 2;
    return -1;
}
static int native_scope_guardian(HANDLE owner, uint64_t birth, HANDLE job,
    HANDLE ready, HANDLE stop, const wchar_t *receipt) {
    if (!owner || !job || !ready || !stop || !birth || handle_birth(owner) != birth ||
        GetProcessId(owner) == GetCurrentProcessId()) return EINVAL;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    if (!QueryInformationJobObject(job, JobObjectExtendedLimitInformation,
        &limits, sizeof(limits), NULL) ||
        !(limits.BasicLimitInformation.LimitFlags & JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE)) return EINVAL;
    void *tree = NULL;
    int error = write_scope_receipt(receipt, "armed\n");
    if (!error && !SetEvent(ready)) error = windows_error(GetLastError());
    CloseHandle(ready);
    HANDLE waitables[2] = {owner, stop};
    while (!error) {
        error = fx_win32_job_tree_poll(job, &tree);
        if (error) break;
        DWORD wait = WaitForMultipleObjects(2, waitables, FALSE, 25);
        if (wait == WAIT_OBJECT_0 || wait == WAIT_OBJECT_0 + 1) break;
        if (wait != WAIT_TIMEOUT) { error = windows_error(GetLastError()); break; }
    }
    if (!error) {
        error = fx_win32_job_tree_drain(job, &tree, 10000);
    }
    if (!error) error = write_scope_receipt(receipt, "drained\n");
    fx_win32_job_tree_free(&tree);
    CloseHandle(owner); CloseHandle(stop); CloseHandle(job);
    return error;
}
static uint64_t guardian_argument(const wchar_t *text) {
    if (!text || !*text || text[0] == L'-' || text[0] == L'+') return 0;
    wchar_t *end;
    uint64_t value = wcstoull(text, &end, 10);
    return *end ? 0 : value;
}
/* Windows constructors receive no argc/argv. Parse the wide command line only
   for this private, inherited-handle protocol before entering the Fortran app. */
static void __attribute__((constructor)) fo_windows_guardian_entry(void) {
    const wchar_t *raw = GetCommandLineW();
    if (!raw || !wcsstr(raw, L"fo-internal-scope-guardian-v1")) return;
    HMODULE shell = LoadLibraryW(L"shell32.dll");
    if (!shell) return;
    typedef wchar_t **(WINAPI *parse_command)(LPCWSTR, int *);
    union { FARPROC address; parse_command parse; } converter;
    converter.address = GetProcAddress(shell, "CommandLineToArgvW");
    int count = 0;
    wchar_t **arguments = converter.address ? converter.parse(raw, &count) : NULL;
    if (arguments && count == 8 && !wcscmp(arguments[1], L"fo-internal-scope-guardian-v1")) {
        int result = native_scope_guardian((HANDLE)(uintptr_t)guardian_argument(arguments[2]),
            guardian_argument(arguments[3]), (HANDLE)(uintptr_t)guardian_argument(arguments[4]),
            (HANDLE)(uintptr_t)guardian_argument(arguments[5]),
            (HANDLE)(uintptr_t)guardian_argument(arguments[6]), arguments[7]);
        LocalFree(arguments); FreeLibrary(shell); ExitProcess((UINT)result);
    }
    if (arguments) LocalFree(arguments);
    FreeLibrary(shell);
}
static int stop_scope_guardian(HANDLE guardian, HANDLE stop) {
    if (!guardian || !stop) return EIO;
    if (!SetEvent(stop)) return windows_error(GetLastError());
    DWORD wait = WaitForSingleObject(guardian, 15000), code;
    if (wait != WAIT_OBJECT_0) return wait == WAIT_TIMEOUT ? ETIMEDOUT : windows_error(GetLastError());
    if (!GetExitCodeProcess(guardian, &code)) return windows_error(GetLastError());
    return (int)code;
}
static int start_scope_guardian(const char *directory, const wchar_t *name, HANDLE job,
    HANDLE *guardian, HANDLE *stop) {
    char image[32768], packed[65536], numbers[5][64];
    wchar_t *receipt = scope_receipt_path(directory, name), *application = NULL, *command = NULL;
    HANDLE inherited[4] = {0};
    STARTUPINFOEXW startup = {0}; PROCESS_INFORMATION child = {0};
    SIZE_T attribute_size = 0;
    int error = 0, initialized = 0;
    extern int fo_c_self_executable(char *, int);
    if (!receipt || fo_c_self_executable(image, sizeof(image))) { error = EIO; goto finished; }
    inherited[0] = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, TRUE, GetCurrentProcessId());
    if (!DuplicateHandle(GetCurrentProcess(), job, GetCurrentProcess(), &inherited[1],
        0, TRUE, DUPLICATE_SAME_ACCESS)) { error = windows_error(GetLastError()); goto finished; }
    SECURITY_ATTRIBUTES security = {sizeof(security), NULL, TRUE};
    inherited[2] = CreateEventW(&security, TRUE, FALSE, NULL);
    inherited[3] = CreateEventW(&security, TRUE, FALSE, NULL);
    for (int i = 0; i < 4; ++i) if (!inherited[i]) { error = windows_error(GetLastError()); goto finished; }
    snprintf(numbers[0], sizeof(numbers[0]), "%llu", (unsigned long long)(uintptr_t)inherited[0]);
    snprintf(numbers[1], sizeof(numbers[1]), "%llu", (unsigned long long)handle_birth(inherited[0]));
    for (int i = 1; i < 4; ++i)
        snprintf(numbers[i + 1], sizeof(numbers[i + 1]), "%llu", (unsigned long long)(uintptr_t)inherited[i]);
    int receipt_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, receipt, -1, NULL, 0, NULL, NULL);
    char *receipt_utf8 = receipt_size ? malloc((size_t)receipt_size) : NULL;
    if (!receipt_utf8) { error = ENOMEM; goto finished; }
    WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, receipt, -1, receipt_utf8, receipt_size, NULL, NULL);
    const char *arguments[8] = {image, "fo-internal-scope-guardian-v1", numbers[0], numbers[1],
        numbers[2], numbers[3], numbers[4], receipt_utf8};
    size_t used = 0;
    for (int i = 0; i < 8; ++i) {
        size_t length = strlen(arguments[i]) + 1;
        if (used + length > sizeof(packed)) { error = E2BIG; break; }
        memcpy(packed + used, arguments[i], length); used += length;
    }
    free(receipt_utf8);
    if (error) goto finished;
    command = fx_win32_command_line(packed, (int)used, 8);
    application = fx_win32_utf16(image);
    if (!command || !application) { error = errno; goto finished; }
    startup.StartupInfo.cb = sizeof(startup);
    InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_size);
    startup.lpAttributeList = malloc(attribute_size);
    if (!startup.lpAttributeList) { error = ENOMEM; goto finished; }
    if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 1, 0, &attribute_size)) {
        error = windows_error(GetLastError()); goto finished;
    }
    initialized = 1;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
        inherited, sizeof(inherited), NULL, NULL) || !CreateProcessW(application, command, NULL, NULL,
        TRUE, EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW, NULL, NULL, &startup.StartupInfo, &child)) {
        error = windows_error(GetLastError()); goto finished;
    }
    CloseHandle(child.hThread); child.hThread = NULL;
    HANDLE readiness[2] = {inherited[2], child.hProcess};
    DWORD wait = WaitForMultipleObjects(2, readiness, FALSE, 10000);
    if (wait != WAIT_OBJECT_0 || read_scope_receipt(receipt) != 1) {
        error = EIO;
        SetEvent(inherited[3]);
        if (WaitForSingleObject(child.hProcess, 15000) != WAIT_OBJECT_0) {
            TerminateProcess(child.hProcess, 125);
            WaitForSingleObject(child.hProcess, 10000);
        }
        goto finished;
    }
    *guardian = child.hProcess; child.hProcess = NULL;
    *stop = inherited[3]; inherited[3] = NULL;
    SetHandleInformation(*stop, HANDLE_FLAG_INHERIT, 0);
finished:
    if (initialized) DeleteProcThreadAttributeList(startup.lpAttributeList);
    free(startup.lpAttributeList);
    for (int i = 0; i < 4; ++i) if (inherited[i]) CloseHandle(inherited[i]);
    if (child.hThread) CloseHandle(child.hThread);
    if (child.hProcess) CloseHandle(child.hProcess);
    free(receipt); free(application); free(command);
    return error;
}
static int set_scope_environment(const char *directory, int owner, const char *birth) {
    char pid[32];
    snprintf(pid, sizeof(pid), "%d", owner);
    if (_putenv_s("FO_GREMLIN_PROCESS_SCOPE_DIR", directory) ||
        _putenv_s("FO_GREMLIN_PROCESS_SCOPE_PID", pid) ||
        _putenv_s("FO_GREMLIN_PROCESS_SCOPE_START", birth)) return EIO;
    return 0;
}
int fo_c_process_set_async_scope(const char *directory, int owner, const char *birth) {
    if (owner != (int)GetCurrentProcessId()) return EINVAL;
    if (!fo_gremlin_process_matches(owner, birth)) return ESTALE;
    wchar_t name[128];
    int error = scope_name(directory, owner, birth, name, 128);
    if (error) return error;
    wchar_t *wide_directory = fx_win32_utf16(directory);
    if (!wide_directory) return errno;
    DWORD attributes = GetFileAttributesW(wide_directory); free(wide_directory);
    if (attributes == INVALID_FILE_ATTRIBUTES || !(attributes & FILE_ATTRIBUTE_DIRECTORY))
        return ENOENT;
    char *copy = _strdup(directory);
    if (!copy) return ENOMEM;
    lock_processes();
    wchar_t current_name[128];
    if (scope_job && scope_directory && !scope_name(scope_directory, owner, birth, current_name, 128) &&
        !wcscmp(current_name, name)) {
        error = set_scope_environment(directory, owner, birth);
        unlock_processes(); free(copy); return error;
    }
    HANDLE job = make_job(name, &error);
    if (job) {
        HANDLE guardian = NULL, stop = NULL;
        error = start_scope_guardian(directory, name, job, &guardian, &stop);
        if (!error && scope_guardian) error = stop_scope_guardian(scope_guardian, scope_stop);
        if (!error) error = set_scope_environment(directory, owner, birth);
        if (error) {
            if (guardian) {
                (void)stop_scope_guardian(guardian, stop);
                CloseHandle(guardian); CloseHandle(stop);
            }
            CloseHandle(job); job = NULL;
        }
        else {
            if (scope_job) CloseHandle(scope_job);
            if (scope_guardian) CloseHandle(scope_guardian);
            if (scope_stop) CloseHandle(scope_stop);
            free(scope_directory); scope_directory = copy; scope_job = job;
            scope_guardian = guardian; scope_stop = stop;
        }
    }
    unlock_processes();
    if (!job) free(copy);
    return error;
}
struct saved_windows_scope {
    HANDLE job, guardian, stop;
    char *directory, *environment[3];
    int shared;
};
static const char *scope_keys[3] = {"FO_GREMLIN_PROCESS_SCOPE_DIR",
    "FO_GREMLIN_PROCESS_SCOPE_PID", "FO_GREMLIN_PROCESS_SCOPE_START"};
int fo_c_process_push_async_scope(const char *directory, void **token) {
    if (!token || *token) return EBUSY;
    struct saved_windows_scope *saved = calloc(1, sizeof(*saved));
    if (!saved) return ENOMEM;
    for (int i = 0; i < 3; ++i) {
        const char *value = getenv(scope_keys[i]);
        if (value && !(saved->environment[i] = _strdup(value))) {
            for (int j = 0; j < i; ++j) free(saved->environment[j]);
            free(saved); return ENOMEM;
        }
    }
    char birth[64];
    snprintf(birth, sizeof(birth), "%llu", (unsigned long long)handle_birth(GetCurrentProcess()));
    lock_processes();
    saved->job = scope_job; saved->directory = scope_directory;
    saved->guardian = scope_guardian; saved->stop = scope_stop;
    scope_job = NULL; scope_directory = NULL; scope_guardian = NULL; scope_stop = NULL;
    int error = 0;
    wchar_t saved_name[128], next_name[128];
    if (saved->job && saved->directory &&
        !scope_name(directory, (int)GetCurrentProcessId(), birth, next_name, 128) &&
        !scope_name(saved->directory, (int)GetCurrentProcessId(), birth, saved_name, 128) &&
        !wcscmp(next_name, saved_name)) {
        saved->shared = 1;
        if (!DuplicateHandle(GetCurrentProcess(), saved->job, GetCurrentProcess(), &scope_job,
            0, FALSE, DUPLICATE_SAME_ACCESS)) error = windows_error(GetLastError());
        if (!error && !DuplicateHandle(GetCurrentProcess(), saved->guardian, GetCurrentProcess(),
            &scope_guardian, 0, FALSE, DUPLICATE_SAME_ACCESS)) error = windows_error(GetLastError());
        if (!error && !DuplicateHandle(GetCurrentProcess(), saved->stop, GetCurrentProcess(),
            &scope_stop, 0, FALSE, DUPLICATE_SAME_ACCESS)) error = windows_error(GetLastError());
        if (!error && !(scope_directory = _strdup(directory))) {
            CloseHandle(scope_job); scope_job = NULL; error = ENOMEM;
        }
        if (!error) error = set_scope_environment(directory, (int)GetCurrentProcessId(), birth);
    } else error = fo_c_process_set_async_scope(directory, (int)GetCurrentProcessId(), birth);
    if (error) {
        if (scope_job) CloseHandle(scope_job);
        if (scope_guardian) CloseHandle(scope_guardian);
        if (scope_stop) CloseHandle(scope_stop);
        free(scope_directory);
        scope_job = saved->job; scope_directory = saved->directory;
        scope_guardian = saved->guardian; scope_stop = saved->stop;
        for (int i = 0; i < 3; ++i) {
            _putenv_s(scope_keys[i], saved->environment[i] ? saved->environment[i] : "");
            free(saved->environment[i]);
        }
        free(saved);
    }
    else *token = saved;
    unlock_processes();
    return error;
}
int fo_c_process_pop_async_scope(void **token) {
    if (!token || !*token) return 0;
    struct saved_windows_scope *saved = *token;
    lock_processes();
    if (!saved->shared && scope_job) {
        int drainage = stop_scope_guardian(scope_guardian, scope_stop);
        if (drainage) { unlock_processes(); return drainage; }
    }
    if (scope_job) CloseHandle(scope_job);
    if (scope_guardian) CloseHandle(scope_guardian);
    if (scope_stop) CloseHandle(scope_stop);
    free(scope_directory);
    scope_job = saved->job; scope_directory = saved->directory;
    scope_guardian = saved->guardian; scope_stop = saved->stop;
    int error = 0;
    for (int i = 0; i < 3; ++i) {
        if (_putenv_s(scope_keys[i], saved->environment[i] ? saved->environment[i] : "")) error = EIO;
        free(saved->environment[i]);
    }
    unlock_processes();
    free(saved); *token = NULL; return error;
}
int fo_c_process_owned_by_scope(const char *directory, int owner, const char *birth) {
    wchar_t name[128];
    if (owner == (int)GetCurrentProcessId() ||
        scope_name(directory, owner, birth, name, 128) ||
        !fo_gremlin_process_matches(owner, birth)) return 0;
    HANDLE job = OpenJobObjectW(JOB_OBJECT_QUERY, FALSE, name);
    if (!job) return 0;
    BOOL owned = FALSE;
    int result = IsProcessInJob(GetCurrentProcess(), job, &owned) && owned;
    CloseHandle(job); return result;
}
int fo_c_recover_async_scope(const char *directory, int owner, const char *birth) {
    wchar_t name[128];
    int error = scope_name(directory, owner, birth, name, 128);
    if (error) return error;
    if (fo_gremlin_process_matches(owner, birth)) return EBUSY;
    wchar_t *receipt = scope_receipt_path(directory, name);
    if (!receipt) return ENOMEM;
    int authority = read_scope_receipt(receipt);
    HANDLE job = OpenJobObjectW(JOB_OBJECT_QUERY | JOB_OBJECT_TERMINATE, FALSE, name);
    if (!job) {
        DWORD missing = GetLastError();
        free(receipt);
        if (missing != ERROR_FILE_NOT_FOUND) return windows_error(missing);
        /* An armed guardian that disappeared without its durable drainage receipt
           leaves ownership unresolved. Never turn a missing job into false success. */
        return authority == 0 || authority == 2 ? 0 : EIO;
    }
    /* Recheck the exact birth before any scoped kill; a live owner wins the race. */
    if (fo_gremlin_process_matches(owner, birth)) { CloseHandle(job); free(receipt); return EBUSY; }
    ULONGLONG deadline = GetTickCount64() + 15000;
    while (authority == 1 && GetTickCount64() < deadline) {
        Sleep(20); authority = read_scope_receipt(receipt);
    }
    error = authority == 2 ? 0 : EIO;
    DWORD active = 0;
    if (!error) error = job_accounting(job, &active, NULL);
    if (!error && active) error = EIO;
    CloseHandle(job); free(receipt); return error;
}
int fo_c_reap_adopted_scope_zombies(void) {
    /* Windows process objects do not become Unix zombies; owned handles are
       closed by poll/cancel, and last job-handle close drains crash descendants. */
    return 0;
}

int fo_c_process_containment_required(void) {
    /* Native Fx capture owns an atomic kill-on-close job and waits exact member
       handles on return. Windows does not use the Unix fd/exec monitor ABI. */
    return 0;
}
int fo_c_start_capture_monitor(const char *monitor, const char *cwd, char *const argv[],
    char *const overrides[], int input, int output, int diagnostic) {
    (void)monitor; (void)cwd; (void)argv; (void)overrides;
    (void)input; (void)output; (void)diagnostic;
    return -ENOTSUP;
}

int fo_c_has_fortran_source_ext(const char *path) {
    if (!path) return 0;
    size_t length = strlen(path);
    return (length >= 4 && (!strcmp(path + length - 4, ".f90") ||
        !strcmp(path + length - 4, ".F90"))) || (length >= 2 &&
        (!strcmp(path + length - 2, ".f") || !strcmp(path + length - 2, ".F")));
}
struct windows_source_list { char **paths; size_t count, capacity; };
struct windows_directory_id { DWORD volume; uint64_t file; };
struct windows_scan {
    struct windows_source_list files;
    struct windows_directory_id *directories;
    size_t count, capacity;
    int project;
};
static int windows_source_add(struct windows_source_list *list, const char *path) {
    if (list->count == list->capacity) {
        size_t capacity = list->capacity ? list->capacity * 2 : 64;
        char **grown = realloc(list->paths, capacity * sizeof(*grown));
        if (!grown) return ENOMEM;
        list->paths = grown; list->capacity = capacity;
    }
    list->paths[list->count] = _strdup(path);
    if (!list->paths[list->count]) return ENOMEM;
    ++list->count; return 0;
}
static int directory_seen(struct windows_scan *scan, const wchar_t *path) {
    HANDLE directory = CreateFileW(path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (directory == INVALID_HANDLE_VALUE) return -1;
    BY_HANDLE_FILE_INFORMATION information;
    BOOL success = GetFileInformationByHandle(directory, &information);
    CloseHandle(directory);
    if (!success) return -1;
    uint64_t id = ((uint64_t)information.nFileIndexHigh << 32) | information.nFileIndexLow;
    for (size_t i = 0; i < scan->count; ++i)
        if (scan->directories[i].volume == information.dwVolumeSerialNumber &&
            scan->directories[i].file == id) return 1;
    if (scan->count == scan->capacity) {
        size_t capacity = scan->capacity ? scan->capacity * 2 : 64;
        struct windows_directory_id *grown = realloc(scan->directories, capacity * sizeof(*grown));
        if (!grown) return -1;
        scan->directories = grown; scan->capacity = capacity;
    }
    scan->directories[scan->count++] = (struct windows_directory_id){information.dwVolumeSerialNumber, id};
    return 0;
}
static int skip_windows_directory(const wchar_t *name, int project, int depth) {
    if (name[0] == L'.') return 1;
    static const wchar_t *excluded[] = {L"__pycache__", L"site-packages", L"_deps",
        L"dependencies", L"deps-src", NULL};
    for (int i = 0; excluded[i]; ++i) if (!wcscmp(name, excluded[i])) return 1;
    return project && depth == 0 && (!wcsncmp(name, L"build", 5) || !wcscmp(name, L"SRC"));
}
static wchar_t *join_windows_path(const wchar_t *base, const wchar_t *name) {
    size_t length = wcslen(base) + wcslen(name) + 2;
    wchar_t *path = calloc(length, sizeof(*path));
    if (path) swprintf(path, length, L"%ls/%ls", base, name);
    return path;
}
static int windows_project_root(const wchar_t *path) {
    wchar_t *manifest = join_windows_path(path, L"fpm.toml");
    if (!manifest) return 0;
    DWORD attributes = GetFileAttributesW(manifest); free(manifest);
    return attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_DIRECTORY);
}
static int scan_windows_sources(struct windows_scan *scan, const wchar_t *path, int depth) {
    int seen = directory_seen(scan, path);
    if (seen) return seen > 0 ? 0 : EIO;
    wchar_t *pattern = join_windows_path(path, L"*");
    if (!pattern) return ENOMEM;
    WIN32_FIND_DATAW entry;
    HANDLE search = FindFirstFileW(pattern, &entry); free(pattern);
    if (search == INVALID_HANDLE_VALUE) return windows_error(GetLastError());
    int error = 0;
    do {
        if (skip_windows_directory(entry.cFileName, scan->project, depth)) continue;
        wchar_t *child = join_windows_path(path, entry.cFileName);
        if (!child) { error = ENOMEM; break; }
        if (entry.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            if (!windows_project_root(child)) error = scan_windows_sources(scan, child, depth + 1);
        } else {
            char encoded[16384];
            error = utf8_copy(child, encoded, sizeof(encoded));
            if (!error && fo_c_has_fortran_source_ext(encoded)) error = windows_source_add(&scan->files, encoded);
        }
        free(child);
        if (error) break;
    } while (FindNextFileW(search, &entry));
    DWORD final = GetLastError(); FindClose(search);
    if (!error && final != ERROR_NO_MORE_FILES) error = windows_error(final);
    return error;
}
static int windows_path_compare(const void *left, const void *right) {
    return strcmp(*(char *const *)left, *(char *const *)right);
}
void fo_c_scan_sources(const char *root, const char *output_file, int *exitcode) {
    struct windows_scan scan = {0};
    wchar_t *path = fx_win32_utf16(root), *output = fx_win32_utf16(output_file);
    *exitcode = path && output ? 0 : EINVAL;
    if (!*exitcode) {
        scan.project = windows_project_root(path);
        *exitcode = scan_windows_sources(&scan, path, 0);
    }
    if (!*exitcode) {
        qsort(scan.files.paths, scan.files.count, sizeof(char *), windows_path_compare);
        FILE *stream = _wfopen(output, L"wb");
        if (!stream) *exitcode = errno;
        else {
            for (size_t i = 0; i < scan.files.count; ++i)
                if (fprintf(stream, "%s\n", scan.files.paths[i]) < 0) *exitcode = EIO;
            if (fclose(stream)) *exitcode = EIO;
        }
    }
    for (size_t i = 0; i < scan.files.count; ++i) free(scan.files.paths[i]);
    free(scan.files.paths); free(scan.directories); free(path); free(output);
}
