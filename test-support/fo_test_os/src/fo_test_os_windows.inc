/* Native test OS boundaries. Child ownership stays in Fo's process engine. */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
extern wchar_t *fx_win32_utf16(const char *);
extern int fo_c_start_redirected_async(const char *, char *const *, char *const *, int, int, int);
extern void fo_c_poll_pid(int, int *, int *);
extern void fo_c_cancel_pid(int, int *);
extern int fo_c_mkdir_p(const char *);
extern int fo_c_rm_rf(const char *);
int fo_test_host_is_linux(void) { return 0; }
int fo_test_host_is_windows(void) { return 1; }
int fo_test_os_initialize(void) {
    if (_setmode(0, _O_BINARY) < 0 || _setmode(1, _O_BINARY) < 0 || _setmode(2, _O_BINARY) < 0) return -1;
    return 162;
}
int64_t fo_test_monotonic_ms(void) { return (int64_t)GetTickCount64(); }
int fo_test_sleep_ms(int ms) { if (ms < 0) return -1; Sleep((DWORD)ms); return 0; }
int fo_test_ignore_sigpipe(void) { return 0; } /* Win32 has no SIGPIPE. */

/* Only parent endpoints are overlapped. Children receive synchronous handles. */
typedef struct pipe_state {
    int fd, writes, pending;
    HANDLE handle;
    OVERLAPPED operation;
    struct pipe_state *next;
} pipe_state;
static pipe_state *pipes;
static LONG pipe_serial;
static pipe_state *pipe_for(int fd) {
    for (pipe_state *p = pipes; p; p = p->next) if (p->fd == fd) return p;
    return NULL;
}
int fo_test_pipe_mode(int descriptors[2], int parent_reads) {
    wchar_t name[128];
    swprintf(name, 128, L"\\\\.\\pipe\\fo-test-%lu-%lld-%ld", GetCurrentProcessId(),
        (long long)GetTickCount64(), InterlockedIncrement(&pipe_serial));
    HANDLE parent = CreateNamedPipeW(name,
        (parent_reads ? PIPE_ACCESS_INBOUND : PIPE_ACCESS_OUTBOUND) | FILE_FLAG_OVERLAPPED,
        PIPE_TYPE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 65536, 65536, 0, NULL);
    if (parent == INVALID_HANDLE_VALUE) return -1;
    HANDLE child = CreateFileW(name, parent_reads ? GENERIC_WRITE : GENERIC_READ,
        0, NULL, OPEN_EXISTING, 0, NULL);
    if (child == INVALID_HANDLE_VALUE) { CloseHandle(parent); return -1; }
    /* A client already connected; ERROR_PIPE_CONNECTED is the expected result. */
    OVERLAPPED connection = {0};
    connection.hEvent = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!connection.hEvent) { CloseHandle(parent); CloseHandle(child); return -1; }
    BOOL connected = ConnectNamedPipe(parent, &connection);
    DWORD error = connected ? ERROR_SUCCESS : GetLastError();
    if (error == ERROR_IO_PENDING) {
        DWORD transferred;
        connected = GetOverlappedResult(parent, &connection, &transferred, TRUE);
    } else connected = error == ERROR_PIPE_CONNECTED || error == ERROR_SUCCESS;
    CloseHandle(connection.hEvent);
    if (!connected) { CloseHandle(parent); CloseHandle(child); return -1; }
    int parent_fd = _open_osfhandle((intptr_t)parent,
        _O_BINARY | _O_NOINHERIT | (parent_reads ? _O_RDONLY : _O_WRONLY));
    int child_fd = _open_osfhandle((intptr_t)child,
        _O_BINARY | _O_NOINHERIT | (parent_reads ? _O_WRONLY : _O_RDONLY));
    if (parent_fd < 0 || child_fd < 0) {
        if (parent_fd >= 0) _close(parent_fd); else CloseHandle(parent);
        if (child_fd >= 0) _close(child_fd); else CloseHandle(child);
        return -1;
    }
    pipe_state *p = calloc(1, sizeof(*p));
    if (p) p->operation.hEvent = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!p || !p->operation.hEvent) {
        free(p); _close(parent_fd); _close(child_fd); return -1;
    }
    p->fd = parent_fd; p->writes = !parent_reads; p->handle = parent;
    p->next = pipes; pipes = p;
    descriptors[parent_reads ? 0 : 1] = parent_fd;
    descriptors[parent_reads ? 1 : 0] = child_fd;
    return 0;
}
int fo_test_close(int fd) {
    pipe_state **slot = &pipes;
    while (*slot && (*slot)->fd != fd) slot = &(*slot)->next;
    if (*slot) {
        pipe_state *p = *slot;
        if (p->pending) {
            DWORD count;
            (void)CancelIoEx(p->handle, &p->operation);
            (void)GetOverlappedResult(p->handle, &p->operation, &count, TRUE);
        }
        *slot = p->next; CloseHandle(p->operation.hEvent); free(p);
    }
    return _close(fd);
}
int fo_test_set_nonblocking(int fd) { return pipe_for(fd) ? 0 : -1; }
int64_t fo_test_read(int fd, char *buffer, size_t capacity) {
    pipe_state *p = pipe_for(fd);
    DWORD available = 0, count = 0;
    if (!p || p->writes) return -1;
    if (!PeekNamedPipe(p->handle, NULL, 0, NULL, &available, NULL))
        return GetLastError() == ERROR_BROKEN_PIPE ? 0 : -1;
    if (!available) return -2;
    ResetEvent(p->operation.hEvent);
    DWORD length = (DWORD)(capacity < available ? capacity : available);
    if (!ReadFile(p->handle, buffer, length, &count, &p->operation)) {
        if (GetLastError() != ERROR_IO_PENDING) return -1;
        if (!GetOverlappedResult(p->handle, &p->operation, &count, TRUE)) return -1;
    }
    return (int64_t)count;
}
int64_t fo_test_write(int fd, const char *buffer, size_t length) {
    pipe_state *p = pipe_for(fd);
    DWORD count = 0;
    if (!p || !p->writes || length > MAXDWORD) return -1;
    if (p->pending) {
        if (!GetOverlappedResult(p->handle, &p->operation, &count, FALSE)) {
            DWORD error = GetLastError();
            if (error == ERROR_IO_INCOMPLETE) return -2;
            p->pending = 0;
            return error == ERROR_BROKEN_PIPE || error == ERROR_NO_DATA ? -3 : -1;
        }
        p->pending = 0; return count;
    }
    ResetEvent(p->operation.hEvent);
    if (WriteFile(p->handle, buffer, (DWORD)length, &count, &p->operation)) return count;
    DWORD error = GetLastError();
    if (error == ERROR_IO_PENDING) { p->pending = 1; return -2; }
    return error == ERROR_BROKEN_PIPE || error == ERROR_NO_DATA ? -3 : -1;
}
typedef struct pollfd { int fd; short events, revents; } test_pollfd;
#define POLLIN 1
#define POLLOUT 4
int fo_test_poll(test_pollfd *fds, size_t count, int timeout_ms) {
    uint64_t deadline = GetTickCount64() + (timeout_ms < 0 ? 0 : (unsigned)timeout_ms);
    for (;;) {
        int ready = 0;
        for (size_t i = 0; i < count; ++i) {
            fds[i].revents = 0;
            pipe_state *p = pipe_for(fds[i].fd);
            if (!p) fds[i].revents = 8;
            else if (p->writes && (fds[i].events & 4)) {
                if (!p->pending || WaitForSingleObject(p->operation.hEvent, 0) == WAIT_OBJECT_0)
                    fds[i].revents = 4;
            } else if (!p->writes && (fds[i].events & 17)) {
                DWORD bytes = 0;
                if (!PeekNamedPipe(p->handle, NULL, 0, NULL, &bytes, NULL))
                    fds[i].revents = GetLastError() == ERROR_BROKEN_PIPE ? 16 : 8;
                else if (bytes) fds[i].revents = 1;
            }
            if (fds[i].revents) ++ready;
        }
        if (ready || GetTickCount64() >= deadline) return ready;
        Sleep(1);
    }
}
int fo_test_spawn_redirected(char *const argv[], char *const env[], const char *cwd,
        const int input[2], const int output[2], const int diagnostic[2], int delay_ms) {
    /* A native suspended child is owned before it can execute; no fork window. */
    if (delay_ms != 0) { errno = ENOTSUP; return -1; }
    return fo_c_start_redirected_async(cwd, argv, env, input[0], output[1], diagnostic[1]);
}
int fo_test_wait_child(int pid, int *exit_code, int *term_signal, int blocking) {
    int done = 0;
    do {
        fo_c_poll_pid(pid, &done, exit_code);
        if (!done && *exit_code) return -1;
        if (!done && blocking) Sleep(10);
    } while (!done && blocking);
    *term_signal = 0;
    return done ? 1 : 0;
}
int fo_test_cancel(int pid) { int error = 0; fo_c_cancel_pid(pid, &error); return error ? -1 : 0; }

uint64_t fo_test_process_start_time(int pid) {
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
    FILETIME born, exited, kernel, user;
    uint64_t value = 0;
    if (process && GetProcessTimes(process, &born, &exited, &kernel, &user))
        value = ((uint64_t)born.dwHighDateTime << 32) | born.dwLowDateTime;
    if (process) CloseHandle(process);
    return value;
}
int fo_test_process_running(int pid) {
    HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!process) return 0;
    DWORD waited = WaitForSingleObject(process, 0);
    CloseHandle(process);
    return waited == WAIT_TIMEOUT;
}
int fo_test_signal_identity(int pid, uint64_t birth, int signal_number) {
    if (pid <= 0 || !birth) return -1;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE |
        (signal_number ? PROCESS_TERMINATE : 0), FALSE, (DWORD)pid);
    if (!process) return -1;
    FILETIME born, exited, kernel, user;
    int result = -2;
    if (GetProcessTimes(process, &born, &exited, &kernel, &user) &&
        (((uint64_t)born.dwHighDateTime << 32) | born.dwLowDateTime) == birth)
        result = !signal_number ? 0 : TerminateProcess(process, 137) ? 0 : -1;
    CloseHandle(process);
    return result;
}
int fo_test_collect_descendants(int root, uint64_t birth, int *pids,
        uint64_t *births, int capacity) {
    if (capacity < 1 || !pids || !births) return -1;
    if (fo_test_process_start_time(root) != birth) return -2;
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) return -3;
    typedef struct { DWORD pid, parent; uint64_t birth; } record;
    record *records = NULL;
    size_t count = 0, allocated = 0;
    PROCESSENTRY32W entry = {0}; entry.dwSize = sizeof(entry);
    BOOL more = Process32FirstW(snapshot, &entry);
    while (more) {
        uint64_t value = fo_test_process_start_time((int)entry.th32ProcessID);
        if (value) {
            if (count == allocated) {
                size_t next = allocated ? allocated * 2 : 128;
                record *grown = realloc(records, next * sizeof(*grown));
                if (!grown) { CloseHandle(snapshot); free(records); return -3; }
                records = grown; allocated = next;
            }
            records[count++] = (record){entry.th32ProcessID, entry.th32ParentProcessID, value};
        }
        more = Process32NextW(snapshot, &entry);
    }
    DWORD error = GetLastError(); CloseHandle(snapshot);
    if (error != ERROR_NO_MORE_FILES || fo_test_process_start_time(root) != birth) {
        free(records); return -2;
    }
    pids[0] = root; births[0] = birth;
    int used = 1;
    for (int frontier = 0; frontier < used; ++frontier) {
        for (size_t i = 0; i < count; ++i) {
            if (records[i].parent != (DWORD)pids[frontier] || records[i].birth < births[frontier]) continue;
            int duplicate = 0;
            for (int j = 0; j < used; ++j) if (pids[j] == (int)records[i].pid) duplicate = 1;
            if (duplicate) continue;
            if (used >= capacity) { free(records); return -4; }
            pids[used] = (int)records[i].pid; births[used++] = records[i].birth;
        }
    }
    free(records); return used - 1;
}
int fo_test_mkdirs(const char *path) { return fo_c_mkdir_p(path); }
int fo_test_remove_tree(const char *path) { return fo_c_rm_rf(path); }
int fo_test_unlink(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    if (!wide) return -1;
    BOOL removed = DeleteFileW(wide); DWORD error = GetLastError(); free(wide);
    return removed || error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND ? 0 : -1;
}
int fo_test_rename(const char *from, const char *to) {
    wchar_t *a = fx_win32_utf16(from), *b = fx_win32_utf16(to);
    BOOL moved = a && b && MoveFileExW(a, b, MOVEFILE_REPLACE_EXISTING);
    free(a); free(b); return moved ? 0 : -1;
}
int fo_test_symlink(const char *target, const char *path) {
    wchar_t *a = fx_win32_utf16(target), *b = fx_win32_utf16(path);
    DWORD attributes = a ? GetFileAttributesW(a) : INVALID_FILE_ATTRIBUTES;
    DWORD flags = attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
    BOOL made = a && b && CreateSymbolicLinkW(b, a, flags | 2);
    if (!made && GetLastError() == ERROR_INVALID_PARAMETER && a && b)
        made = CreateSymbolicLinkW(b, a, flags);
    free(a); free(b); return made ? 0 : -1;
}
int fo_test_getcwd(char *buffer, size_t capacity) {
    DWORD needed = GetCurrentDirectoryW(0, NULL);
    wchar_t *wide = needed ? malloc((size_t)needed * sizeof(*wide)) : NULL;
    if (!wide) return -1;
    if (!GetCurrentDirectoryW(needed, wide)) { free(wide); return -1; }
    int bytes = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, NULL, 0, NULL, NULL);
    int result = bytes > 0 && (size_t)bytes <= capacity &&
        WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, buffer, bytes, NULL, NULL) ? 0 : -1;
    free(wide);
    if (!result) for (char *p = buffer; *p; ++p) if (*p == '\\') *p = '/';
    return result;
}
int fo_test_mkdtemp(char *pattern) {
    size_t length = strlen(pattern);
    if (length < 6 || strcmp(pattern + length - 6, "XXXXXX")) return -1;
    for (unsigned attempt = 0; attempt < 256; ++attempt) {
        unsigned value = GetCurrentProcessId() ^ (unsigned)GetTickCount64() ^
            (unsigned)InterlockedIncrement(&pipe_serial);
        snprintf(pattern + length - 6, 7, "%06x", value & 0xffffff);
        wchar_t *wide = fx_win32_utf16(pattern);
        if (!wide) return -1;
        BOOL made = CreateDirectoryW(wide, NULL); DWORD error = GetLastError(); free(wide);
        if (made) return 0;
        if (error != ERROR_ALREADY_EXISTS) return -1;
    }
    return -1;
}
int fo_test_open_fds(void) {
    DWORD count;
    return GetProcessHandleCount(GetCurrentProcess(), &count) ? (int)count : -1;
}

static int open_path(const char *path, int flags) {
    wchar_t *wide = fx_win32_utf16(path);
    int fd = wide ? _wopen(wide, flags | _O_BINARY | _O_NOINHERIT, _S_IREAD | _S_IWRITE) : -1;
    free(wide); return fd;
}
int fo_test_spawn(char *const argv[], char *const env[], const char *cwd) {
    int input = open_path("NUL", _O_RDONLY), output = open_path("NUL", _O_WRONLY);
    int pid = input >= 0 && output >= 0 ?
        fo_c_start_redirected_async(cwd, argv, env, input, output, output) : -1;
    if (input >= 0) _close(input);
    if (output >= 0) _close(output);
    return pid;
}
int fo_test_spawn_capture(char *const argv[], const char *cwd, const char *out, const char *err) {
    int input = open_path("NUL", _O_RDONLY);
    int output = open_path(out, _O_CREAT | _O_TRUNC | _O_WRONLY);
    int diagnostic = open_path(err, _O_CREAT | _O_TRUNC | _O_WRONLY);
    int pid = input >= 0 && output >= 0 && diagnostic >= 0 ?
        fo_c_start_redirected_async(cwd, argv, NULL, input, output, diagnostic) : -1;
    if (input >= 0) _close(input);
    if (output >= 0) _close(output);
    if (diagnostic >= 0) _close(diagnostic);
    return pid;
}
int fo_test_wait_nonblocking(int pid, int *status) {
    int signal_number;
    int done = fo_test_wait_child(pid, status, &signal_number, 0);
    return done < 0 ? -999 : done ? *status : 999;
}
int fo_test_wait_blocking(int pid) {
    int status, signal_number;
    return fo_test_wait_child(pid, &status, &signal_number, 1) == 1 ? status : -999;
}
int fo_test_signal_group(int pid, int signal_number) {
    if (signal_number != 15 && signal_number != 9) { errno = ENOTSUP; return -1; }
    return fo_test_cancel(pid);
}
int fo_test_signal(int pid, int signal_number) {
    if (!signal_number) return fo_test_process_running(pid) ? 0 : -1;
    if (pid < 0) return fo_test_signal_group(-pid, signal_number);
    uint64_t birth = fo_test_process_start_time(pid);
    return fo_test_signal_identity(pid, birth, signal_number);
}
/* Native leaf scripts are fixed test producers; no user text enters the script. */
static int powershell_leaf(const char *script, char *const env[], const char *cwd) {
    wchar_t wide[MAX_PATH];
    UINT length = GetSystemDirectoryW(wide, MAX_PATH);
    const wchar_t *suffix = L"\\WindowsPowerShell\\v1.0\\powershell.exe";
    if (!length || length + wcslen(suffix) >= MAX_PATH) return -1;
    wcscat(wide, suffix);
    char path[4 * MAX_PATH];
    if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1,
        path, sizeof(path), NULL, NULL)) return -1;
    char *const argv[] = {path, "-NoLogo", "-NoProfile", "-NonInteractive", "-Command", (char *)script, NULL};
    return fo_test_spawn(argv, env, cwd);
}
int fo_test_spawn_sentinel(void) {
    return powershell_leaf("while ($true) { Start-Sleep -Seconds 1 }", NULL, ".");
}
int fo_test_spawn_same_group_sentinel(void) { return fo_test_spawn_sentinel(); }
int fo_test_spawn_heartbeat(const char *path, const char *cwd) {
    size_t bytes = strlen(path) + sizeof("FO_TEST_HEARTBEAT=");
    char *entry = malloc(bytes);
    if (!entry) return -1;
    snprintf(entry, bytes, "FO_TEST_HEARTBEAT=%s", path);
    char *const env[] = {entry, NULL};
    int pid = powershell_leaf("$f=[IO.File]::Open($env:FO_TEST_HEARTBEAT,'Append','Write','ReadWrite'); "
        "try { while($true){$f.WriteByte(46);$f.Flush();Start-Sleep -Milliseconds 20} } finally {$f.Dispose()}", env, cwd);
    free(entry); return pid;
}

int fo_test_open_write(const char *path) { return open_path(path, _O_CREAT | _O_TRUNC | _O_WRONLY); }

/* Adversarial test leaf: inherits the outer Job and streams, never creates a
   nested Fo owner that would remove its descendant when this parent exits. */
extern wchar_t *fx_win32_command_line(const char *, int, int);
int fo_test_spawn_unowned(char *const argv[], const char *cwd) {
    size_t length = 0; int count = 0;
    while (argv[count]) { length += strlen(argv[count++]) + 1; if (length > INT_MAX) return -1; }
    if (!count) return -1;
    char *packed = malloc(length); size_t at = 0;
    if (!packed) return -1;
    for (int i = 0; i < count; ++i) {
        size_t n = strlen(argv[i]) + 1; memcpy(packed + at, argv[i], n); at += n;
    }
    wchar_t *line = fx_win32_command_line(packed, (int)length, count);
    wchar_t *directory = fx_win32_utf16(cwd);
    wchar_t *image = fx_win32_utf16(argv[0]);
    free(packed);
    STARTUPINFOW startup = {0}; PROCESS_INFORMATION child = {0};
    startup.cb = sizeof(startup); startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    BOOL created = line && directory && image && CreateProcessW(image, line, NULL,
        NULL, TRUE, CREATE_UNICODE_ENVIRONMENT, NULL, directory, &startup, &child);
    free(line); free(directory); free(image);
    if (!created) return -1;
    CloseHandle(child.hThread); CloseHandle(child.hProcess);
    return child.dwProcessId <= INT_MAX ? (int)child.dwProcessId : -1;
}
static BOOL WINAPI ignore_console_term(DWORD event) {
    return event == CTRL_C_EVENT || event == CTRL_BREAK_EVENT;
}
int fo_test_ignore_term(void) { return SetConsoleCtrlHandler(ignore_console_term, TRUE) ? 0 : -1; }
int fo_test_terminate_self(void) { return TerminateProcess(GetCurrentProcess(), 143) ? 0 : -1; }
int fo_test_getpid(void) { return (int)GetCurrentProcessId(); }
int fo_test_chdir(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    int result = wide && SetCurrentDirectoryW(wide) && !_wchdir(wide) ? 0 : -1;
    free(wide); return result;
}
int fo_test_setenv(const char *name, const char *value, int overwrite) {
    wchar_t *n = fx_win32_utf16(name), *v = fx_win32_utf16(value);
    if (!n || !v) { free(n); free(v); return -1; }
    SetLastError(ERROR_SUCCESS);
    DWORD existing = GetEnvironmentVariableW(n, NULL, 0);
    DWORD error = GetLastError();
    if (!overwrite && (existing || error != ERROR_ENVVAR_NOT_FOUND)) {
        free(n); free(v); return 0;
    }
    BOOL changed = SetEnvironmentVariableW(n, v);
    /* Fortran GET_ENVIRONMENT_VARIABLE reads the CRT environment too. */
    int crt = changed ? _wputenv_s(n, v) : -1;
    free(n); free(v); return crt ? -1 : 0;
}

/* Real protected owner-only DACL; Unix mode-bit inspection has no Win32 meaning. */
#include <sddl.h>
int fo_test_private_mode(const char *path, int directory) {
    HMODULE security = LoadLibraryW(L"advapi32.dll");
    if (!security) return -1;
    typedef BOOL (WINAPI *convert_fn)(LPCWSTR, DWORD, PSECURITY_DESCRIPTOR *, PULONG);
    typedef BOOL (WINAPI *set_fn)(LPCWSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR);
    union { FARPROC raw; convert_fn typed; } convert;
    union { FARPROC raw; set_fn typed; } set;
    convert.raw = GetProcAddress(security, "ConvertStringSecurityDescriptorToSecurityDescriptorW");
    set.raw = GetProcAddress(security, "SetFileSecurityW");
    PSECURITY_DESCRIPTOR descriptor = NULL;
    wchar_t *wide = fx_win32_utf16(path);
    BOOL changed = wide && convert.raw && set.raw && convert.typed(directory ? L"D:P(A;OICI;FA;;;OW)" : L"D:P(A;;FA;;;OW)",
        SDDL_REVISION_1, &descriptor, NULL) && set.typed(wide,
        DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION, descriptor);
    if (descriptor) LocalFree(descriptor);
    free(wide); FreeLibrary(security); return changed ? 0 : -1;
}
int fo_test_mode_bits(const char *path) { (void)path; errno = ENOTSUP; return -1; }
int fo_test_release_fifo(const char *path, int timeout_ms) {
    (void)path; (void)timeout_ms; errno = ENOTSUP; return -1;
}

int fo_test_mkfifo(const char *path, int mode) { (void)path; (void)mode; errno = ENOTSUP; return -1; }

int fo_test_is_regular_file(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    if (!wide) return 0;
    HANDLE file = CreateFileW(wide, FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, NULL);
    free(wide);
    if (file == INVALID_HANDLE_VALUE) return 0;
    BY_HANDLE_FILE_INFORMATION info;
    int regular = GetFileType(file) == FILE_TYPE_DISK &&
        GetFileInformationByHandle(file, &info) && !(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
    CloseHandle(file); return regular;
}
