/* Independent native Win32 process oracle. Link with the owning Fo process
   object and Fx UTF-8/argv boundary. No shell or interpreter runs the oracle. */
#define _WIN32_WINNT 0x0a00
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <errno.h>
#include <winioctl.h>
#include <stddef.h>
#include <io.h>
#include <fcntl.h>

extern wchar_t *fx_win32_utf16(const char *);
extern wchar_t *fx_win32_command_line(const char *, int, int);
void fo_c_run_argv_budget(const char *, const char *, int, int, const char *, int,
    int, int, int, const char *, int *, int *, long long *, long long *);
void fo_c_start_argv_logged(const char *, const char *, int, int, const char *,
    const char *, int *, int *);
void fo_c_poll_pid(int, int *, int *);
void fo_c_cancel_pid(int, int *);
int fo_c_start_redirected_async(const char *, char *const [], char *const [], int, int, int);
int fo_c_process_set_async_scope(const char *, int, const char *);
int fo_c_process_push_async_scope(const char *, void **);
int fo_c_process_pop_async_scope(void **);
int fo_c_process_owned_by_scope(const char *, int, const char *);
int fo_c_recover_async_scope(const char *, int, const char *);
int fo_c_self_executable(char *, int);
void fo_c_getcwd(char *, int, int *);

static int failures;
static char image[32768], scratch[32768];
static void check(int okay, const char *message) {
    printf("%s: %s\n", okay ? "PASS" : "FAIL", message); fflush(stdout);
    if (!okay) ++failures;
}
static void diagnostic_handles(const char *phase) {
    DWORD count = 0;
    GetProcessHandleCount(GetCurrentProcess(), &count);
    printf("native-handles-phase: %s=%lu\n", phase, (unsigned long)count);
}
static uint64_t creation(HANDLE process) {
    FILETIME birth, exit, kernel, user;
    if (!GetProcessTimes(process, &birth, &exit, &kernel, &user)) return 0;
    return ((uint64_t)birth.dwHighDateTime << 32) | birth.dwLowDateTime;
}
int fo_gremlin_process_matches(int pid, const char *text) {
    if (pid <= 0 || !text || !*text) return 0;
    char *end;
    uint64_t expected = strtoull(text, &end, 10);
    if (*end || !expected) return 0;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!process) return 0;
    int match = creation(process) == expected && WaitForSingleObject(process, 0) == WAIT_TIMEOUT;
    CloseHandle(process); return match;
}
static char *utf8(const wchar_t *wide) {
    int count = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, NULL, 0, NULL, NULL);
    char *text = count ? malloc((size_t)count) : NULL;
    if (text) WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, text, count, NULL, NULL);
    return text;
}
static FILE *open_utf8(const char *path, const wchar_t *mode) {
    wchar_t *wide = fx_win32_utf16(path);
    FILE *stream = wide ? _wfopen(wide, mode) : NULL;
    free(wide); return stream;
}
static void make_path(char *path, size_t capacity, const char *name) {
    snprintf(path, capacity, "%s/%s", scratch, name);
}
static size_t file_size(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    WIN32_FILE_ATTRIBUTE_DATA value;
    int success = wide && GetFileAttributesExW(wide, GetFileExInfoStandard, &value);
    free(wide);
    return success ? (size_t)(((uint64_t)value.nFileSizeHigh << 32) | value.nFileSizeLow) : 0;
}
static int append_bytes(const char *file, const char *bytes, DWORD length) {
    wchar_t *wide = fx_win32_utf16(file);
    HANDLE output = wide ? CreateFileW(wide, FILE_APPEND_DATA,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_ALWAYS,
        FILE_ATTRIBUTE_NORMAL, NULL) : INVALID_HANDLE_VALUE;
    free(wide);
    if (output == INVALID_HANDLE_VALUE) return 0;
    DWORD written = 0;
    int okay = WriteFile(output, bytes, length, &written, NULL) && written == length;
    CloseHandle(output); return okay;
}
static void record_identity(const char *file, const char *role) {
    char line[256];
    int length = snprintf(line, sizeof(line), "%s %lu %llu\n", role, (unsigned long)GetCurrentProcessId(),
        (unsigned long long)creation(GetCurrentProcess()));
    if (length <= 0 || !append_bytes(file, line, (DWORD)length)) ExitProcess(93);
}
static int packed(char *buffer, size_t capacity, const char **arguments, int count) {
    size_t used = 0;
    for (int i = 0; i < count; ++i) {
        size_t length = strlen(arguments[i]) + 1;
        if (used + length > capacity) return 0;
        memcpy(buffer + used, arguments[i], length); used += length;
    }
    return (int)used;
}
static PROCESS_INFORMATION independent_spawn(const char **arguments, int count) {
    char buffer[65536];
    int length = packed(buffer, sizeof(buffer), arguments, count);
    wchar_t *command = fx_win32_command_line(buffer, length, count);
    wchar_t *application = fx_win32_utf16(arguments[0]);
    STARTUPINFOW startup = {0}; startup.cb = sizeof(startup);
    PROCESS_INFORMATION process = {0};
    if (!command || !application || !CreateProcessW(application, command, NULL, NULL, FALSE,
        CREATE_UNICODE_ENVIRONMENT | CREATE_NEW_PROCESS_GROUP, NULL, NULL, &startup, &process))
        check(0, "independent control starts through native CreateProcessW");
    free(command); free(application);
    if (process.hThread) CloseHandle(process.hThread);
    return process;
}
static int independent_pipe_roundtrip(void) {
    int descriptors[2]; char value[16] = {0};
    if (_pipe(descriptors, 4096, _O_BINARY | _O_NOINHERIT)) return 0;
    int okay = _write(descriptors[1], "pipe-token", 10) == 10 &&
        _read(descriptors[0], value, sizeof(value)) == 10 && !memcmp(value, "pipe-token", 10);
    _close(descriptors[0]); _close(descriptors[1]);
    return okay;
}
static BOOL WINAPI ignore_break(DWORD event) { (void)event; return TRUE; }
static void heartbeat(const char *file, int burn) {
    SetConsoleCtrlHandler(ignore_break, TRUE);
    ULONGLONG next = 0;
    volatile uint64_t accumulator = 1;
    for (;;) {
        if (GetTickCount64() >= next) {
            if (!append_bytes(file, "heartbeat\n", 10)) ExitProcess(94);
            next = GetTickCount64() + 30;
        }
        if (burn) { for (int i = 0; i < 100000; ++i) accumulator = accumulator * 13 + 7; }
        else Sleep(10);
    }
}
static int helper(int argc, wchar_t **argv) {
    if (argc < 3) return 91;
    char *file = utf8(argv[2]);
    if (!file) return 92;
    if (!wcscmp(argv[1], L"--pipe-control")) {
        DWORD before = 0, after = 0, first = 0;
        GetProcessHandleCount(GetCurrentProcess(), &before);
        for (int iteration = 0; iteration < 5; ++iteration) {
            for (int index = 0; index < 3; ++index) {
                if (!independent_pipe_roundtrip()) return 81;
            }
            GetProcessHandleCount(GetCurrentProcess(), &after);
            if (!iteration) first = after;
        }
        printf("independent-CRT-pipe-handles: before=%lu first=%lu after5=%lu\n",
            (unsigned long)before, (unsigned long)first, (unsigned long)after);
        free(file); return 0;
    }
    if (!wcscmp(argv[1], L"--runtime-control")) {
        DWORD before = 0, after = 0, first = 0;
        GetProcessHandleCount(GetCurrentProcess(), &before);
        const char *arguments[] = {image, "--exit", "unused"};
        for (int index = 0; index < 5; ++index) {
            PROCESS_INFORMATION child = independent_spawn(arguments, 3);
            if (!child.hProcess || WaitForSingleObject(child.hProcess, 5000) != WAIT_OBJECT_0) return 84;
            CloseHandle(child.hProcess);
            GetProcessHandleCount(GetCurrentProcess(), &after);
            if (!index) first = after;
        }
        printf("independent-CreateProcessW-runtime-handles: before=%lu first=%lu after5=%lu\n",
            (unsigned long)before, (unsigned long)first, (unsigned long)after);
        free(file); return 0;
    }
    if (!wcscmp(argv[1], L"--arguments")) {
        int okay = argc == 10 && !wcscmp(argv[3], L"") && !wcscmp(argv[4], L"space tab\tvalue") &&
            !wcscmp(argv[5], L"quote\"value") && !wcscmp(argv[6], L"trailing\\\\") &&
            !wcscmp(argv[7], L"slash\\\"quoted") && !wcscmp(argv[8], L"caf\u00e9-\U0001f680");
        wchar_t environment[256], directory[32768], expected_directory[32768];
        okay = okay && GetEnvironmentVariableW(L"FO_WIN32_VALUE", environment, 256) &&
            !wcscmp(environment, L"caf\u00e9 value") && GetCurrentDirectoryW(32768, directory) &&
            GetFullPathNameW(argv[2], 32768, expected_directory, NULL) &&
            CompareStringOrdinal(directory, -1, expected_directory, -1, TRUE) == CSTR_EQUAL;
        if (argc == 10) {
            uintptr_t private_handle = (uintptr_t)wcstoull(argv[9], NULL, 10);
            SetEvent((HANDLE)private_handle);
        }
        puts("native-stdout-token"); fputs("native-stderr-token\n", stderr);
        return okay ? 23 : 92;
    }
    if (!wcscmp(argv[1], L"--tree") || !wcscmp(argv[1], L"--tree-exit") ||
        !wcscmp(argv[1], L"--tree-cpu")) {
        record_identity(file, "leader");
        const char *child_arguments[] = {image, "--leaf", file,
            !wcscmp(argv[1], L"--tree-cpu") ? "burn" : "idle"};
        PROCESS_INFORMATION child = independent_spawn(child_arguments, 4);
        if (!child.hProcess) return 95;
        CloseHandle(child.hProcess);
        if (!wcscmp(argv[1], L"--tree-exit")) { puts("early-leader-output-token"); return 17; }
        heartbeat(file, 0);
    }
    if (!wcscmp(argv[1], L"--leaf")) {
        const char *scope = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
        const char *owner = getenv("FO_GREMLIN_PROCESS_SCOPE_PID");
        const char *birth = getenv("FO_GREMLIN_PROCESS_SCOPE_START");
        if (scope && owner && birth) {
            if (!fo_c_process_owned_by_scope(scope, atoi(owner), birth)) return 86;
            if (!append_bytes(file, "scope-owned\n", 12)) return 85;
        }
        record_identity(file, "leaf"); heartbeat(file, argc > 3 && !wcscmp(argv[3], L"burn"));
    }
    if (!wcscmp(argv[1], L"--sentinel")) { record_identity(file, "sentinel"); heartbeat(file, 0); }
    if (!wcscmp(argv[1], L"--streams")) {
        char input[32] = {0}; DWORD received = 0, written = 0;
        wchar_t value[64], actual_cwd[32768], expected_cwd[32768];
        int okay = ReadFile(GetStdHandle(STD_INPUT_HANDLE), input, sizeof(input), &received, NULL) &&
            received == 13 && !memcmp(input, "stdin-token\r\n", 13) &&
            GetEnvironmentVariableW(L"FO_WIN32_VALUE", value, 64) && !wcscmp(value, L"caf\u00e9; intact") &&
            GetCurrentDirectoryW(32768, actual_cwd) &&
            GetFullPathNameW(argv[2], 32768, expected_cwd, NULL) &&
            CompareStringOrdinal(actual_cwd, -1, expected_cwd, -1, TRUE) == CSTR_EQUAL;
        const char output[] = "stdout-token\r\n", diagnostic[] = "stderr-token\r\n";
        okay = okay && WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), output, sizeof(output) - 1, &written, NULL) &&
            WriteFile(GetStdHandle(STD_ERROR_HANDLE), diagnostic, sizeof(diagnostic) - 1, &written, NULL);
        free(file); return okay ? 23 : 83;
    }
    if (!wcscmp(argv[1], L"--exit")) { puts("completed-output-token"); return 23; }
    if (!wcscmp(argv[1], L"--crash-owner")) {
        if (argc != 4) return 96;
        char *directory = utf8(argv[3]);
        char birth[64];
        snprintf(birth, sizeof(birth), "%llu", (unsigned long long)creation(GetCurrentProcess()));
        if (!directory || fo_c_process_set_async_scope(directory, (int)GetCurrentProcessId(), birth)) return 97;
        char buffer[65536];
        const char *arguments[] = {image, "--tree", file};
        int pid, error;
        fo_c_start_argv_logged(directory, buffer, packed(buffer, sizeof(buffer), arguments, 3), 3,
            "NUL", NULL, &pid, &error);
        if (error) return 98;
        record_identity(file, "scope-owner"); heartbeat(file, 0);
    }
    free(file); return 90;
}

struct identity { DWORD pid; uint64_t birth; };
static int identities(const char *file, struct identity *values, int capacity) {
    FILE *stream = open_utf8(file, L"rb");
    if (!stream) return 0;
    char line[256], role[32];
    unsigned long pid; unsigned long long birth;
    int count = 0;
    while (fgets(line, sizeof(line), stream)) {
        if (sscanf(line, "%31s %lu %llu", role, &pid, &birth) == 3 && count < capacity)
            values[count++] = (struct identity){(DWORD)pid, (uint64_t)birth};
    }
    fclose(stream); return count;
}
static int identity_live(struct identity value) {
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, value.pid);
    if (!process) return 0;
    int live = creation(process) == value.birth && WaitForSingleObject(process, 0) == WAIT_TIMEOUT;
    CloseHandle(process); return live;
}
static int wait_identities(const char *file, struct identity *values, int expected) {
    ULONGLONG end = GetTickCount64() + 5000;
    int count;
    do {
        count = identities(file, values, 8);
        if (count >= expected) return count;
        Sleep(20);
    } while (GetTickCount64() < end);
    check(0, "real descendants publish exact independent birth records within five seconds");
    return count;
}
static void check_gone(struct identity *values, int count, const char *file) {
    check(count >= 2, "tree oracle observes both leader and descendant identities");
    for (int i = 0; i < count; ++i) check(!identity_live(values[i]), "exact owned process absent at return");
    size_t stopped = file_size(file); Sleep(300);
    check(file_size(file) == stopped, "owned heartbeat bytes stay frozen after return");
}
static int junction(const char *target, const char *alias) {
    wchar_t *wide_target = fx_win32_utf16(target), *wide_alias = fx_win32_utf16(alias);
    wchar_t physical[4096];
    int okay = wide_target && wide_alias && GetFullPathNameW(wide_target, 4096, physical, NULL) &&
        CreateDirectoryW(wide_alias, NULL);
    HANDLE directory = okay ? CreateFileW(wide_alias, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
        FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL) : INVALID_HANDLE_VALUE;
    struct mount_point {
        DWORD tag; WORD length, reserved;
        WORD substitute_offset, substitute_length, print_offset, print_length;
        wchar_t names[8192];
    } point = {0};
    point.tag = IO_REPARSE_TAG_MOUNT_POINT;
    if (okay) {
        size_t length = wcslen(physical), substituted = length + 4;
        swprintf(point.names, 8192, L"\\??\\%ls", physical);
        memcpy(point.names + substituted + 1, physical, (length + 1) * sizeof(wchar_t));
        point.substitute_length = (WORD)(substituted * sizeof(wchar_t));
        point.print_offset = (WORD)((substituted + 1) * sizeof(wchar_t));
        point.print_length = (WORD)(length * sizeof(wchar_t));
        point.length = (WORD)(8 + (substituted + length + 2) * sizeof(wchar_t));
    }
    DWORD returned;
    okay = okay && directory != INVALID_HANDLE_VALUE && DeviceIoControl(directory, FSCTL_SET_REPARSE_POINT,
        &point, point.length + 8, NULL, 0, &returned, NULL);
    if (directory != INVALID_HANDLE_VALUE) CloseHandle(directory);
    free(wide_target); free(wide_alias); return okay;
}

int wmain(int argc, wchar_t **argv) {
    if (fo_c_self_executable(image, sizeof(image))) return 89;
    if (argc > 1 && wcsncmp(argv[1], L"--", 2) == 0) return helper(argc, argv);
    if (argc != 2) { fputs("usage: windows_process.exe PRIVATE_EXISTING_SCRATCH\n", stderr); return 88; }
    char *root = utf8(argv[1]);
    if (!root) return 87;
    snprintf(scratch, sizeof(scratch), "%s", root); free(root);
    char directory[32768], log[32768], buffer[65536], cwd[32768];
    make_path(directory, sizeof(directory), "space-caf\xc3\xa9-\xf0\x9f\x9a\x80");
    wchar_t *wide_directory = fx_win32_utf16(directory);
    check(wide_directory && CreateDirectoryW(wide_directory, NULL), "creates actual Unicode working directory");
    free(wide_directory);
    make_path(log, sizeof(log), "arguments.log");
    /* A bare CreateProcessW control independently establishes the platform's
       fixed first-launch initialization before measuring Fo handle ownership.
       The final comparison remains exact; no handle allowance is permitted. */
    const char *warm_arguments[] = {image, "--exit", "unused"};
    PROCESS_INFORMATION warm_child = independent_spawn(warm_arguments, 3);
    DWORD warm_exit = 0;
    check(warm_child.hProcess && WaitForSingleObject(warm_child.hProcess, 5000) == WAIT_OBJECT_0 &&
        GetExitCodeProcess(warm_child.hProcess, &warm_exit) && warm_exit == 23,
        "bare Windows API control completes before Fo handle baseline");
    if (warm_child.hProcess) CloseHandle(warm_child.hProcess);
    /* The independent --pipe-control also establishes one fixed UCRT pipe
       initialization handle across five repeated rounds, outside Fo. */
    int warm_pipes = 1;
    for (int index = 0; index < 3; ++index)
        if (!independent_pipe_roundtrip()) warm_pipes = 0;
    check(warm_pipes, "bare CRT pipe control completes before Fo handle baseline");
    DWORD handles_before = 0;
    GetProcessHandleCount(GetCurrentProcess(), &handles_before);
    SECURITY_ATTRIBUTES inheritable = {sizeof(inheritable), NULL, TRUE};
    HANDLE private_event = CreateEventW(&inheritable, TRUE, FALSE, NULL);
    char event_argument[64];
    snprintf(event_argument, sizeof(event_argument), "%llu", (unsigned long long)(uintptr_t)private_event);
    const char *argument_test[] = {image, "--arguments", directory, "", "space tab\tvalue",
        "quote\"value", "trailing\\\\", "slash\\\"quoted", "caf\xc3\xa9-\xf0\x9f\x9a\x80", event_argument};
    int code, kind, pid, done, error;
    long long cpu, wall;
    SetEnvironmentVariableW(L"FO_WIN32_VALUE", L"inherited wrong value");
    fo_c_run_argv_budget(directory, buffer, packed(buffer, sizeof(buffer), argument_test, 10), 10,
        log, 0, 0, 5, 0, "FO_WIN32_VALUE=caf\xc3\xa9 value", &code, &kind, &cpu, &wall);
    check(code == 23, "UTF8 argv empty/space/tab/quote/backslash/Unicode, cwd and env reach real child exactly");
    check(private_event && WaitForSingleObject(private_event, 0) == WAIT_TIMEOUT,
        "inheritable private event handle cannot be used by the child");
    if (private_event) CloseHandle(private_event);
    diagnostic_handles("after-first-sync-and-event-cleanup");
    check(kind == 0 && cpu >= 0 && wall >= 0, "completed child reports measured job CPU and wall time");
    FILE *stream = open_utf8(log, L"rb");
    char output[4096] = {0};
    if (stream) { fread(output, 1, sizeof(output) - 1, stream); fclose(stream); }
    check(strstr(output, "native-stdout-token") && strstr(output, "native-stderr-token"),
        "public logged run retains both standard output and diagnostics");
    fo_c_getcwd(cwd, sizeof(cwd), &error);
    check(!error && strlen(cwd) > 0, "native current working directory is available as UTF8");
    const char *normal[] = {image, "--exit", "unused"};
    int length = packed(buffer, sizeof(buffer), normal, 3);
    fo_c_start_argv_logged(directory, buffer, length - 1, 3, log, NULL, &pid, &error);
    check(error != 0 && pid == 0, "unterminated packed argv is rejected without launching a child");
    fo_c_start_argv_logged(directory, buffer, length, 4, log, NULL, &pid, &error);
    check(error != 0 && pid == 0, "inconsistent argument count is rejected without out-of-bounds reads");
    fo_c_start_argv_logged(directory, buffer, length, 3, log, NULL, &pid, &error);
    check(!error && pid > 0, "starts exact native async child");
    ULONGLONG deadline = GetTickCount64() + 5000;
    do { fo_c_poll_pid(pid, &done, &code); if (!done) Sleep(20); }
    while (!done && GetTickCount64() < deadline);
    check(done && code == 23, "poll preserves actual completed exit code");
    fo_c_cancel_pid(pid, &error);
    check(error == ESRCH, "completed numeric PID no longer has cancellation authority");
    diagnostic_handles("after-first-async-and-malformed-cleanup");
    int input_pipe[2] = {-1, -1}, output_pipe[2] = {-1, -1}, diagnostic_pipe[2] = {-1, -1};
    int pipes_ok = !_pipe(input_pipe, 4096, _O_BINARY | _O_NOINHERIT) &&
        !_pipe(output_pipe, 4096, _O_BINARY | _O_NOINHERIT) &&
        !_pipe(diagnostic_pipe, 4096, _O_BINARY | _O_NOINHERIT);
    check(pipes_ok, "creates native CRT pipes without ambient inheritance");
    if (pipes_ok) {
        char *redirected_args[] = {image, "--streams", directory, NULL};
        char *redirected_env[] = {"FO_WIN32_VALUE=caf\xc3\xa9; intact", NULL};
        pid = fo_c_start_redirected_async(directory, redirected_args, redirected_env,
            input_pipe[0], output_pipe[1], diagnostic_pipe[1]);
        _close(input_pipe[0]); input_pipe[0] = -1;
        _close(output_pipe[1]); output_pipe[1] = -1;
        _close(diagnostic_pipe[1]); diagnostic_pipe[1] = -1;
        check(pid > 0, "redirected async spawn uses owned process authority");
        check(_write(input_pipe[1], "stdin-token\r\n", 13) == 13, "writes exact native child input");
        _close(input_pipe[1]); input_pipe[1] = -1;
        deadline = GetTickCount64() + 5000;
        done = 0; code = 0;
        if (pid > 0) {
            do { fo_c_poll_pid(pid, &done, &code); if (!done) Sleep(20); }
            while (!done && GetTickCount64() < deadline);
        }
        printf("redirected-result: done=%d exit=%d\n", done, code);
        check(done && code == 23, "redirected child receives native stdin, Unicode cwd and intact array environment");
        char actual_output[64] = {0}, actual_diagnostic[64] = {0};
        int output_count = _read(output_pipe[0], actual_output, sizeof(actual_output));
        int diagnostic_count = _read(diagnostic_pipe[0], actual_diagnostic, sizeof(actual_diagnostic));
        check(output_count == 14 && !memcmp(actual_output, "stdout-token\r\n", 14) &&
            diagnostic_count == 14 && !memcmp(actual_diagnostic, "stderr-token\r\n", 14),
            "redirected output and diagnostic pipes retain distinct exact byte streams");
    }
    for (int index = 0; index < 2; ++index) {
        if (input_pipe[index] >= 0) _close(input_pipe[index]);
        if (output_pipe[index] >= 0) _close(output_pipe[index]);
        if (diagnostic_pipe[index] >= 0) _close(diagnostic_pipe[index]);
    }
    char sentinel_file[32768], tree_file[32768];
    make_path(sentinel_file, sizeof(sentinel_file), "sentinel.heartbeat");
    const char *sentinel_args[] = {image, "--sentinel", sentinel_file};
    PROCESS_INFORMATION sentinel = independent_spawn(sentinel_args, 3);
    struct identity sentinel_identity = {sentinel.dwProcessId, creation(sentinel.hProcess)};
    struct identity values[8] = {0};
    wait_identities(sentinel_file, values, 1);
    make_path(tree_file, sizeof(tree_file), "owned.heartbeat");
    const char *tree_args[] = {image, "--tree", tree_file};
    fo_c_start_argv_logged(directory, buffer, packed(buffer, sizeof(buffer), tree_args, 3), 3,
        "NUL", NULL, &pid, &error);
    check(!error, "starts independently observed parent and descendant in owned job");
    int count = wait_identities(tree_file, values, 2);
    size_t sentinel_before = file_size(sentinel_file);
    ULONGLONG began = GetTickCount64();
    fo_c_cancel_pid(pid, &error);
    ULONGLONG elapsed = GetTickCount64() - began;
    check(!error, "cancellation drains exact job before reporting success");
    check(elapsed >= 1500 && elapsed < 3500, "TERM-resistant tree gets bounded 1.5-to-3.5-second grace");
    check_gone(values, count, tree_file);
    check(identity_live(sentinel_identity) && file_size(sentinel_file) > sentinel_before,
        "unrelated real process stays alive and keeps progressing after cancellation");
    diagnostic_handles("after-cancel-with-sentinel-handle");
    make_path(tree_file, sizeof(tree_file), "early-exit.heartbeat");
    const char *early_args[] = {image, "--tree-exit", tree_file};
    fo_c_start_argv_logged(directory, buffer, packed(buffer, sizeof(buffer), early_args, 3), 3,
        log, NULL, &pid, &error);
    check(!error, "starts early-finishing parent with a continuing descendant");
    count = wait_identities(tree_file, values, 2);
    Sleep(100);
    fo_c_poll_pid(pid, &done, &code);
    check(done && code == 17, "poll drains descendants while retaining original leader exit status");
    check_gone(values, count, tree_file);
    make_path(tree_file, sizeof(tree_file), "wall-budget.heartbeat");
    diagnostic_handles("after-early-exit-with-sentinel-handle");
    const char *wall_args[] = {image, "--tree", tree_file};
    fo_c_run_argv_budget(directory, buffer, packed(buffer, sizeof(buffer), wall_args, 3), 3,
        "NUL", 0, 2, 1, 0, NULL, &code, &kind, &cpu, &wall);
    check(code == 124 && kind == 2 && cpu >= 0 && cpu < 2000 && wall >= 1000 && wall < 3500,
        "idle tree reaches wall limit rather than inventing a CPU-budget timeout");
    count = identities(tree_file, values, 8); check_gone(values, count, tree_file);
    diagnostic_handles("after-wall-budget-with-sentinel-handle");
    make_path(tree_file, sizeof(tree_file), "cpu-budget.heartbeat");
    const char *cpu_args[] = {image, "--tree-cpu", tree_file};
    fo_c_run_argv_budget(directory, buffer, packed(buffer, sizeof(buffer), cpu_args, 3), 3,
        "NUL", 0, 1, 8, 0, NULL, &code, &kind, &cpu, &wall);
    check(code == 124 && kind == 1 && cpu >= 1000 && wall < 8000,
        "job CPU accounting includes burning grandchild and fires before wall cap");
    count = identities(tree_file, values, 8); check_gone(values, count, tree_file);
    diagnostic_handles("after-cpu-budget-with-sentinel-handle");
    char scope[32768], birth[64];
    make_path(scope, sizeof(scope), "owned-scope");
    wide_directory = fx_win32_utf16(scope);
    check(wide_directory && CreateDirectoryW(wide_directory, NULL), "creates private recovery scope");
    free(wide_directory);
    snprintf(birth, sizeof(birth), "%llu", (unsigned long long)creation(GetCurrentProcess()));
    check(fo_c_process_set_async_scope(scope, (int)GetCurrentProcessId(), "1") != 0,
        "wrong owner birth cannot acquire process scope authority");
    make_path(tree_file, sizeof(tree_file), "crash-recovery.heartbeat");
    const char *owner_args[] = {image, "--crash-owner", tree_file, scope};
    PROCESS_INFORMATION owner = independent_spawn(owner_args, 4);
    snprintf(birth, sizeof(birth), "%llu", (unsigned long long)creation(owner.hProcess));
    count = wait_identities(tree_file, values, 3);
    stream = open_utf8(tree_file, L"rb");
    memset(output, 0, sizeof(output));
    if (stream) { fread(output, 1, sizeof(output) - 1, stream); fclose(stream); }
    check(strstr(output, "scope-owned") != NULL,
        "real grandchild independently verifies inherited exact scope authority");
    check(fo_c_recover_async_scope(scope, (int)owner.dwProcessId, birth) == EBUSY,
        "recovery never steals a live exact owner scope");
    check(!fo_c_process_owned_by_scope(scope, (int)owner.dwProcessId, birth),
        "unrelated caller does not inherit another owner's admission authority");
    check(TerminateProcess(owner.hProcess, 99) && WaitForSingleObject(owner.hProcess, 5000) == WAIT_OBJECT_0,
        "crashes only independently recorded owner process");
    check(!fo_c_recover_async_scope(scope, (int)owner.dwProcessId, birth),
        "stale-scope recovery confirms kernel job drainage before returning success");
    check_gone(values, count, tree_file);
    check(identity_live(sentinel_identity), "independent sentinel survives owner crash and scoped recovery");
    CloseHandle(owner.hProcess);
    diagnostic_handles("after-foreign-owner-recovery-with-sentinel-handle");
    char alias_scope[32768];
    make_path(alias_scope, sizeof(alias_scope), "scope-junction");
    check(junction(scope, alias_scope), "creates an actual junction alias to the private scope");
    void *outer_scope = NULL, *inner_scope = NULL;
    check(!fo_c_process_push_async_scope(scope, &outer_scope), "arms live native owner scope through ready guardian handshake");
    snprintf(birth, sizeof(birth), "%llu", (unsigned long long)creation(GetCurrentProcess()));
    check(!fo_c_process_set_async_scope(alias_scope, (int)GetCurrentProcessId(), birth),
        "junction alias attaches to the same physical owner job without duplicate guardians");
    check(!fo_c_process_push_async_scope(alias_scope, &inner_scope), "nested physical alias preserves outer scope authority");
    make_path(tree_file, sizeof(tree_file), "alias-scope.heartbeat");
    const char *alias_args[] = {image, "--tree", tree_file};
    fo_c_start_argv_logged(directory, buffer, packed(buffer, sizeof(buffer), alias_args, 3), 3,
        "NUL", NULL, &pid, &error);
    check(!error, "real owned tree starts in a junction-attached scope");
    count = wait_identities(tree_file, values, 2);
    check(!fo_c_process_pop_async_scope(&inner_scope) && inner_scope == NULL,
        "nested same-job scope pop releases only its duplicate authority");
    check(count >= 2 && identity_live(values[0]) && identity_live(values[1]),
        "popping nested alias leaves the live outer-owned tree running");
    check(!fo_c_process_pop_async_scope(&outer_scope) && outer_scope == NULL,
        "outer scope pop waits for guardian's exact-handle drainage receipt");
    check_gone(values, count, tree_file);
    fo_c_cancel_pid(pid, &error);
    check(!error, "explicitly retires already-drained per-command ownership handles");
    diagnostic_handles("after-all-scope-popup-with-sentinel-handle");
    HANDLE scope_handle;
    wide_directory = fx_win32_utf16(scope);
    scope_handle = wide_directory ? CreateFileW(wide_directory, 0,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, NULL) : INVALID_HANDLE_VALUE;
    free(wide_directory);
    BY_HANDLE_FILE_INFORMATION scope_identity;
    check(scope_handle != INVALID_HANDLE_VALUE && GetFileInformationByHandle(scope_handle, &scope_identity),
        "negative oracle independently reads physical scope volume and file identity");
    if (scope_handle != INVALID_HANDLE_VALUE) CloseHandle(scope_handle);
    char undrained[65536], name[256];
    snprintf(name, sizeof(name), "FoProcessScope-%d-1-%08lx-%016llx.guard", INT_MAX,
        (unsigned long)scope_identity.dwVolumeSerialNumber,
        ((unsigned long long)scope_identity.nFileIndexHigh << 32) | scope_identity.nFileIndexLow);
    snprintf(undrained, sizeof(undrained), "%s/%s", scope, name);
    check(append_bytes(undrained, "armed\n", 6), "seeds independent armed-but-undrained authority debt");
    check(fo_c_recover_async_scope(scope, INT_MAX, "1") == EIO,
        "missing native job with undrained armed state never becomes false recovery success");
    check(TerminateProcess(sentinel.hProcess, 0) &&
        WaitForSingleObject(sentinel.hProcess, 5000) == WAIT_OBJECT_0, "cleans sentinel through its exact held process handle");
    CloseHandle(sentinel.hProcess);
    DWORD handles_after = 0;
    GetProcessHandleCount(GetCurrentProcess(), &handles_after);
    printf("native-handle-count: before=%lu after=%lu\n", (unsigned long)handles_before,
        (unsigned long)handles_after);
    check(handles_after == handles_before, "all process/thread/job/stream handles return to baseline after cleanup");
    printf("windows-process: %d failures\n", failures);
    return failures ? 1 : 0;
}
