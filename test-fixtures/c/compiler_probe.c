/* A real compiler wrapper whose --version observation can be held independently. */
#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#include <process.h>
int wmain(int argc, wchar_t **argv) {
    const wchar_t *marker = _wgetenv(L"FO_TEST_COMPILER_MARKER");
    if (marker) {
        FILE *log = _wfopen(marker, L"a");
        if (!log) return 124;
        fputs("compiler invoked\n", log);
        if (fclose(log)) return 125;
    }
    const wchar_t *hold = _wgetenv(L"FO_TIMEOUT_VERSION_HOLD");
    if (argc == 2 && !wcscmp(argv[1], L"--version") && hold && _waccess(hold, 0) == 0) {
        const wchar_t *entered = _wgetenv(L"FO_TIMEOUT_VERSION_ENTERED");
        FILE *stream = entered ? _wfopen(entered, L"w") : NULL;
        if (!stream) return 120;
        fprintf(stream, "%lu\n", GetCurrentProcessId());
        if (fclose(stream)) return 121;
        const wchar_t *release = _wgetenv(L"FO_TIMEOUT_VERSION_RELEASE");
        if (!release) return 122;
        while (_waccess(release, 0)) Sleep(20);
    }
    const wchar_t *compiler = _wgetenv(L"FO_TEST_REAL_COMPILER");
    if (!compiler) return 123;
    argv[0] = (wchar_t *)compiler;
    _wexecv(compiler, (const wchar_t *const *)argv);
    return 127;
}
#else
#include <unistd.h>
#include <time.h>
int main(int argc, char **argv) {
    const char *marker = getenv("FO_TEST_COMPILER_MARKER");
    if (marker) {
        FILE *log = fopen(marker, "a");
        if (!log) return 124;
        fputs("compiler invoked\n", log);
        if (fclose(log)) return 125;
    }
    const char *hold = getenv("FO_TIMEOUT_VERSION_HOLD");
    if (argc == 2 && !strcmp(argv[1], "--version") && hold && !access(hold, F_OK)) {
        const char *entered = getenv("FO_TIMEOUT_VERSION_ENTERED");
        FILE *stream = entered ? fopen(entered, "w") : NULL;
        if (!stream) return 120;
        fprintf(stream, "%ld\n", (long)getpid());
        if (fclose(stream)) return 121;
        const char *release = getenv("FO_TIMEOUT_VERSION_RELEASE");
        if (!release) return 122;
        struct timespec delay = {0, 20000000};
        while (access(release, F_OK)) nanosleep(&delay, NULL);
    }
    const char *compiler = getenv("FO_TEST_REAL_COMPILER");
    if (!compiler) return 123;
    argv[0] = (char *)compiler;
    execv(compiler, argv);
    return 127;
}
#endif
