/* Adversarial fixture leaf: inherits the current Job without creating another. */
#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdint.h>
#include <limits.h>
#include <stdlib.h>
#include <wchar.h>
int fo_fixture_spawn_descendant(void) {
    wchar_t image[32768];
    DWORD length = GetModuleFileNameW(NULL, image, 32768);
    if (!length || length >= 32768) return -1;
    const wchar_t suffix[] = L"\" --fo-fixture-sleep";
    wchar_t *line = calloc((size_t)length + 1 + sizeof(suffix) / sizeof(wchar_t), sizeof(wchar_t));
    if (!line) return -1;
    line[0] = L'"'; memcpy(line + 1, image, length * sizeof(wchar_t));
    wcscpy(line + length + 1, suffix);
    STARTUPINFOW startup = {0}; PROCESS_INFORMATION child = {0};
    startup.cb = sizeof(startup); startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    BOOL created = CreateProcessW(image, line, NULL, NULL, TRUE,
        CREATE_UNICODE_ENVIRONMENT, NULL, NULL, &startup, &child);
    free(line);
    if (!created) return -1;
    DWORD pid = child.dwProcessId;
    CloseHandle(child.hThread); CloseHandle(child.hProcess);
    return pid <= INT_MAX ? (int)pid : -1;
}
int fo_fixture_wait_forever(void) { Sleep(INFINITE); return -1; }
char *fo_fixture_getcwd(char *buffer, size_t capacity) {
    DWORD count = GetCurrentDirectoryW(0, NULL);
    wchar_t *wide = count ? calloc(count, sizeof(wchar_t)) : NULL;
    if (!wide || capacity > INT_MAX) { free(wide); return NULL; }
    DWORD length = GetCurrentDirectoryW(count, wide);
    int written = length && length < count ? WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
        wide, -1, buffer, (int)capacity, NULL, NULL) : 0;
    free(wide); return written ? buffer : NULL;
}
#endif
