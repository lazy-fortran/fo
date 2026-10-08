#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win32.h"
#endif

/* Clear both the language runtime and the environment inherited by children. */
int fo_c_unsetenv(const char *name) {
    if (!name || !*name || strchr(name, '=')) { errno = EINVAL; return -1; }
#if defined(_WIN32) && !defined(__CYGWIN__)
    wchar_t *wide = fx_win32_wide(name);
    if (!wide) return fx_win32_errno(GetLastError());
    int error = _wputenv_s(wide, L"");
    if (!error && !SetEnvironmentVariableW(wide, NULL)) {
        DWORD native_error = GetLastError();
        if (native_error != ERROR_ENVVAR_NOT_FOUND) {
            free(wide); return fx_win32_errno(native_error);
        }
    }
    free(wide);
    if (error) { errno = error; return -1; }
    return 0;
#else
    return unsetenv(name);
#endif
}
