#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
extern int fo_c_unsetenv(const char *);
int main(void) {
    const char *key = "FO_GREMLIN_EXECUTION_CWD";
    if (_putenv_s(key, "private known routing hint") ||
        !SetEnvironmentVariableW(L"FO_GREMLIN_EXECUTION_CWD", L"private known routing hint")) return 2;
    if (!getenv(key)) return 3;
    if (fo_c_unsetenv(key) || getenv(key)) return 4;
    wchar_t value[128]; SetLastError(ERROR_SUCCESS);
    if (GetEnvironmentVariableW(L"FO_GREMLIN_EXECUTION_CWD", value, 128) ||
        GetLastError() != ERROR_ENVVAR_NOT_FOUND) return 5;
    if (fo_c_unsetenv(key)) return 6;
    errno = 0;
    if (fo_c_unsetenv("invalid=name") != -1 || errno != EINVAL) return 7;
    puts("PASS: production unset removes the routing hint from CRT and native child environment");
    return 0;
}
