#ifndef FO_WINDOWS_ENV_H
#define FO_WINDOWS_ENV_H
#if defined(_WIN32) && !defined(__CYGWIN__)
/* Fo's native image manifest makes UCRT environment strings UTF-8. */
static inline int fo_windows_setenv(const char *name, const char *value, int overwrite) {
    if (!overwrite && getenv(name)) return 0;
    int error = _putenv_s(name, value);
    if (!error) return 0;
    errno = error;
    return -1;
}
static inline int fo_windows_unsetenv(const char *name) {
    int error = _putenv_s(name, "");
    if (!error) return 0;
    errno = error;
    return -1;
}
#define setenv fo_windows_setenv
#define unsetenv fo_windows_unsetenv
#endif
#endif
