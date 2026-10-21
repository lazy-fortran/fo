#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <direct.h>
#else
#include <unistd.h>
#endif

/* Hook-local policy; the process exits after checking the snapshot. */
int fo_c_hook_prepare(const char *root, const char *view, int build_cap) {
    char source[8192], destination[8192], seconds[32];
    struct stat st;
    snprintf(seconds, sizeof(seconds), "%d", build_cap);
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (_putenv_s("FO_BUILD_TIMEOUT", seconds) ||
        _putenv_s("GIT_ALLOW_PROTOCOL", "file")) return EINVAL;
#else
    if (setenv("FO_BUILD_TIMEOUT", seconds, 1) ||
        setenv("GIT_ALLOW_PROTOCOL", "file", 1)) return errno;
#endif
    if (snprintf(source, sizeof(source), "%s/build/dependencies", root) >= (int)sizeof(source) ||
        snprintf(destination, sizeof(destination), "%s/build", view) >= (int)sizeof(destination))
        return ENAMETOOLONG;
    if (stat(source, &st) != 0 || !S_ISDIR(st.st_mode)) return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    return ENOTSUP;
#else
    if (mkdir(destination, 0700) && errno != EEXIST) return errno;
    if (snprintf(destination, sizeof(destination), "%s/build/dependencies", view) >= (int)sizeof(destination))
        return ENAMETOOLONG;
    /* Do not replace a dependency tree deliberately included in the index. */
    if (lstat(destination, &st) == 0) return 0;
    if (symlink(source, destination)) return errno;
    return 0;
#endif
}
