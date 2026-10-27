#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <direct.h>
#else
#include <unistd.h>
#endif

/* Append `addition` unless an earlier stage already contributed it, so a
   bounded hook that re-runs in a child cannot grow the argument string. */
static int append_once(char *dst, size_t size, const char *addition) {
    size_t used = strlen(dst);
    if (addition == NULL || addition[0] == '\0') return 0;
    if (strstr(dst, addition) != NULL) return 0;
    if (used + strlen(addition) + 1 > size) return ENAMETOOLONG;
    memcpy(dst + used, addition, strlen(addition) + 1);
    return 0;
}

/* Hook-local policy; the process exits after checking the snapshot. */
int fo_c_hook_prepare(const char *root, const char *view, int build_cap,
                      const char *reuse) {
    char source[8192], destination[8192], seconds[32];
    struct stat st;
    const char *prior = getenv("FO_CMAKE_ARGS");
    const char *offline = " -DFETCHCONTENT_FULLY_DISCONNECTED=ON"
                          " -DFETCHCONTENT_UPDATES_DISCONNECTED=ON";
    size_t size = (prior ? strlen(prior) : 0) + strlen(offline) +
                  (reuse ? strlen(reuse) : 0) + 1;
    char *arguments = malloc(size);
    int status;
    if (!arguments) return ENOMEM;
    snprintf(arguments, size, "%s", prior ? prior : "");
    status = append_once(arguments, size, offline);
    if (status == 0) status = append_once(arguments, size, reuse);
    if (status == 0) {
#if defined(_WIN32) && !defined(__CYGWIN__)
        status = _putenv_s("FO_CMAKE_ARGS", arguments) ? EINVAL : 0;
#else
        status = setenv("FO_CMAKE_ARGS", arguments, 1) ? errno : 0;
#endif
    }
    free(arguments);
    if (status) return status;
    snprintf(seconds, sizeof(seconds), "%d", build_cap);
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (_putenv_s("FO_CACHE_RELOCATABLE", "1") ||
        _putenv_s("FO_BUILD_TIMEOUT", seconds) ||
        _putenv_s("GIT_ALLOW_PROTOCOL", "file")) return EINVAL;
#else
    if (setenv("FO_CACHE_RELOCATABLE", "1", 1) ||
        setenv("FO_BUILD_TIMEOUT", seconds, 1) ||
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
