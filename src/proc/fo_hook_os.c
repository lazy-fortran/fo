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

/* The hook builds relocatably to share the checkout's cache, but its evidence
   key must use the caller's FO_CACHE_RELOCATABLE, as a resident lane of the
   same checkout does. The first prepare saves it ("-" unset, "+value"); a
   bounded hook's child inherits the saved value. */
#define CALLER_RELOCATABLE "FO_HOOK_CALLER_RELOCATABLE"

static int put_env(const char *name, const char *value) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    return _putenv_s(name, value ? value : "") ? EINVAL : 0;
#else
    if (!value) return unsetenv(name) ? errno : 0;
    return setenv(name, value, 1) ? errno : 0;
#endif
}

static int save_caller_relocatable(void) {
    char saved[4096];
    const char *value = getenv("FO_CACHE_RELOCATABLE");
    if (getenv(CALLER_RELOCATABLE)) return 0;
    if (value && strlen(value) + 2 > sizeof(saved)) return ENAMETOOLONG;
    snprintf(saved, sizeof(saved), value ? "+%s" : "-", value);
    return put_env(CALLER_RELOCATABLE, saved);
}

/* caller != 0 restores the caller's value for an identity capture; 0 sets
   the hook's own relocatable mode again. */
int fo_c_hook_identity_env(int caller) {
    const char *saved = getenv(CALLER_RELOCATABLE);
    if (!caller) return put_env("FO_CACHE_RELOCATABLE", "1");
    if (!saved || (saved[0] != '+' && saved[0] != '-')) return 0;
    return put_env("FO_CACHE_RELOCATABLE", saved[0] == '+' ? saved + 1 : NULL);
}

/* Hook-local policy; the process exits after checking the snapshot. An
   offline view builds against reused dependency roots and fetches nothing; the
   shared CMake view fetches what the staged commit pins. */
int fo_c_hook_prepare(const char *root, const char *view, int build_cap,
                      const char *reuse, int offline_view) {
    char source[8192], destination[8192], seconds[32];
    struct stat st;
    const char *prior = getenv("FO_CMAKE_ARGS");
    const char *offline = offline_view ? " -DFETCHCONTENT_FULLY_DISCONNECTED=ON"
                          " -DFETCHCONTENT_UPDATES_DISCONNECTED=ON" : "";
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
    /* The staged tree is materialized; private Git operations must not use
       the caller's repository, objects or commit index. Keep auth/config. */
    const char *git_context[] = {
        "GIT_DIR", "GIT_COMMON_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE",
        "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        "GIT_SHALLOW_FILE", "GIT_GRAFT_FILE"
    };
    for (size_t i = 0; i < sizeof(git_context) / sizeof(git_context[0]); ++i) {
#if defined(_WIN32) && !defined(__CYGWIN__)
        if (_putenv_s(git_context[i], "")) return EINVAL;
#else
        if (unsetenv(git_context[i])) return errno;
#endif
    }
    snprintf(seconds, sizeof(seconds), "%d", build_cap);
    status = save_caller_relocatable();
    if (status) return status;
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (_putenv_s("FO_CACHE_RELOCATABLE", "1") ||
        _putenv_s("FO_BUILD_TIMEOUT", seconds) ||
        (offline_view && _putenv_s("GIT_ALLOW_PROTOCOL", "file"))) return EINVAL;
#else
    if (setenv("FO_CACHE_RELOCATABLE", "1", 1) ||
        setenv("FO_BUILD_TIMEOUT", seconds, 1) ||
        (offline_view && setenv("GIT_ALLOW_PROTOCOL", "file", 1))) return errno;
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
