/* Observe the actual install destination before publication, without a race. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#ifndef __APPLE__
#include <dlfcn.h>
#endif

static int observed_rename(const char *source, const char *destination) {
    const char *target = getenv("FO_TEST_INSTALL_TARGET");
    const char *marker = getenv("FO_TEST_INSTALL_MARKER");
    const char *failure = getenv("FO_TEST_INSTALL_FAILURE_TARGET");
    if (strstr(source, "/incoming/")) {
        if (target && marker && strcmp(destination, target) == 0) {
            const char *state = access(destination, F_OK) == 0 ?
                "present\n" : "missing\n";
            int fd = open(marker, O_WRONLY | O_CREAT | O_TRUNC, 0600);
            if (fd < 0) _exit(91);
            if (write(fd, state, strlen(state)) != (ssize_t)strlen(state)) _exit(92);
            if (close(fd) != 0) _exit(93);
        }
        if (failure && strcmp(destination, failure) == 0) {
            errno = EIO;
            return -1;
        }
    }
#ifdef __APPLE__
    return rename(source, destination);
#else
    int (*real_rename)(const char *, const char *) = dlsym(RTLD_NEXT, "rename");
    if (!real_rename) _exit(94);
    return real_rename(source, destination);
#endif
}

#ifdef __APPLE__
/* dyld reads replacement/original pairs from this section. Some SDKs omit
 * the convenience header for constructing this record. */
static const struct {
    int (*replacement)(const char *, const char *);
    int (*original)(const char *, const char *);
} install_interpose __attribute__((used, section("__DATA,__interpose"))) = {
    observed_rename, rename
};
#else
int rename(const char *source, const char *destination) {
    return observed_rename(source, destination);
}
#endif
