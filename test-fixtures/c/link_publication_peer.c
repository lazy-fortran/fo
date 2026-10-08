/* Replace a just-published executable before its publisher resumes. This
 * deterministic interposition models a competing producer's atomic rename,
 * without depending on the scheduler to hit the post-publication cache seam. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int rename(const char *source, const char *destination) {
    int (*real_rename)(const char *, const char *) = dlsym(RTLD_NEXT, "rename");
    const char *target = getenv("FO_TEST_PUBLISH_TARGET");
    const char *peer = getenv("FO_TEST_PUBLISH_PEER");
    const char *marker = getenv("FO_TEST_PUBLISH_MARKER");
    int rc = real_rename(source, destination);
    if (rc == 0 && target && peer && marker &&
        strcmp(destination, target) == 0 && strstr(source, "/.fo-link.tmp-") &&
        strcmp(strrchr(source, '/'), "/binary") == 0) {
        size_t bytes = strlen(destination) + 64;
        char *stage = malloc(bytes);
        if (!stage) _exit(91);
        snprintf(stage, bytes, "%s.peer-%ld", destination, (long)getpid());
        if (link(peer, stage) != 0) _exit(92);
        if (real_rename(stage, destination) != 0) _exit(93);
        free(stage);
        int fd = open(marker, O_WRONLY | O_CREAT | O_TRUNC, 0600);
        if (fd < 0) _exit(94);
        if (write(fd, "replaced\n", 9) != 9) _exit(95);
        if (close(fd) != 0) _exit(96);
    }
    return rc;
}
