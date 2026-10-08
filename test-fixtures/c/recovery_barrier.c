/* Narrow durability-operation barrier; Fortran owns the recovery oracle. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
static int published = 0;
static char session[128];
static void barrier(void) {
    const char *ready = getenv("RECOVERY_BARRIER_READY");
    const char *gate = getenv("RECOVERY_BARRIER_GATE");
    char text[64];
    int n = snprintf(text, sizeof(text), "%ld\n", (long)getpid());
    int fd = open(ready, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd < 0 || write(fd, text, (size_t)n) != n) _exit(120);
    close(fd);
    fd = open(gate, O_RDONLY);
    if (fd < 0) _exit(121);
    char byte;
    (void)read(fd, &byte, 1);
    close(fd);
}
int rename(const char *oldpath, const char *newpath) {
    int (*real_rename)(const char *, const char *) = dlsym(RTLD_NEXT, "rename");
    int result = real_rename(oldpath, newpath);
    const char *owner = getenv("RECOVERY_BARRIER_OWNER");
    if (result == 0 && owner && strcmp(owner, newpath) == 0) {
        FILE *file = fopen(owner, "r");
        if (!file || !fgets(session, sizeof(session), file)) _exit(122);
        fclose(file);
        session[strcspn(session, "\n")] = 0;
        published = 1;
    }
    return result;
}
int fsync(int fd) {
    int (*real_fsync)(int) = dlsym(RTLD_NEXT, "fsync");
    int result = real_fsync(fd);
    const char *owner = getenv("RECOVERY_BARRIER_OWNER");
    const char *mode = getenv("RECOVERY_BARRIER_MODE");
    if (result != 0 || !owner || !mode || !published) return result;
    char link[64], path[PATH_MAX], expected[PATH_MAX];
    snprintf(link, sizeof(link), "/proc/self/fd/%d", fd);
    ssize_t n = readlink(link, path, sizeof(path) - 1);
    if (n < 0) _exit(123);
    path[n] = 0;
    snprintf(expected, sizeof(expected), "%s", owner);
    char *slash = strrchr(expected, '/');
    if (!slash) _exit(124);
    *slash = 0;
    int hit = strcmp(mode, "owner") == 0 && strcmp(path, expected) == 0;
    if (strcmp(mode, "import") == 0) {
        snprintf(expected, sizeof(expected), "/%s/journal.jsonl", session);
        size_t a = strlen(path), b = strlen(expected);
        hit = a >= b && strcmp(path + a - b, expected) == 0;
    }
    if (hit) { published = 0; barrier(); }
    return result;
}
