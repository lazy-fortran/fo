#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

static int owner_published;
static long owner_pid;
static char session[128];

static void wait_at_barrier(void) {
    const char *ready = getenv("RECOVERY_BARRIER_READY");
    const char *gate = getenv("RECOVERY_BARRIER_GATE");
    char text[64];
    int length = snprintf(text, sizeof(text), "%ld\n", (long)getpid());
    int fd = open(ready, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd < 0 || write(fd, text, (size_t)length) != length) _exit(120);
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
    if (result == 0 && owner != NULL && strcmp(owner, newpath) == 0) {
        FILE *file = fopen(owner, "r");
        char pid_text[64];
        if (file == NULL || fgets(session, sizeof(session), file) == NULL ||
            fgets(pid_text, sizeof(pid_text), file) == NULL) _exit(122);
        fclose(file);
        session[strcspn(session, "\n")] = 0;
        owner_pid = strtol(pid_text, NULL, 10);
        owner_published = owner_pid == (long)getpid();
    }
    return result;
}

int fsync(int fd) {
    int (*real_fsync)(int) = dlsym(RTLD_NEXT, "fsync");
    int result = real_fsync(fd);
    const char *owner = getenv("RECOVERY_BARRIER_OWNER");
    const char *mode = getenv("RECOVERY_BARRIER_MODE");
    if (result != 0 || owner == NULL || mode == NULL || !owner_published ||
        owner_pid != (long)getpid()) return result;

    char link[64], path[PATH_MAX], expected[PATH_MAX];
    (void)snprintf(link, sizeof(link), "/proc/self/fd/%d", fd);
    ssize_t length = readlink(link, path, sizeof(path) - 1);
    if (length < 0) _exit(123);
    path[length] = 0;
    if (snprintf(expected, sizeof(expected), "%s", owner) >= (int)sizeof(expected)) {
        _exit(124);
    }
    char *slash = strrchr(expected, '/');
    if (slash == NULL) _exit(125);
    *slash = 0;

    int hit = strcmp(mode, "owner") == 0 && strcmp(path, expected) == 0;
    if (strcmp(mode, "import") == 0) {
        if (snprintf(expected, sizeof(expected), "/%s/journal.jsonl", session) >=
            (int)sizeof(expected)) _exit(126);
        size_t path_length = strlen(path), suffix_length = strlen(expected);
        hit = path_length >= suffix_length &&
            strcmp(path + path_length - suffix_length, expected) == 0;
    }
    if (hit) {
        owner_published = 0;
        wait_at_barrier();
    }
    return result;
}
