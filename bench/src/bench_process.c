#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

int64_t fo_bench_monotonic_ns(void) {
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    return (int64_t)now.tv_sec * INT64_C(1000000000) + now.tv_nsec;
}

int fo_bench_setenv(const char *name, const char *value) {
    return setenv(name, value, 1);
}

int fo_bench_mkdir(const char *path) {
    if (mkdir(path, 0700) == 0 || errno == EEXIST) return 0;
    return errno;
}

int fo_bench_getpid(void) { return (int)getpid(); }

int fo_bench_create_temp(const char *prefix, char *output, size_t capacity) {
    int n = snprintf(output, capacity, "%sXXXXXX", prefix);
    if (n < 0) return errno ? errno : EINVAL;
    if ((size_t)n >= capacity) return ENAMETOOLONG;
    if (!mkdtemp(output)) return errno;
    return 0;
}

static int remove_entry(const char *path) {
    struct stat info;
    if (lstat(path, &info) != 0) return errno;
    if (S_ISDIR(info.st_mode)) {
        DIR *directory = opendir(path);
        struct dirent *entry;
        int result = 0;
        if (!directory) return errno;
        while ((entry = readdir(directory)) != NULL) {
            char child[4096];
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;
            int n = snprintf(child, sizeof(child), "%s/%s", path, entry->d_name);
            if (n < 0 || (size_t)n >= sizeof(child)) { result = ENAMETOOLONG; break; }
            result = remove_entry(child);
            if (result != 0) break;
        }
        closedir(directory);
        if (result != 0) return result;
        if (rmdir(path) != 0) return errno;
        return 0;
    }
    if (unlink(path) != 0) return errno;
    return 0;
}

int fo_bench_remove_owned_temp(const char *path) {
    static const char *const prefixes[] = {
        "/var/tmp/fo-bench-cache-", "/var/tmp/fo-bench-fixture-"
    };
    int allowed = 0;
    for (size_t i = 0; i < sizeof(prefixes) / sizeof(prefixes[0]); ++i) {
        size_t length = strlen(prefixes[i]);
        if (strncmp(path, prefixes[i], length) == 0 && path[length] != '\0' &&
            strchr(path + length, '/') == NULL && strstr(path + length, "..") == NULL) {
            allowed = 1;
            break;
        }
    }
    if (!allowed) return EPERM;
    return remove_entry(path);
}

int fo_bench_touch(const char *path) {
    struct timespec now[2];
    if (clock_gettime(CLOCK_REALTIME, &now[0]) != 0) return errno;
    now[1] = now[0];
    if (utimensat(AT_FDCWD, path, now, 0) != 0) return errno;
    return 0;
}

int fo_bench_self_path(char *path, size_t capacity) {
    if (capacity < 2) return EINVAL;
#if defined(__linux__)
    int n = snprintf(path, capacity, "/proc/%ld/exe", (long)getpid());
    if (n < 0) return errno;
    if ((size_t)n >= capacity) return ENAMETOOLONG;
    return 0;
#elif defined(__APPLE__)
    uint32_t size = (uint32_t)capacity;
    if (_NSGetExecutablePath(path, &size) != 0) return ENAMETOOLONG;
    return 0;
#else
    return ENOTSUP;
#endif
}

void fo_bench_sleep_ms(int milliseconds) {
    struct timespec delay;
    if (milliseconds < 0) return;
    delay.tv_sec = milliseconds / 1000;
    delay.tv_nsec = (long)(milliseconds % 1000) * 1000000L;
    while (nanosleep(&delay, &delay) != 0 && errno == EINTR) {}
}

/* argv tokens use byte 1 separators; direct children are reaped. */
int fo_bench_run_argv(const char *cwd, const char *argv_blob, int argc,
                      const char *output_path, int timeout_seconds) {
    char **argv = calloc((size_t)argc + 1, sizeof(char *));
    char *packed = strdup(argv_blob), *cursor;
    int status = 0, fd, waited = 0;
    pid_t pid;
    struct timespec pause = {0, 10000000};
    if (!argv || !packed) { free(argv); free(packed); return 125; }
    cursor = packed;
    for (int i = 0; i < argc; ++i) {
        argv[i] = (char *)cursor;
        char *separator = strchr(cursor, 1);
        if (!separator) { free(argv); free(packed); return 125; }
        *separator = '\0';
        cursor = separator + 1;
    }
    pid = fork();
    if (pid < 0) { free(argv); free(packed); return 125; }
    if (pid == 0) {
        (void)setpgid(0, 0);
        if (chdir(cwd) != 0) _exit(126);
        fd = open(output_path, O_WRONLY | O_CREAT | O_TRUNC, 0600);
        if (fd < 0) _exit(126);
        (void)dup2(fd, STDOUT_FILENO);
        (void)dup2(fd, STDERR_FILENO);
        if (fd > STDERR_FILENO) close(fd);
        execvp(argv[0], argv);
        _exit(errno == ENOENT ? 127 : 126);
    }
    (void)setpgid(pid, pid);
    free(argv);
    free(packed);
    for (int elapsed = 0; ; elapsed += 10) {
        pid_t result = waitpid(pid, &status, WNOHANG);
        if (result == pid) break;
        if (result < 0 && errno != EINTR) return 125;
        if (timeout_seconds > 0 && elapsed >= timeout_seconds * 1000) {
            (void)kill(-pid, SIGTERM);
            for (int grace = 0; grace < 200; grace += 10) {
                result = waitpid(pid, &status, WNOHANG);
                if (result == pid) { waited = 1; break; }
                (void)nanosleep(&pause, NULL);
            }
            if (!waited) {
                (void)kill(-pid, SIGKILL);
                while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
            }
            return 124;
        }
        (void)nanosleep(&pause, NULL);
    }
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 125;
}
