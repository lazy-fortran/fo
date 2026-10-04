#if defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#else
#define _POSIX_C_SOURCE 200809L
#endif
/* Test-only POSIX wrappers; process supervision and assertions live in Fortran. */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static uint64_t process_start_time(pid_t pid) {
#if defined(__linux__)
    char path[64], line[4096];
    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    FILE *stream = fopen(path, "r");
    if (stream == NULL) return 0;
    char *read_result = fgets(line, sizeof(line), stream);
    fclose(stream);
    if (read_result == NULL) return 0;
    char *end = strrchr(line, ')');
    if (end == NULL || end[1] != ' ') return 0;
    char *save = NULL;
    char *field = strtok_r(end + 2, " ", &save);
    for (int number = 3; field != NULL && number < 22; ++number) {
        field = strtok_r(NULL, " ", &save);
    }
    if (field == NULL) return 0;
    char *tail = NULL;
    unsigned long long value = strtoull(field, &tail, 10);
    return tail == field || (*tail != '\n' && *tail != '\0') ? 0 : (uint64_t)value;
#else
    (void)pid;
    return 0;
#endif
}

uint64_t fo_test_process_start_time(int pid) {
    return pid > 0 ? process_start_time((pid_t)pid) : 0;
}

typedef struct {
    pid_t pid;
    pid_t parent;
    uint64_t start_time;
} fo_test_process_record;

static int read_process_record(pid_t pid, fo_test_process_record *record) {
#if defined(__linux__)
    char path[64], line[4096];
    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    FILE *stream = fopen(path, "r");
    if (stream == NULL) return -1;
    char *read_result = fgets(line, sizeof(line), stream);
    fclose(stream);
    if (read_result == NULL) return -1;
    char *end = strrchr(line, ')');
    if (end == NULL || end[1] != ' ') return -1;
    char *save = NULL;
    char *field = strtok_r(end + 2, " ", &save);
    long parent = -1;
    uint64_t start_time = 0;
    for (int number = 3; field != NULL && number <= 22; ++number) {
        if (number == 4) parent = strtol(field, NULL, 10);
        if (number == 22) start_time = (uint64_t)strtoull(field, NULL, 10);
        if (number < 22) field = strtok_r(NULL, " ", &save);
    }
    if (parent < 0 || start_time == 0) return -1;
    record->pid = pid;
    record->parent = (pid_t)parent;
    record->start_time = start_time;
    return 0;
#else
    (void)pid; (void)record;
    return -1;
#endif
}

int fo_test_collect_descendants(int root_pid, uint64_t root_start_time,
                                int *processes, uint64_t *start_times, int capacity) {
#if defined(__linux__)
    if (root_pid <= 0 || root_start_time == 0 || processes == NULL ||
        start_times == NULL || capacity <= 0) return -1;
    if (process_start_time((pid_t)root_pid) != root_start_time) return -2;
    size_t allocated = 4096;
    size_t count = 0;
    fo_test_process_record *records = calloc(allocated, sizeof(*records));
    if (records == NULL) return -3;
    DIR *directory = opendir("/proc");
    if (directory == NULL) { free(records); return -3; }
    struct dirent *entry;
    while ((entry = readdir(directory)) != NULL) {
        char *end = NULL;
        long value = strtol(entry->d_name, &end, 10);
        if (end == entry->d_name || *end != '\0' || value <= 0 || value > INT32_MAX) continue;
        fo_test_process_record record;
        if (read_process_record((pid_t)value, &record) != 0) continue;
        if (count == allocated) {
            allocated *= 2;
            fo_test_process_record *grown = realloc(records, allocated * sizeof(*records));
            if (grown == NULL) { closedir(directory); free(records); return -3; }
            records = grown;
        }
        records[count++] = record;
    }
    closedir(directory);
    int used = 0;
    int frontier = 0;
    processes[used] = root_pid;
    start_times[used++] = root_start_time;
    while (frontier < used) {
        pid_t parent = (pid_t)processes[frontier++];
        for (size_t i = 0; i < count; ++i) {
            if (records[i].parent != parent) continue;
            if (used >= capacity) { free(records); return -4; }
            processes[used] = (int)records[i].pid;
            start_times[used++] = records[i].start_time;
        }
    }
    free(records);
    return used - 1;
#else
    (void)root_pid; (void)root_start_time; (void)processes;
    (void)start_times; (void)capacity;
    return -5;
#endif
}

int fo_test_signal_identity(int pid, uint64_t start_time, int signal_number) {
    if (pid <= 0 || start_time == 0) return -1;
    if (process_start_time((pid_t)pid) != start_time) return -2;
    return kill((pid_t)pid, signal_number);
}

int fo_test_link_probe(void) { return 162; }

int fo_test_mkdtemp(char *pattern) {
    return mkdtemp(pattern) == NULL ? -1 : 0;
}

int fo_test_mkdirs(const char *path) {
    char *copy = strdup(path);
    if (copy == NULL || copy[0] == '\0') { free(copy); return -1; }
    size_t length = strlen(copy);
    while (length > 1 && copy[length - 1] == '/') copy[--length] = '\0';
    for (size_t i = 1; i <= length; ++i) {
        if (copy[i] != '/' && copy[i] != '\0') continue;
        char saved = copy[i];
        copy[i] = '\0';
        if (mkdir(copy, 0777) != 0 && errno != EEXIST) { free(copy); return -1; }
        copy[i] = saved;
    }
    free(copy);
    return 0;
}

static int remove_one(const char *path) {
    struct stat info;
    if (lstat(path, &info) != 0) return errno == ENOENT ? 0 : -1;
    if (!S_ISDIR(info.st_mode) || S_ISLNK(info.st_mode)) return unlink(path);
    /* Fixture snapshots deliberately remove directory write permission. */
    if (chmod(path, info.st_mode | S_IRUSR | S_IWUSR | S_IXUSR) != 0) return -1;
    DIR *directory = opendir(path);
    if (directory == NULL) return -1;
    struct dirent *entry;
    int result = 0;
    while ((entry = readdir(directory)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
        size_t size = strlen(path) + strlen(entry->d_name) + 2;
        char *child = malloc(size);
        if (child == NULL) { result = -1; break; }
        snprintf(child, size, "%s/%s", path, entry->d_name);
        result = remove_one(child);
        free(child);
        if (result != 0) break;
    }
    if (closedir(directory) != 0) result = -1;
    if (result == 0 && rmdir(path) != 0) result = -1;
    return result;
}

int fo_test_remove_tree(const char *path) { return remove_one(path); }
int fo_test_unlink(const char *path) { return unlink(path) == 0 || errno == ENOENT ? 0 : -1; }
int fo_test_rename(const char *source, const char *target) { return rename(source, target); }
int fo_test_symlink(const char *target, const char *link_path) { return symlink(target, link_path); }
int fo_test_getcwd(char *buffer, size_t size) { return getcwd(buffer, size) == NULL ? -1 : 0; }
int fo_test_mode_bits(const char *path) {
    struct stat info;
    return stat(path, &info) == 0 ? (int)(info.st_mode & 07777) : -1;
}

int64_t fo_test_monotonic_ms(void) {
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    return (int64_t)now.tv_sec * 1000 + (int64_t)now.tv_nsec / 1000000;
}

int fo_test_release_fifo(const char *path, int timeout_ms) {
    int64_t deadline = fo_test_monotonic_ms() + timeout_ms;
    int descriptor;
    struct timespec pause = {0, 20000000};
    do {
        descriptor = open(path, O_WRONLY | O_NONBLOCK);
        if (descriptor >= 0) break;
        if (errno != ENXIO && errno != EINTR) return -1;
        nanosleep(&pause, NULL);
    } while (fo_test_monotonic_ms() < deadline);
    if (descriptor < 0) return -1;
    struct sigaction ignored = {0}, previous;
    ignored.sa_handler = SIG_IGN;
    sigemptyset(&ignored.sa_mask);
    if (sigaction(SIGPIPE, &ignored, &previous) != 0) { close(descriptor); return -1; }
    ssize_t written = write(descriptor, "x", 1);
    int result = written == 1 ? 0 : -1;
    if (sigaction(SIGPIPE, &previous, NULL) != 0) result = -1;
    if (close(descriptor) != 0) result = -1;
    return result;
}

int fo_test_process_running(int pid) {
    if (pid <= 0 || kill(pid, 0) != 0) return 0;
#if defined(__linux__)
    char path[64], line[4096];
    snprintf(path, sizeof(path), "/proc/%d/stat", pid);
    FILE *stream = fopen(path, "r");
    if (stream == NULL) return 0;
    char *read_result = fgets(line, sizeof(line), stream);
    fclose(stream);
    if (read_result == NULL) return 0;
    char *end = strrchr(line, ')');
    if (end == NULL || end[1] != ' ') return 0;
    return end[2] != 'Z' && end[2] != 'X';
#else
    return 1;
#endif
}

int fo_test_set_nonblocking(int descriptor) {
    int flags = fcntl(descriptor, F_GETFL, 0);
    return flags < 0 ? -1 : fcntl(descriptor, F_SETFL, flags | O_NONBLOCK);
}

int fo_test_ignore_sigpipe(void) { return signal(SIGPIPE, SIG_IGN) == SIG_ERR ? -1 : 0; }
int fo_test_default_sigpipe(void) { return signal(SIGPIPE, SIG_DFL) == SIG_ERR ? -1 : 0; }

int64_t fo_test_read(int descriptor, char *buffer, size_t capacity) {
    ssize_t count = read(descriptor, buffer, capacity);
    if (count >= 0) return (int64_t)count;
    return errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? -2 : -1;
}

int64_t fo_test_write(int descriptor, const char *buffer, size_t length) {
    ssize_t count = write(descriptor, buffer, length);
    if (count >= 0) return (int64_t)count;
    if (errno == EPIPE) return -3;
    return errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? -2 : -1;
}

int fo_test_poll(struct pollfd *descriptors, size_t count, int timeout_ms) {
    int ready = poll(descriptors, (nfds_t)count, timeout_ms);
    return ready < 0 && errno == EINTR ? 0 : ready;
}

int fo_test_waitpid(pid_t process, int *status, int options) {
    pid_t waited;
    do { waited = waitpid(process, status, options); } while (waited < 0 && errno == EINTR);
    return (int)waited;
}

int fo_test_spawn_capture(const char *const *arguments, const char *cwd,
        const char *stdout_path, const char *stderr_path) {
    pid_t child = fork();
    if (child != 0) return child < 0 ? -1 : (int)child;
    if (setpgid(0, 0) != 0) _exit(126);
    int out = open(stdout_path, O_CREAT | O_TRUNC | O_WRONLY, 0600);
    int err = open(stderr_path, O_CREAT | O_TRUNC | O_WRONLY, 0600);
    if (out < 0 || err < 0 || dup2(out, STDOUT_FILENO) < 0 ||
            dup2(err, STDERR_FILENO) < 0 || chdir(cwd) != 0) _exit(126);
    close(out);
    close(err);
    execvp(arguments[0], (char *const *)arguments);
    _exit(127);
}

int fo_test_silence_output(void) {
    int descriptor = open("/dev/null", O_WRONLY);
    if (descriptor < 0) return -1;
    int result = dup2(descriptor, STDOUT_FILENO) < 0 ||
        dup2(descriptor, STDERR_FILENO) < 0 ? -1 : 0;
    close(descriptor);
    return result;
}

int fo_test_open_fds(void) {
    DIR *directory = opendir("/proc/self/fd");
    if (directory == NULL) directory = opendir("/dev/fd");
    if (directory == NULL) return -1;
    int count = 0;
    struct dirent *entry;
    while ((entry = readdir(directory)) != NULL) {
        if (entry->d_name[0] >= '0' && entry->d_name[0] <= '9') ++count;
    }
    if (closedir(directory) != 0) return -1;
    return count - 1; /* Exclude this directory's own descriptor. */
}

int fo_test_spawn(char *const arguments[], char *const environment[], const char *directory) {
    pid_t child = fork();
    if (child < 0) return -1;
    if (child == 0) {
        int null_output = open("/dev/null", O_WRONLY);
        if (null_output >= 0) {
            (void)dup2(null_output, STDOUT_FILENO);
            (void)dup2(null_output, STDERR_FILENO);
            if (null_output > STDERR_FILENO) close(null_output);
        }
        (void)setpgid(0, 0);
        for (size_t i = 0; environment[i] != NULL; ++i) {
            char *separator = strchr(environment[i], '=');
            if (separator == NULL) continue;
            size_t name_size = (size_t)(separator - environment[i]);
            char *name = strndup(environment[i], name_size);
            if (name == NULL) _exit(126);
            int changed = setenv(name, separator + 1, 1);
            free(name);
            if (changed != 0) _exit(126);
        }
        if (chdir(directory) != 0) _exit(126);
        execvp(arguments[0], arguments);
        _exit(127);
    }
    (void)setpgid(child, child);
    return (int)child;
}

int fo_test_signal_group(int process, int signal_number) {
    return kill(-process, signal_number);
}

int fo_test_wait_nonblocking(int process, int *status) {
    pid_t waited;
    do { waited = waitpid(process, status, WNOHANG); } while (waited < 0 && errno == EINTR);
    if (waited == 0) return 999;
    if (waited < 0) return -999;
    if (WIFEXITED(*status)) return WEXITSTATUS(*status);
    if (WIFSIGNALED(*status)) return -WTERMSIG(*status);
    return -998;
}

int fo_test_wait_blocking(int process) {
    int status;
    pid_t waited;
    do { waited = waitpid(process, &status, 0); } while (waited < 0 && errno == EINTR);
    if (waited != process) return -999;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return -WTERMSIG(status);
    return -998;
}

int fo_test_spawn_heartbeat(const char *path, const char *directory) {
    pid_t child = fork();
    if (child < 0) return -1;
    if (child == 0) {
        (void)setpgid(0, 0);
        if (chdir(directory) != 0) _exit(126);
        int output = open(path, O_WRONLY | O_CREAT | O_APPEND, 0600);
        if (output < 0) _exit(126);
        const struct timespec interval = {0, 20000000};
        for (;;) {
            if (write(output, ".", 1) != 1) _exit(126);
            (void)nanosleep(&interval, NULL);
        }
    }
    (void)setpgid(child, child);
    return (int)child;
}
