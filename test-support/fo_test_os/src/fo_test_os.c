#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fo_test_os_windows.inc"
#else
#if defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#elif defined(__linux__)
#define _GNU_SOURCE
#else
#define _POSIX_C_SOURCE 200809L
#endif
/* Test-only POSIX wrappers; process supervision and assertions live in Fortran. */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
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
#ifdef __APPLE__
#include <libproc.h>
#include <sys/proc.h>
#endif

int fo_test_host_is_linux(void) {
#ifdef __linux__
    return 1;
#else
    return 0;
#endif
}

static void child_setup_error(int fd, int operation) {
    /* Runs before stderr redirection in a forked child. Avoid stdio, allocation
       and strerror: write the operation and saved errno directly to its pipe. */
    int saved_error = errno;
    const char *name;
    char message[160], digits[16];
    size_t used = 0, count = 0, offset = 0;
    unsigned value = saved_error < 0 ? 0U : (unsigned)saved_error;
    const char *text = "fo test harness: child setup ";
    switch (operation) {
    case 1: name = "setpgid"; break;
    case 2: name = "default SIGPIPE"; break;
    case 3: name = "stdin dup2"; break;
    case 4: name = "stdout dup2"; break;
    case 5: name = "stderr dup2"; break;
    default: name = "unknown"; break;
    }
    while (*text) message[used++] = *text++;
    while (*name) message[used++] = *name++;
    text = " failed with errno ";
    while (*text) message[used++] = *text++;
    do { digits[count++] = (char)('0' + value % 10U); value /= 10U; } while (value);
    while (count) message[used++] = digits[--count];
    message[used++] = '\n';
    while (offset < used) {
        ssize_t written = write(fd, message + offset, used - offset);
        if (written < 0 && errno == EINTR) continue;
        if (written <= 0) break;
        offset += (size_t)written;
    }
    errno = saved_error;
}

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
#elif defined(__APPLE__)
    struct proc_bsdinfo info;
    int bytes = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, sizeof(info));
    if (bytes != sizeof(info)) return 0;
    return (uint64_t)info.pbi_start_tvsec * 1000000ULL +
           (uint64_t)info.pbi_start_tvusec;
#else
    (void)pid;
    return 0;
#endif
}

uint64_t fo_test_process_start_time(int pid) {
    return pid > 0 ? process_start_time((pid_t)pid) : 0;
}

int fo_test_private_mode(const char *path, int directory) {
    return chmod(path, directory ? 0700 : 0600);
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
#elif defined(__APPLE__)
    struct proc_bsdinfo info;
    int bytes = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, sizeof(info));
    if (bytes != sizeof(info)) return -1;
    record->pid = pid;
    record->parent = (pid_t)info.pbi_ppid;
    record->start_time = (uint64_t)info.pbi_start_tvsec * 1000000ULL +
                         (uint64_t)info.pbi_start_tvusec;
    return record->start_time == 0 ? -1 : 0;
#else
    (void)pid; (void)record;
    return -1;
#endif
}

int fo_test_collect_descendants(int root_pid, uint64_t root_start_time,
                                int *processes, uint64_t *start_times, int capacity) {
#if defined(__linux__) || defined(__APPLE__)
    if (root_pid <= 0 || root_start_time == 0 || processes == NULL ||
        start_times == NULL || capacity <= 0) return -1;
    if (process_start_time((pid_t)root_pid) != root_start_time) return -2;
    size_t allocated = 4096;
    size_t count = 0;
    fo_test_process_record *records = calloc(allocated, sizeof(*records));
    if (records == NULL) return -3;
#if defined(__linux__)
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
#else
    int bytes = proc_listallpids(NULL, 0);
    if (bytes <= 0 || (size_t)bytes > SIZE_MAX / sizeof(pid_t) - 1024) {
        free(records); return -3;
    }
    size_t pid_capacity = (size_t)bytes + 1024;
    if (pid_capacity > INT_MAX / sizeof(pid_t)) { free(records); return -3; }
    pid_t *pids = calloc(pid_capacity, sizeof(*pids));
    if (pids == NULL) { free(records); return -3; }
    int listed = proc_listallpids(pids, (int)(pid_capacity * sizeof(*pids)));
    if (listed <= 0 || (size_t)listed >= pid_capacity) {
        free(pids); free(records); return -3;
    }
    if ((size_t)listed > allocated) {
        fo_test_process_record *grown = realloc(records, (size_t)listed * sizeof(*records));
        if (grown == NULL) { free(pids); free(records); return -3; }
        records = grown;
    }
    for (int i = 0; i < listed; ++i) {
        fo_test_process_record record;
        if (pids[i] <= 0 || read_process_record(pids[i], &record) != 0) continue;
        records[count++] = record;
    }
    free(pids);
#endif
    /* Refuse a tree observed after the root's recorded identity disappeared. */
    if (process_start_time((pid_t)root_pid) != root_start_time) {
        free(records); return -2;
    }
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

int fo_test_os_initialize(void) { return 162; }

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
#elif defined(__APPLE__)
    struct proc_bsdinfo info;
    int bytes = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, sizeof(info));
    if (bytes != sizeof(info)) return 0;
    return info.pbi_status != SZOMB;
#else
    return 1;
#endif
}

int fo_test_set_nonblocking(int descriptor) {
    int flags = fcntl(descriptor, F_GETFL, 0);
    return flags < 0 ? -1 : fcntl(descriptor, F_SETFL, flags | O_NONBLOCK);
}

int fo_test_ignore_sigpipe(void) { return signal(SIGPIPE, SIG_IGN) == SIG_ERR ? -1 : 0; }

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

static int wait_posix_child(pid_t process, int *status, int options) {
    pid_t waited;
    do { waited = waitpid(process, status, options); } while (waited < 0 && errno == EINTR);
    return (int)waited;
}

int fo_test_close(int descriptor) { return close(descriptor); }
int fo_test_host_is_windows(void) { return 0; }
int fo_test_pipe_cloexec(int descriptors[2]);
int fo_test_pipe_mode(int descriptors[2], int parent_reads) {
    (void)parent_reads;
    return fo_test_pipe_cloexec(descriptors);
}

/* Decode the actual OS result at this boundary, not in Fortran. */
int fo_test_wait_child(int process, int *exit_code, int *term_signal, int blocking) {
    int status = 0;
    int waited = wait_posix_child(process, &status, blocking ? 0 : WNOHANG);
    if (waited <= 0) return waited;
    *exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    *term_signal = WIFSIGNALED(status) ? WTERMSIG(status) : 0;
    return 1;
}

int fo_test_spawn_unowned(char *const argv[], const char *cwd) {
    pid_t child = fork();
    if (child != 0) return child < 0 ? -1 : (int)child;
    if (chdir(cwd)) _exit(126);
    execvp(argv[0], argv); _exit(127);
}
int fo_test_ignore_term(void) { return signal(SIGTERM, SIG_IGN) == SIG_ERR ? -1 : 0; }
int fo_test_terminate_self(void) { return kill(getpid(), SIGTERM); }
int fo_test_in_job(void) { errno = ENOTSUP; return -1; }
int fo_test_getpid(void) { return (int)getpid(); }
int fo_test_is_regular_file(const char *path) {
    struct stat value;
    return stat(path, &value) == 0 && S_ISREG(value.st_mode);
}
int fo_test_chdir(const char *path) { return chdir(path); }
int fo_test_setenv(const char *name, const char *value, int overwrite) { return setenv(name, value, overwrite); }
int fo_test_mkfifo(const char *path, int mode) { return mkfifo(path, (mode_t)mode); }
int fo_test_open_write(const char *path) { return open(path, O_CREAT | O_TRUNC | O_WRONLY, 0600); }
int fo_test_sleep_ms(int ms) { return ms < 0 ? -1 : poll(NULL, 0, ms) < 0 && errno != EINTR ? -1 : 0; }
int fo_test_cancel(int pid) { return kill(-pid, SIGKILL); }
int fo_test_signal(int pid, int signal_number) { return kill(pid, signal_number); }
int fo_test_spawn(char *const[], char *const[], const char *);
int fo_test_spawn_sentinel(void) {
    char *const args[] = {"/bin/sleep", "3600", NULL};
    char *const env[] = {NULL};
    return fo_test_spawn(args, env, ".");
}

int fo_test_spawn_redirected(char *const arguments[], char *const environment[],
        const char *directory, const int input[2], const int output[2],
        const int diagnostic[2], int delay_ms) {
    pid_t child = fork();
    if (child != 0) return child < 0 ? -1 : (int)child;
    if (delay_ms > 0) (void)poll(NULL, 0, delay_ms);
    if (setpgid(0, 0) != 0) {
        child_setup_error(diagnostic[1], 1); _exit(126);
    }
    if (signal(SIGPIPE, SIG_DFL) == SIG_ERR) {
        child_setup_error(diagnostic[1], 2); _exit(126);
    }
    const int from[] = {input[0], output[1], diagnostic[1]};
    for (int i = 0; i < 3; ++i) {
        if (dup2(from[i], i) < 0) {
            child_setup_error(diagnostic[1], 3 + i); _exit(126);
        }
    }
    for (int i = 0; i < 2; ++i) {
        close(input[i]); close(output[i]); close(diagnostic[i]);
    }
    if (chdir(directory) != 0) _exit(125);
    for (size_t i = 0; environment && environment[i]; ++i) {
        char *entry = strdup(environment[i]);
        if (!entry) _exit(125);
        char *equals = strchr(entry, '=');
        if (!equals || equals == entry) _exit(125);
        *equals = '\0';
        if (setenv(entry, equals + 1, 1) != 0) _exit(125);
        free(entry);
    }
    execvp(arguments[0], arguments);
    _exit(127);
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

int fo_test_pipe_cloexec(int descriptors[2]) {
    if (descriptors == NULL) return -1;
#if defined(__linux__)
    return pipe2(descriptors, O_CLOEXEC);
#else
    if (pipe(descriptors) != 0) return -1;
    for (int i = 0; i < 2; i++) {
        int flags = fcntl(descriptors[i], F_GETFD);
        if (flags < 0 || fcntl(descriptors[i], F_SETFD, flags | FD_CLOEXEC) != 0) {
            int error = errno;
            close(descriptors[0]);
            close(descriptors[1]);
            errno = error;
            return -1;
        }
    }
    return 0;
#endif
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

/* A deliberately unowned same-group process for containment tests. */
int fo_test_spawn_same_group_sentinel(void) {
    pid_t child = fork();
    if (child < 0) return -1;
    if (child == 0) {
        for (;;) pause();
    }
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

#endif

#include "fo_test_os_mcp.inc"

/* Independent child-side CRT/libc producer for the binary pipe oracle. */
int fo_test_copy_stdin(void) {
    char buffer[4096];
    for (;;) {
#if defined(_WIN32) && !defined(__CYGWIN__)
        int count = _read(0, buffer, sizeof(buffer));
#else
        ssize_t count = read(0, buffer, sizeof(buffer));
#endif
        if (!count) return 0;
        if (count < 0) { if (errno == EINTR) continue; return 125; }
        size_t offset = 0;
        while (offset < (size_t)count) {
#if defined(_WIN32) && !defined(__CYGWIN__)
            int written = _write(1, buffer + offset, (unsigned)((size_t)count - offset));
#else
            ssize_t written = write(1, buffer + offset, (size_t)count - offset);
#endif
            if (written < 0 && errno == EINTR) continue;
            if (written <= 0) return 125;
            offset += (size_t)written;
        }
    }
}
int fo_test_silence_output(void) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    int fd = open_path("NUL", _O_WRONLY);
    if (fd < 0) return -1;
    int result = _dup2(fd, 1) || _dup2(fd, 2) ? -1 : 0;
    _close(fd);
#else
    int fd = open("/dev/null", O_WRONLY);
    if (fd < 0) return -1;
    int result = dup2(fd, 1) < 0 || dup2(fd, 2) < 0 ? -1 : 0;
    close(fd);
#endif
    return result;
}
