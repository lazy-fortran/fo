#define _GNU_SOURCE

#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef __linux__
#include <stddef.h>
#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <sys/syscall.h>
#include <sys/prctl.h>
#endif
#ifdef __APPLE__
#include <libproc.h>
#endif
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifdef __linux__
#define FO_MONITOR_FD 9
#define FO_MONITOR_ARG "\037fo-process-monitor-v1"
#endif

struct async_process {
    pid_t pid;
    pid_t session;
    int owns_session;
    uint64_t start_identity;
    int leader_done;
    int exitcode;
    struct async_process *next;
};

static struct async_process *async_processes = NULL;

static int heartbeats_suppressed = 0;

static int has_text(const char *text) { return text != NULL && text[0] != '\0'; }

static int timespec_at_or_after(const struct timespec *lhs,
                                const struct timespec *rhs) {
    return lhs->tv_sec > rhs->tv_sec ||
           (lhs->tv_sec == rhs->tv_sec && lhs->tv_nsec >= rhs->tv_nsec);
}

static void emit_heartbeat(const char *log_file) {
    static const char message[] =
        "fo: command still running; output is captured\n";
    int fd;

    write(STDERR_FILENO, message, sizeof(message) - 1);
    if (!has_text(log_file) || strcmp(log_file, "/dev/null") == 0) return;
    fd = open(log_file, O_WRONLY | O_CREAT | O_APPEND, 0666);
    if (fd < 0) return;
    write(fd, message, sizeof(message) - 1);
    close(fd);
}

static void emit_spawn_error(const char *log_file, const char *operation,
                             int error_number) {
    char message[256];
    int fd, n;

    n = snprintf(message, sizeof(message), "fo: %s failed: %s\n", operation,
                 strerror(error_number));
    if (n <= 0) return;
    if (n >= (int)sizeof(message)) n = (int)sizeof(message) - 1;
    if (has_text(log_file) && strcmp(log_file, "/dev/null") != 0) {
        fd = open(log_file, O_WRONLY | O_CREAT | O_APPEND, 0666);
        if (fd >= 0) {
            write(fd, message, (size_t)n);
            close(fd);
        }
    }
}

static void add_seconds(struct timespec *ts, int seconds) {
    ts->tv_sec += seconds;
}

static int milliseconds_until(const struct timespec *now,
                              const struct timespec *target) {
    long long nanoseconds =
        (long long)(target->tv_sec - now->tv_sec) * 1000000000LL +
        (long long)(target->tv_nsec - now->tv_nsec);
    long long milliseconds;

    if (nanoseconds <= 0) return 0;
    milliseconds = (nanoseconds + 999999LL) / 1000000LL;
    return milliseconds > INT_MAX ? INT_MAX : (int)milliseconds;
}

static void sleep_ms(int ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (long)(ms % 1000) * 1000000L;
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {
    }
}

struct path_list {
    char **items;
    size_t n;
    size_t cap;
};

static int path_list_add(struct path_list *list, const char *path) {
    char **next;

    if (list->n == list->cap) {
        size_t next_cap = list->cap == 0 ? 64 : list->cap * 2;
        next = realloc(list->items, next_cap * sizeof(char *));
        if (next == NULL) return 1;
        list->items = next;
        list->cap = next_cap;
    }
    list->items[list->n] = strdup(path);
    if (list->items[list->n] == NULL) return 1;
    list->n++;
    return 0;
}

static int path_cmp(const void *lhs, const void *rhs) {
    const char *const *a = lhs;
    const char *const *b = rhs;
    return strcmp(*a, *b);
}

static void path_list_free(struct path_list *list) {
    size_t i;

    for (i = 0; i < list->n; i++) free(list->items[i]);
    free(list->items);
    list->items = NULL;
    list->n = 0;
    list->cap = 0;
}

static int is_project_root(const char *dir) {
    char path[4096];
    struct stat st;
    snprintf(path, sizeof(path), "%s/fpm.toml", dir);
    if (stat(path, &st) == 0) return 1;
    return 0;
}

static int skip_dir_name(const char *name, int is_proj_root, int depth) {
    if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) return 1;
    /* Hidden directories never hold project Fortran sources: .git, .venv,
       .cache, .claude, .tox, .mypy_cache, ... Skipping any dot-directory
       matches the ripgrep/fd default and avoids descending into vendored
       Python virtualenvs whose dependencies ship .f90 fixtures. */
    if (name[0] == '.') return 1;
    /* Non-hidden vendored / environment trees. */
    if (strcmp(name, "node_modules") == 0) return 1;
    if (strcmp(name, "venv") == 0) return 1;
    if (strcmp(name, "__pycache__") == 0) return 1;
    if (strcmp(name, "site-packages") == 0) return 1;
    /* Build/output trees are never source roots for the current project. */
    if (strcmp(name, "_deps") == 0) return 1;
    if (strcmp(name, "dependencies") == 0) return 1;
    if (strcmp(name, "deps-src") == 0) return 1;
    if (is_proj_root && depth == 0 && strcmp(name, "build") == 0) return 1;
    if (is_proj_root && depth == 0 && strncmp(name, "build", 5) == 0) return 1;
    if (is_proj_root && depth == 0 && strcmp(name, "SRC") == 0) return 1;
    return 0;
}

static int has_fortran_ext(const char *path) {
    size_t n = strlen(path);
    if (n >= 4 && strcmp(path + n - 4, ".f90") == 0) return 1;
    if (n >= 4 && strcmp(path + n - 4, ".F90") == 0) return 1;
    if (n >= 2 && strcmp(path + n - 2, ".f") == 0) return 1;
    if (n >= 2 && strcmp(path + n - 2, ".F") == 0) return 1;
    return 0;
}

static int scan_sources_recursive(const char *dir, struct path_list *list,
                                  int required, int is_proj_root, int depth) {
    DIR *handle;
    struct dirent *entry;

    handle = opendir(dir);
    if (handle == NULL) return required ? 1 : 0;

    while ((entry = readdir(handle)) != NULL) {
        char path[4096];
        struct stat st;

        if (skip_dir_name(entry->d_name, is_proj_root, depth)) continue;
        snprintf(path, sizeof(path), "%s/%s", dir, entry->d_name);
        if (stat(path, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            if (depth >= 0 && is_project_root(path)) continue;
            if (scan_sources_recursive(path, list, 0, is_proj_root, depth + 1) != 0) {
                closedir(handle);
                return 1;
            }
        } else if (S_ISREG(st.st_mode) && has_fortran_ext(path)) {
            if (path_list_add(list, path) != 0) {
                closedir(handle);
                return 1;
            }
        }
    }

    closedir(handle);
    return 0;
}

void fo_c_scan_sources(const char *root, const char *output_file, int *exitcode) {
    struct path_list list = {0};
    FILE *out;
    size_t i;
    int proj_root;

    *exitcode = 0;
    if (!has_text(root) || !has_text(output_file)) {
        *exitcode = 1;
        return;
    }
    proj_root = is_project_root(root);
    if (scan_sources_recursive(root, &list, 1, proj_root, 0) != 0) {
        path_list_free(&list);
        *exitcode = 1;
        return;
    }

    qsort(list.items, list.n, sizeof(char *), path_cmp);
    out = fopen(output_file, "w");
    if (out == NULL) {
        *exitcode = 1;
    } else {
        for (i = 0; i < list.n; i++) fprintf(out, "%s\n", list.items[i]);
        fclose(out);
    }

    path_list_free(&list);
}

extern char **environ;

/* Build environ plus semicolon-separated "KEY=VALUE" entries in the PARENT,
 * so the child does no malloc (async-signal-safe). Returns NULL on alloc
 * failure. */
static char **env_with_extra(const char *extra) {
    int n = 0, i, extras = 1, slot;
    const char *cursor;
    char **e;
    while (environ[n]) n++;
    for (cursor = extra; *cursor; cursor++) {
        if (*cursor == ';') extras++;
    }
    e = (char **)calloc((size_t)(n + extras + 1), sizeof(char *));
    if (!e) return NULL;
    for (i = 0; i < n; i++) e[i] = environ[i];
    slot = n;
    cursor = extra;
    while (*cursor) {
        const char *end = strchr(cursor, ';');
        size_t length = end ? (size_t)(end - cursor) : strlen(cursor);
        if (length > 0) {
            e[slot] = strndup(cursor, length);
            if (!e[slot]) {
                int j;
                for (j = n; j < slot; j++) free(e[j]);
                free(e);
                return NULL;
            }
            slot++;
        }
        if (!end) break;
        cursor = end + 1;
    }
    return e;
}

static void free_env_with_extra(char **env) {
    int n = 0, i;
    if (!env) return;
    while (environ[n]) n++;
    for (i = n; env[i]; i++) free(env[i]);
    free(env);
}

static int env_name_matches(const char *entry, const char *key, size_t key_len) {
    const char *equals = strchr(entry, '=');
    return equals != NULL && (size_t)(equals - entry) == key_len &&
           memcmp(entry, key, key_len) == 0;
}

static int env_extra_has_key(const char *extra, const char *entry) {
    const char *cursor = extra;
    const char *entry_equals = strchr(entry, '=');
    size_t entry_key_len;

    if (entry_equals == NULL) return 0;
    entry_key_len = (size_t)(entry_equals - entry);
    while (cursor != NULL && *cursor != '\0') {
        const char *end = strchr(cursor, ';');
        const char *equals = strchr(cursor, '=');
        size_t length = end ? (size_t)(end - cursor) : strlen(cursor);
        if (length == 0) {
            cursor = end ? end + 1 : NULL;
            continue;
        }
        if (equals != NULL && equals < cursor + length &&
            (size_t)(equals - cursor) == entry_key_len &&
            memcmp(cursor, entry, entry_key_len) == 0) return 1;
        cursor = end ? end + 1 : NULL;
    }
    return 0;
}

/* Async jobs need true KEY=VALUE replacement when the caller overrides an
   inherited setting. All strings are copied before fork, so the child only
   swaps environ and execs. */
static char **env_with_overrides(const char *extra) {
    size_t n = 0, capacity, used = 0, extras_start, i;
    const char *cursor;
    char **env;

    while (environ[n] != NULL) n++;
    capacity = n + 1;
    for (cursor = extra; cursor != NULL && *cursor != '\0'; ) {
        const char *end = strchr(cursor, ';');
        const char *equals = strchr(cursor, '=');
        size_t length = end ? (size_t)(end - cursor) : strlen(cursor);
        if (length == 0) {
            cursor = end ? end + 1 : NULL;
            continue;
        }
        if (equals == NULL || equals == cursor || equals >= cursor + length) {
            errno = EINVAL;
            return NULL;
        }
        capacity++;
        cursor = end ? end + 1 : NULL;
    }
    env = calloc(capacity + 1, sizeof(char *));
    if (env == NULL) return NULL;
    for (i = 0; i < n; i++) {
        if (env_extra_has_key(extra, environ[i])) continue;
        env[used] = strdup(environ[i]);
        if (env[used] == NULL) goto allocation_failed;
        used++;
    }
    extras_start = used;
    for (cursor = extra; cursor != NULL && *cursor != '\0'; ) {
        const char *end = strchr(cursor, ';');
        const char *equals = strchr(cursor, '=');
        size_t length = end ? (size_t)(end - cursor) : strlen(cursor);
        size_t key_len, j;
        if (length == 0) {
            cursor = end ? end + 1 : NULL;
            continue;
        }
        key_len = (size_t)(equals - cursor);
        for (j = extras_start; j < used; j++) {
            if (env_name_matches(env[j], cursor, key_len)) break;
        }
        if (j == used) used++;
        else free(env[j]);
        env[j] = strndup(cursor, length);
        if (env[j] == NULL) goto allocation_failed;
        cursor = end ? end + 1 : NULL;
    }
    return env;

allocation_failed:
    while (used > 0) free(env[--used]);
    free(env);
    errno = ENOMEM;
    return NULL;
}

static void free_env_copy(char **env) {
    size_t i;
    if (env == NULL) return;
    for (i = 0; env[i] != NULL; i++) free(env[i]);
    free(env);
}

static void add_milliseconds(struct timespec *ts, int ms) {
    ts->tv_nsec += (long)(ms % 1000) * 1000000L;
    ts->tv_sec += ms / 1000 + ts->tv_nsec / 1000000000L;
    ts->tv_nsec %= 1000000000L;
}

enum { CPU_POLL_MS = 200 };

struct run_budget {
    int cpu_s;         /* in: CPU-second budget; 0 means wall clock only */
    int kind;          /* out: 0 none, 1 CPU, 2 wall cap, 3 CPU unmeasurable */
    long long cpu_ms;  /* out: child CPU time at exit or kill, -1 if unknown */
    long long wall_ms; /* out: wall time at the kill */
};

/* User plus system CPU time of a live (or zombie) child over all of its
   threads, in milliseconds; -1 where it cannot be read cheaply. Processes the
   child spawns are not counted, matching RLIMIT_CPU. */
static long long child_cpu_ms(pid_t pid) {
#ifdef __linux__
    char path[64], buf[1024];
    char *p, *end;
    unsigned long long utime, stime;
    long ticks = sysconf(_SC_CLK_TCK);
    ssize_t n;
    int fd, field;

    if (ticks <= 0) return -1;
    snprintf(path, sizeof(path), "/proc/%d/stat", (int)pid);
    fd = open(path, O_RDONLY);
    if (fd < 0) return -1;
    n = read(fd, buf, sizeof(buf) - 1);
    close(fd);
    if (n <= 0) return -1;
    buf[n] = '\0';
    p = strrchr(buf, ')');
    if (p == NULL) return -1;
    p++;
    /* After "pid (comm)" come field 3 (state) onward; utime and stime are
       fields 14 and 15. */
    for (field = 3; field < 14; field++) {
        while (*p == ' ') p++;
        while (*p != ' ' && *p != '\0') p++;
        if (*p == '\0') return -1;
    }
    utime = strtoull(p, &end, 10);
    if (end == p) return -1;
    p = end;
    stime = strtoull(p, &end, 10);
    if (end == p) return -1;
    return (long long)((utime + stime) * 1000ULL / (unsigned long long)ticks);
#else
    (void)pid;
    return -1;
#endif
}

static void record_timeout(struct run_budget *budget, int kind,
                           long long cpu_ms, const struct timespec *start,
                           const struct timespec *now) {
    if (budget == NULL) return;
    budget->kind = kind;
    budget->cpu_ms = cpu_ms;
    budget->wall_ms =
        (long long)(now->tv_sec - start->tv_sec) * 1000LL +
        (long long)(now->tv_nsec - start->tv_nsec) / 1000000LL;
}


/* The command monitor cleans its own adopted descendants on SIGTERM. If it
   cannot finish, fail closed on that nested handle's group. */
static void kill_group_and_reap(pid_t pid, int *status, int isolated_group) {
    int reaped = 0;
    pid_t waited;
    pid_t target = isolated_group ? -pid : pid;

    kill(target, SIGTERM);
    for (int k = 0; k < 15; k++) {
        waited = waitpid(pid, status, WNOHANG);
        if (waited == pid) {
            reaped = 1;
            break;
        }
        if (waited < 0 && errno != EINTR) break;
        sleep_ms(200);
    }
    if (!isolated_group && reaped) return;
    if (!isolated_group && getpgrp() == getpid() && getsid(0) != getpid())
        (void)kill(-getpgrp(), SIGKILL);
    kill(target, SIGKILL);
    if (!reaped) {
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
    }
}

static int read_exact(int fd, void *buffer, size_t size);

#ifdef __linux__
static int start_command_monitor(const char *cwd, char *const argv[],
                                 const char *log_file, int append,
                                 char **child_env, pid_t *monitor_pid,
                                 pid_t *target_pid, int *report_fd) {
    posix_spawn_file_actions_t actions;
    char **monitor_argv;
    int pipefd[2], error, ready, actions_ready = 0;
    size_t count = 0;

    *target_pid = 0;
    while (argv[count] != NULL) count++;
    monitor_argv = calloc(count + 6, sizeof(*monitor_argv));
    if (monitor_argv == NULL) return ENOMEM;
    monitor_argv[0] = "/proc/self/exe";
    monitor_argv[1] = FO_MONITOR_ARG;
    monitor_argv[2] = (char *)(cwd != NULL ? cwd : "");
    monitor_argv[3] = (char *)(log_file != NULL ? log_file : "");
    monitor_argv[4] = append ? "1" : "0";
    for (size_t i = 0; i < count; i++) monitor_argv[i + 5] = argv[i];
    if (pipe2(pipefd, O_CLOEXEC) != 0) {
        error = errno;
        free(monitor_argv);
        return error;
    }
    for (int i = 0; i < 2; i++) {
        if (pipefd[i] == FO_MONITOR_FD) {
            int moved = fcntl(pipefd[i], F_DUPFD_CLOEXEC, FO_MONITOR_FD + 1);
            if (moved < 0) {
                error = errno;
                close(pipefd[0]); close(pipefd[1]);
                free(monitor_argv);
                return error;
            }
            close(pipefd[i]);
            pipefd[i] = moved;
        }
    }
    error = posix_spawn_file_actions_init(&actions);
    if (error == 0) actions_ready = 1;
    if (error == 0)
        error = posix_spawn_file_actions_adddup2(
            &actions, pipefd[1], FO_MONITOR_FD);
    if (error == 0)
        error = posix_spawn_file_actions_addclose(&actions, pipefd[0]);
    if (error == 0)
        error = posix_spawn_file_actions_addclose(&actions, pipefd[1]);
    if (error == 0)
        error = posix_spawn(monitor_pid, "/proc/self/exe", &actions, NULL,
                            monitor_argv, child_env ? child_env : environ);
    if (actions_ready) posix_spawn_file_actions_destroy(&actions);
    free(monitor_argv);
    close(pipefd[1]);
    if (error != 0) {
        close(pipefd[0]);
        return error;
    }
    {
        struct pollfd reply = {.fd = pipefd[0], .events = POLLIN};
        do {
            ready = poll(&reply, 1, 5000);
        } while (ready < 0 && errno == EINTR);
    }
    if (ready <= 0 || read_exact(pipefd[0], target_pid,
                                 sizeof(*target_pid)) != 0 ||
        *target_pid <= 0) {
        int launch_error = *target_pid < 0 ? -*target_pid : EIO;
        (void)kill(*monitor_pid, SIGKILL);
        while (waitpid(*monitor_pid, NULL, 0) < 0 && errno == EINTR) {
        }
        close(pipefd[0]);
        return launch_error;
    }
    *report_fd = pipefd[0];
    return 0;
}
#endif

static int run_argv(const char *cwd, char *const argv[], const char *log_file,
                    int append, int jobs, int timeout_s, int heartbeat_s,
                    const char *env_extra, struct run_budget *budget) {
    pid_t pid;
    int status;
    int pid_fd = -1;
    int spawn_error;
    int isolated_group = 1;
    int report_fd = -1;
    int attrs_ready = 0;
    pid_t accounted_pid = 0;
    char **child_env = NULL;
    posix_spawn_file_actions_t actions;
    posix_spawnattr_t attrs;
    short attr_flags = POSIX_SPAWN_SETPGROUP;

    (void)jobs;
    if (has_text(env_extra)) {
        child_env = env_with_extra(env_extra);
        if (!child_env) return 1;
    }

    spawn_error = posix_spawn_file_actions_init(&actions);
    if (spawn_error != 0) {
        free_env_with_extra(child_env);
        emit_spawn_error(log_file, "posix_spawn file-actions setup", spawn_error);
        return 1;
    }
    if (has_text(cwd)) {
        spawn_error = posix_spawn_file_actions_addchdir_np(&actions, cwd);
    }
    if (spawn_error == 0 && has_text(log_file)) {
        int flags = O_WRONLY | O_CREAT | (append ? O_APPEND : O_TRUNC);
        spawn_error = posix_spawn_file_actions_addopen(
            &actions, STDOUT_FILENO, log_file, flags, 0666);
        if (spawn_error == 0) {
            spawn_error = posix_spawn_file_actions_adddup2(
                &actions, STDOUT_FILENO, STDERR_FILENO);
        }
    }
    if (spawn_error != 0) {
        posix_spawn_file_actions_destroy(&actions);
        free_env_with_extra(child_env);
        emit_spawn_error(log_file, "posix_spawn file-action", spawn_error);
        return 1;
    }

    spawn_error = posix_spawnattr_init(&attrs);
    if (spawn_error == 0) attrs_ready = 1;
    if (spawn_error == 0) {
        spawn_error = posix_spawnattr_setflags(&attrs, attr_flags);
    }
    if (spawn_error == 0) {
        spawn_error = posix_spawnattr_setpgroup(&attrs, 0);
    }
    if (spawn_error == 0) {
        spawn_error = posix_spawnp(&pid, argv[0], &actions, &attrs, argv,
                                   child_env ? child_env : environ);
    }
    /* A nested async job has a stricter inherited filter: its synchronous
       children stay in that job's group so its handle can cancel the tree. */
#ifdef __linux__
    if (spawn_error == EPERM && prctl(PR_GET_SECCOMP, 0, 0, 0, 0) == 2) {
        spawn_error = start_command_monitor(cwd, argv, log_file, append,
                                            child_env, &pid, &accounted_pid,
                                            &report_fd);
        if (spawn_error == 0) isolated_group = 0;
    }
#endif
    posix_spawn_file_actions_destroy(&actions);
    if (attrs_ready) posix_spawnattr_destroy(&attrs);
    free_env_with_extra(child_env);
    if (spawn_error != 0) {
        char operation[192];
        snprintf(operation, sizeof(operation), "posix_spawn of %.80s in %.80s",
                 argv[0] ? argv[0] : "(null)",
                 has_text(cwd) ? cwd : "(current directory)");
        emit_spawn_error(log_file, operation, spawn_error);
        return spawn_error == ENOENT ? 127 : 126;
    }
    if (accounted_pid == 0) accounted_pid = pid;

#if defined(__linux__) && defined(SYS_pidfd_open)
    pid_fd = (int)syscall(SYS_pidfd_open, pid, 0);
#endif

    if (timeout_s > 0) {
        struct timespec start, deadline, next_heartbeat, cpu_check, now;
        int cpu_s = budget ? budget->cpu_s : 0;

        clock_gettime(CLOCK_MONOTONIC, &start);
        deadline = start;
        add_seconds(&deadline, timeout_s);
        next_heartbeat = deadline;
        if (heartbeat_s > 0) {
            next_heartbeat = start;
            add_seconds(&next_heartbeat, heartbeat_s);
        }
        /* The CPU budget is only consulted once the child has also been
           running for that long in wall time: a child cannot have used more
           CPU than that on one thread, and a multi-threaded one keeps the old
           wall-clock grace. Host load then never decides a timeout; only the
           wall-clock cap does, and it is meant to be generous. */
        if (cpu_s <= 0 || cpu_s >= timeout_s) cpu_s = 0;
        cpu_check = deadline;
        if (cpu_s > 0) {
            cpu_check = start;
            add_seconds(&cpu_check, cpu_s);
        }

        for (;;) {
            siginfo_t info;
            int waited;

            /* Peek without reaping, so the exited child's own CPU time can
               still be read; the rusage from wait4 would also count every
               process the test spawned and waited for (compilers, shells). */
            memset(&info, 0, sizeof(info));
            waited = waitid(P_PID, (id_t)pid, &info, WEXITED | WNOHANG | WNOWAIT);
            if (waited == 0 && info.si_pid == pid) {
                struct rusage usage;
                long long own = budget != NULL ? child_cpu_ms(accounted_pid) : -1;

                while (wait4(pid, &status, 0, &usage) < 0) {
                    if (errno == EINTR) continue;
                    if (pid_fd >= 0) close(pid_fd);
                    if (report_fd >= 0) close(report_fd);
                    return 1;
                }
                if (budget != NULL) {
                    if (report_fd >= 0) {
                        long long reported;
                        if (read_exact(report_fd, &reported,
                                       sizeof(reported)) == 0)
                            own = reported;
                    }
                    if (own < 0) {
                        own = (long long)(usage.ru_utime.tv_sec +
                                          usage.ru_stime.tv_sec) * 1000LL +
                              (long long)(usage.ru_utime.tv_usec +
                                          usage.ru_stime.tv_usec) / 1000LL;
                    }
                    budget->cpu_ms = own;
                }
                break;
            }
            if (waited < 0 && errno != EINTR) {
                if (pid_fd >= 0) close(pid_fd);
                if (report_fd >= 0) close(report_fd);
                return 1;
            }

            clock_gettime(CLOCK_MONOTONIC, &now);
            if (timespec_at_or_after(&now, &deadline)) {
                record_timeout(budget, 2, child_cpu_ms(accounted_pid), &start, &now);
                kill_group_and_reap(pid, &status, isolated_group);
                if (pid_fd >= 0) close(pid_fd);
                if (report_fd >= 0) close(report_fd);
                return 124;
            }
            if (cpu_s > 0 && timespec_at_or_after(&now, &cpu_check)) {
                long long used = child_cpu_ms(accounted_pid);
                if (used < 0 || used >= (long long)cpu_s * 1000LL) {
                    record_timeout(budget, used < 0 ? 3 : 1, used, &start, &now);
                    kill_group_and_reap(pid, &status, isolated_group);
                    if (pid_fd >= 0) close(pid_fd);
                    if (report_fd >= 0) close(report_fd);
                    return 124;
                }
                cpu_check = now;
                add_milliseconds(&cpu_check, CPU_POLL_MS);
            }
            if (heartbeat_s > 0 &&
                timespec_at_or_after(&now, &next_heartbeat)) {
                emit_heartbeat(log_file);
                do {
                    add_seconds(&next_heartbeat, heartbeat_s);
                } while (timespec_at_or_after(&now, &next_heartbeat));
            }
            if (pid_fd >= 0) {
                struct pollfd child = {
                    .fd = pid_fd,
                    .events = POLLIN,
                    .revents = 0
                };
                int wait_ms = milliseconds_until(&now, &deadline);
                int poll_result;

                if (heartbeat_s > 0) {
                    int heartbeat_ms =
                        milliseconds_until(&now, &next_heartbeat);
                    if (heartbeat_ms < wait_ms) wait_ms = heartbeat_ms;
                }
                if (cpu_s > 0) {
                    int cpu_ms = milliseconds_until(&now, &cpu_check);
                    if (cpu_ms < wait_ms) wait_ms = cpu_ms;
                }
                poll_result = poll(&child, 1, wait_ms);
                if (poll_result < 0 && errno == EINTR) continue;
                if (poll_result < 0) {
                    close(pid_fd);
                    if (report_fd >= 0) close(report_fd);
                    return 1;
                }
            } else {
                sleep_ms(1);
            }
        }
    } else {
        while (waitpid(pid, &status, 0) < 0) {
            if (errno == EINTR) continue;
            if (report_fd >= 0) close(report_fd);
            return 1;
        }
    }
    if (pid_fd >= 0) close(pid_fd);
    if (report_fd >= 0) close(report_fd);
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 1;
}

static int env_timeout(const char *var, int default_s) {
    const char *s = getenv(var);
    int v;
    if (!s || !s[0]) return default_s;
    v = atoi(s);
    return v > 0 ? v : default_s;
}

static int heartbeat_seconds(void) {
    const char *value = getenv("FO_HEARTBEAT_SECONDS");
    int seconds;

    if (heartbeats_suppressed) return 0;
    if (!value || !value[0]) return 10;
    seconds = atoi(value);
    return seconds >= 0 ? seconds : 10;
}

void fo_c_suppress_heartbeats(int suppress) {
    heartbeats_suppressed = suppress != 0;
}

void fo_c_detect_nproc(int *nproc) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 1) n = 1;
    *nproc = (int)n;
}

void fo_c_configure_openmp(void) {
    /* libgomp's default active wait burns a full worker team while fo waits
     * for compiler children.  Preserve an explicit user choice, but default
     * the build driver to sleeping workers before its first parallel region.
     * Target execution deliberately skips this hook in app/main.f90 so the
     * launched program keeps its own OpenMP/runtime policy. */
    if (getenv("OMP_WAIT_POLICY") == NULL) {
        setenv("OMP_WAIT_POLICY", "PASSIVE", 0);
    }
}

/* Publish a default into the environment that every test child inherits, but
   only when the user has not set the variable themselves: an explicit value
   in the shell is a request and outranks the driver's preference. This is how
   the parallel test team tells a conformance walker inside it to stop sharding
   (FFC_CONFORMANCE_JOBS=1): 24 tests each opening 16 compiler children is
   oversubscription, and the measured effect is that every one of them gets
   slower. */
void fo_c_setenv_default(const char *name, const char *value) {
    if (name == NULL || value == NULL) return;
    if (getenv(name) != NULL) return;
    setenv(name, value, 1);
}

/* Terminate with a status and no runtime banner: Fortran ERROR STOP prints
   "Error termination" and a backtrace, which buries the actual message. The
   Fortran side flushes its units first; exit() then runs the runtime's own
   cleanup. */
void fo_c_exit(int code) {
    fflush(NULL);
    exit(code);
}

void fo_c_getpid(int *pid_out) {
    *pid_out = (int)getpid();
}

void fo_c_getcwd(char *path, int path_len, int *exitcode) {
    if (path == NULL || path_len < 2) {
        *exitcode = EINVAL;
        return;
    }
    if (getcwd(path, (size_t)path_len) == NULL) {
        path[0] = '\0';
        *exitcode = errno;
        return;
    }
    *exitcode = 0;
}

/* Run a single executable with stdout/stderr redirected to log_file, enforcing
   a hard timeout. On timeout the whole process group is killed and 124 is
   returned (mirrors GNU timeout). Used for untrusted test binaries that may
   hang. */
void fo_c_run_logged(const char *cwd, const char *exe_path, const char *log_file,
                     int append, int timeout_s, const char *env_extra,
                     int *exitcode) {
    char *argv[2];

    if (!has_text(exe_path)) {
        *exitcode = 127;
        return;
    }
    argv[0] = (char *)exe_path;
    argv[1] = NULL;
    *exitcode = run_argv(has_text(cwd) ? cwd : NULL, argv, log_file, append, 0,
                         timeout_s, heartbeat_seconds(),
                         has_text(env_extra) ? env_extra : NULL, NULL);
}

/* Run an arbitrary command given as an argv vector, with no shell. args is a
   buffer of n_args NUL-terminated strings packed back-to-back (args_len bytes
   total); argv[0] is the program. This is the quote-proof, async-signal-safe
   path for compile/link invocations inside the OpenMP build loop: fork+execve
   with no /bin/sh, so quoting never breaks and libgomp is never corrupted. */
static void run_packed_argv(const char *cwd, const char *args, int args_len,
                            int n_args, const char *log_file, int append,
                            int timeout_s, int heartbeat_s,
                            const char *env_extra, struct run_budget *budget,
                            int *exitcode) {
    char **argv;
    const char *p;
    const char *end;
    int idx;

    if (n_args <= 0 || args == NULL) {
        *exitcode = 127;
        return;
    }
    argv = (char **)calloc((size_t)n_args + 1, sizeof(char *));
    if (argv == NULL) {
        *exitcode = 1;
        return;
    }
    p = args;
    end = args + args_len;
    idx = 0;
    while (idx < n_args && p < end) {
        argv[idx++] = (char *)p;
        p += strlen(p) + 1;
    }
    argv[idx] = NULL;
    *exitcode = run_argv(has_text(cwd) ? cwd : NULL, argv, log_file, append, 0,
                         timeout_s, heartbeat_s < 0 ? heartbeat_seconds() : heartbeat_s,
                         has_text(env_extra) ? env_extra : NULL, budget);
    free(argv);
}

void fo_c_run_argv_logged(const char *cwd, const char *args, int args_len,
                          int n_args, const char *log_file, int append,
                          int timeout_s, int heartbeat_s,
                          const char *env_extra, int *exitcode) {
    run_packed_argv(cwd, args, args_len, n_args, log_file, append, timeout_s,
                    heartbeat_s, env_extra, NULL, exitcode);
}

/* As fo_c_run_argv_logged, with a CPU-second budget below the wall-clock cap
   timeout_s. On a timeout (exit 124) timeout_kind reports which limit fired:
   1 CPU budget, 2 wall-clock cap, 3 budget reached where CPU time cannot be
   measured (the budget then acts as a wall clock). cpu_ms is -1 if unknown. */
void fo_c_run_argv_budget(const char *cwd, const char *args, int args_len,
                          int n_args, const char *log_file, int append,
                          int cpu_budget_s, int timeout_s, int heartbeat_s,
                          const char *env_extra, int *exitcode,
                          int *timeout_kind, long long *cpu_ms,
                          long long *wall_ms) {
    struct run_budget budget = {cpu_budget_s, 0, -1, 0};

    run_packed_argv(cwd, args, args_len, n_args, log_file, append, timeout_s,
                    heartbeat_s, env_extra, &budget, exitcode);
    *timeout_kind = budget.kind;
    *cpu_ms = budget.cpu_ms;
    *wall_ms = budget.wall_ms;
}

/* Linux's subreaper setting lets this owner reap orphaned grandchildren after
   their parent exits. It is process-wide, but waitpid below is always scoped
   to the session created for one async job. */
static int ensure_async_subreaper(void) {
#ifdef __linux__
    if (prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0) return errno;
#endif
    return 0;
}

/* Linux async descendants cannot leave the owned session. Native setpgid is
   allowed for provider children; cancellation covers every group in the
   session. Cover known syscall ABIs and reject unknown ABIs. */
#if defined(__linux__)
#if defined(FO_ASYNC_TEST_AARCH64) || defined(__aarch64__)
#define FO_ASYNC_AUDIT_ARCH AUDIT_ARCH_AARCH64
#define FO_ASYNC_HAS_COMPAT_ABI 1
#define FO_ASYNC_COMPAT_AUDIT_ARCH AUDIT_ARCH_ARM
#define FO_ASYNC_COMPAT_SETSID 66
#define FO_ASYNC_COMPAT_SETPGID 57
#define FO_ASYNC_NATIVE_SETSID 157
#define FO_ASYNC_NATIVE_SETPGID 154
#define FO_ASYNC_NATIVE_MISMATCH_SKIP 6
#if defined(__aarch64__) && (__NR_setsid != 157 || __NR_setpgid != 154)
#error "AArch64 session syscall numbers differ from the containment filter"
#endif
#elif defined(FO_ASYNC_TEST_ARM) || defined(__arm__)
#define FO_ASYNC_AUDIT_ARCH AUDIT_ARCH_ARM
#define FO_ASYNC_NATIVE_SETSID 66
#define FO_ASYNC_NATIVE_SETPGID 57
#if defined(__arm__) && (__NR_setsid != 66 || __NR_setpgid != 57)
#error "ARM session syscall numbers differ from the containment filter"
#endif
#elif defined(FO_ASYNC_TEST_UNSUPPORTED)
#define FO_ASYNC_AUDIT_ARCH 0
#elif defined(FO_ASYNC_TEST_X86_64) || defined(__x86_64__)
#define FO_ASYNC_AUDIT_ARCH AUDIT_ARCH_X86_64
#define FO_ASYNC_HAS_COMPAT_ABI 1
#define FO_ASYNC_COMPAT_AUDIT_ARCH AUDIT_ARCH_I386
#define FO_ASYNC_COMPAT_SETSID 66
#define FO_ASYNC_COMPAT_SETPGID 57
#define FO_ASYNC_HAS_X32_ABI 1
#define FO_ASYNC_X32_SYSCALL_BIT 0x40000000U
#define FO_ASYNC_NATIVE_SETSID 112
#define FO_ASYNC_NATIVE_SETPGID 109
#define FO_ASYNC_NATIVE_MISMATCH_SKIP 13
#if defined(__x86_64__) && (__NR_setsid != 112 || __NR_setpgid != 109)
#error "x86-64 session syscall numbers differ from the containment filter"
#endif
#elif defined(__i386__)
#define FO_ASYNC_AUDIT_ARCH AUDIT_ARCH_I386
#else
#define FO_ASYNC_AUDIT_ARCH 0
#endif
#if FO_ASYNC_AUDIT_ARCH != 0 && !defined(FO_ASYNC_NATIVE_SETSID)
#define FO_ASYNC_NATIVE_SETSID __NR_setsid
#define FO_ASYNC_NATIVE_SETPGID __NR_setpgid
#endif
#ifndef FO_ASYNC_NATIVE_MISMATCH_SKIP
#define FO_ASYNC_NATIVE_MISMATCH_SKIP 7
#endif
#endif

#if defined(__linux__) && FO_ASYNC_AUDIT_ARCH != 0
static const struct sock_filter async_group_filter[] = {
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                 (unsigned int)offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_AUDIT_ARCH, 0,
                 FO_ASYNC_NATIVE_MISMATCH_SKIP),
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                 (unsigned int)offsetof(struct seccomp_data, nr)),
#ifdef FO_ASYNC_HAS_X32_ABI
        BPF_JUMP(BPF_JMP | BPF_JSET | BPF_K,
                 FO_ASYNC_X32_SYSCALL_BIT, 5, 0),
#endif
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_NATIVE_SETSID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_NATIVE_SETPGID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
#ifdef FO_ASYNC_HAS_X32_ABI
        BPF_STMT(BPF_ALU | BPF_AND | BPF_K, ~FO_ASYNC_X32_SYSCALL_BIT),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_NATIVE_SETSID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_NATIVE_SETPGID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA)),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
#endif
#ifdef FO_ASYNC_HAS_COMPAT_ABI
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                 (unsigned int)offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,
                 FO_ASYNC_COMPAT_AUDIT_ARCH, 0, 6),
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                 (unsigned int)offsetof(struct seccomp_data, nr)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_COMPAT_SETSID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, FO_ASYNC_COMPAT_SETPGID, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA)),
#endif
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | (EPERM & SECCOMP_RET_DATA))
};
#endif

static int install_async_group_containment(int strict_group) {
#if defined(__linux__) && FO_ASYNC_AUDIT_ARCH != 0
    struct sock_filter filter[sizeof(async_group_filter) /
                              sizeof(async_group_filter[0])];
    struct sock_fprog program = {
        (unsigned short)(sizeof(async_group_filter) / sizeof(async_group_filter[0])),
        filter
    };
    size_t i;
#if defined(SYS_pidfd_open) && defined(SYS_pidfd_send_signal)
    int fd = (int)syscall(SYS_pidfd_open, getpid(), 0);
    int probe_error;

    if (fd < 0) return errno;
    probe_error = (int)syscall(SYS_pidfd_send_signal, fd, 0, NULL, 0);
    if (probe_error != 0) probe_error = errno;
    close(fd);
    if (probe_error != 0) return probe_error;
#else
    return ENOTSUP;
#endif

    memcpy(filter, async_group_filter, sizeof(filter));
    if (strict_group) {
        for (i = 0; i + 1 < program.len; i++) {
            if (filter[i].code == (BPF_JMP | BPF_JEQ | BPF_K) &&
                filter[i].k == FO_ASYNC_NATIVE_SETPGID &&
                filter[i + 1].k == SECCOMP_RET_ALLOW) {
                filter[i + 1].k = SECCOMP_RET_ERRNO |
                                  (EPERM & SECCOMP_RET_DATA);
                break;
            }
        }
        if (i + 1 >= program.len) return EINVAL;
    }

    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) return errno;
    if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &program) != 0) return errno;
    return 0;
#else
    /* Without an inherited group-escape barrier, do not claim tree ownership. */
    (void)strict_group;
    return ENOTSUP;
#endif
}

#ifdef __linux__
/* Session IDs remain inherited across process groups. Read the kernel's SID
   for each candidate before signalling it; pidfds pin the selected process
   across a concurrent exit and PID reuse. */
static int linux_process_session(pid_t pid, pid_t *session) {
    char path[64], line[4096], *close, state;
    long parent, group, sid;
    FILE *file;

    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    file = fopen(path, "r");
    if (file == NULL) return -1;
    if (fgets(line, sizeof(line), file) == NULL) {
        fclose(file);
        return -1;
    }
    fclose(file);
    close = strrchr(line, ')');
    if (close == NULL || sscanf(close + 1, " %c %ld %ld %ld",
                                &state, &parent, &group, &sid) != 4)
        return -1;
    *session = (pid_t)sid;
    return 0;
}

static int signal_async_session(pid_t session, int signal_number) {
    DIR *directory = opendir("/proc");
    struct dirent *entry;
    int count = 0;

    if (directory == NULL) return -1;
    while ((entry = readdir(directory)) != NULL) {
        char *end;
        long value = strtol(entry->d_name, &end, 10);
        pid_t candidate, current;
#if defined(SYS_pidfd_open) && defined(SYS_pidfd_send_signal)
        int fd = -1;
#endif

        if (*entry->d_name == '\0' || *end != '\0' || value <= 0 ||
            value > INT_MAX) continue;
        candidate = (pid_t)value;
        if (linux_process_session(candidate, &current) != 0 ||
            current != session) continue;
        count++;
        if (signal_number == 0) continue;
#if defined(SYS_pidfd_open) && defined(SYS_pidfd_send_signal)
        fd = (int)syscall(SYS_pidfd_open, candidate, 0);
        if (fd >= 0) {
            if (linux_process_session(candidate, &current) == 0 &&
                current == session &&
                syscall(SYS_pidfd_send_signal, fd, signal_number,
                        NULL, 0) != 0 && errno != ESRCH) {
                int error = errno;
                close(fd);
                closedir(directory);
                errno = error;
                return -1;
            }
            close(fd);
            continue;
        }
        if (errno == ESRCH) continue;
#else
        errno = ENOTSUP;
#endif
        {
            int error = errno;
            closedir(directory);
            errno = error;
            return -1;
        }
    }
    closedir(directory);
    return count;
}
#endif

/* Start ticks on Linux and start time on macOS protect against signalling a
   reused leader PID. A surviving process group keeps its group ID allocated. */
static uint64_t process_start_identity(pid_t pid) {
#ifdef __linux__
    char path[64], buf[4096], *close, *p, *end;
    FILE *file;
    int field;
    unsigned long long value = 0;

    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    file = fopen(path, "r");
    if (file == NULL) return 0;
    if (fgets(buf, sizeof(buf), file) == NULL) {
        fclose(file);
        return 0;
    }
    fclose(file);
    close = strrchr(buf, ')');
    if (close == NULL) return 0;
    p = close + 1;
    for (field = 3; field <= 22; field++) {
        while (*p == ' ') p++;
        if (*p == '\0') return 0;
        end = p;
        while (*end != '\0' && *end != ' ') end++;
        if (field == 22) {
            char saved = *end;
            *end = '\0';
            value = strtoull(p, NULL, 10);
            *end = saved;
            break;
        }
        p = end;
    }
    return (uint64_t)value;
#elif defined(__APPLE__)
    struct proc_bsdinfo info;
    int bytes = proc_pidinfo((int)pid, PROC_PIDTBSDINFO, 0, &info,
                             (int)sizeof(info));
    if (bytes != (int)sizeof(info)) return 0;
    return (uint64_t)info.pbi_start_tvsec * 1000000ULL +
           (uint64_t)info.pbi_start_tvusec;
#else
    (void)pid;
    return 0;
#endif
}

static int read_exact(int fd, void *buffer, size_t size) {
    char *p = buffer;
    size_t used = 0;
    while (used < size) {
        ssize_t n = read(fd, p + used, size - used);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        used += (size_t)n;
    }
    return 0;
}

static int write_exact(int fd, const void *buffer, size_t size) {
    const char *p = buffer;
    size_t used = 0;
    while (used < size) {
        ssize_t n = write(fd, p + used, size - used);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        used += (size_t)n;
    }
    return 0;
}

static int async_group_exists(pid_t pid) {
    if (kill(-pid, 0) == 0) return 1;
    return errno == EPERM;
}

static int async_owner_exists(const struct async_process *item) {
#ifdef __linux__
    if (item->owns_session) {
        int count = signal_async_session(item->session, 0);
        return count < 0 ? -1 : count > 0;
    }
#endif
    return async_group_exists(item->pid);
}

static int signal_async_owner(const struct async_process *item, int signal_number) {
#ifdef __linux__
    if (item->owns_session)
        return signal_async_session(item->session, signal_number) < 0 ? errno : 0;
#endif
    if (kill(-item->pid, signal_number) != 0 && errno != ESRCH) return errno;
    return 0;
}

static struct async_process *find_async_process(pid_t pid) {
    struct async_process *item;
    for (item = async_processes; item != NULL; item = item->next) {
        if (item->pid == pid) return item;
    }
    return NULL;
}

static void forget_async_process(struct async_process *item) {
    struct async_process **link = &async_processes;
    while (*link != NULL) {
        if (*link == item) {
            *link = item->next;
            free(item);
            return;
        }
        link = &(*link)->next;
    }
}

static int async_identity_matches(const struct async_process *item) {
    pid_t group = getpgid(item->pid);
    if (group >= 0) {
        uint64_t current;
        if (group != item->pid || getsid(item->pid) != item->session) return 0;
        current = process_start_identity(item->pid);
        if (item->start_identity != 0 && current != item->start_identity) return 0;
        return 1;
    }
    /* A dead leader can leave ordinary descendants in its original group. */
    return errno == ESRCH && async_owner_exists(item) != 0;
}

static int observe_async_leader(struct async_process *item) {
    int status;
    pid_t got;

    if (item->leader_done) return 0;
    do {
        got = waitpid(item->pid, &status, WNOHANG);
    } while (got < 0 && errno == EINTR);
    if (got == 0) return 0;
    if (got < 0) {
        if (errno != ECHILD) return errno;
        item->exitcode = ESRCH;
    } else if (WIFEXITED(status)) {
        item->exitcode = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        item->exitcode = 128 + WTERMSIG(status);
    } else {
        item->exitcode = 1;
    }
    item->leader_done = 1;
    return 0;
}

static int reap_async_group_children(pid_t pid) {
    int status;
    pid_t got;
    for (;;) {
        got = waitpid(-pid, &status, WNOHANG);
        if (got > 0) continue;
        if (got == 0 || (got < 0 && errno == ECHILD)) return 0;
        if (got < 0 && errno == EINTR) continue;
        return errno;
    }
}

#ifdef __linux__
static void reap_async_session_children(pid_t session, pid_t leader) {
    DIR *directory = opendir("/proc");
    struct dirent *entry;

    if (directory == NULL) return;
    while ((entry = readdir(directory)) != NULL) {
        char *end;
        long value = strtol(entry->d_name, &end, 10);
        pid_t current;
        if (*entry->d_name == '\0' || *end != '\0' || value <= 0 ||
            value > INT_MAX || (pid_t)value == leader) continue;
        if (linux_process_session((pid_t)value, &current) == 0 &&
            current == session) (void)waitpid((pid_t)value, NULL, WNOHANG);
    }
    closedir(directory);
}
#endif

static int terminate_async_group(struct async_process *item) {
    struct timespec now, deadline;
    int error, exists;

    if (!item->leader_done && !async_identity_matches(item)) return ESRCH;
    exists = async_owner_exists(item);
    if (exists < 0) return errno;
    if (!exists) return 0;
    error = signal_async_owner(item, SIGTERM);
    if (error != 0) return error;
    clock_gettime(CLOCK_MONOTONIC, &deadline);
    add_seconds(&deadline, 2);
    for (;;) {
        error = observe_async_leader(item);
        if (error != 0) return error;
        if (item->leader_done) {
            if (item->owns_session) {
#ifdef __linux__
                reap_async_session_children(item->session, item->pid);
#endif
            } else {
                error = reap_async_group_children(item->pid);
                if (error != 0) return error;
            }
        }
        exists = async_owner_exists(item);
        if (exists < 0) return errno;
        if (!exists) return 0;
        clock_gettime(CLOCK_MONOTONIC, &now);
        if (timespec_at_or_after(&now, &deadline)) break;
        sleep_ms(25);
    }

    exists = async_owner_exists(item);
    if (exists < 0) return errno;
    if (exists) {
        error = signal_async_owner(item, SIGKILL);
        if (error != 0) return error;
    }
    clock_gettime(CLOCK_MONOTONIC, &deadline);
    add_seconds(&deadline, 1);
    for (;;) {
        error = observe_async_leader(item);
        if (error != 0) return error;
        if (item->leader_done) {
            if (item->owns_session) {
#ifdef __linux__
                reap_async_session_children(item->session, item->pid);
#endif
            } else {
                error = reap_async_group_children(item->pid);
                if (error != 0) return error;
            }
        }
        exists = async_owner_exists(item);
        if (exists < 0) return errno;
        if (!exists) return 0;
        clock_gettime(CLOCK_MONOTONIC, &now);
        if (timespec_at_or_after(&now, &deadline)) return ETIMEDOUT;
        sleep_ms(10);
    }
}

static void start_argv_session(const char *cwd, const char *args, int args_len,
                               int n_args, const char *log_file,
                               const char *env_extra, int *pid_out,
                               int *exitcode) {
    char **argv = NULL;
    char **child_env = NULL;
    const char *p, *end;
    int index, ready_pipe[2], gate_pipe[2], child_error = 0, reaper_error;
    pid_t pid;
    uint64_t identity;
    struct async_process *item;

    *pid_out = 0;
    *exitcode = 0;
    if (n_args <= 0 || args == NULL || args_len <= 0 || !has_text(log_file)) {
        *exitcode = EINVAL;
        return;
    }
    argv = calloc((size_t)n_args + 1, sizeof(char *));
    if (argv == NULL) {
        *exitcode = ENOMEM;
        return;
    }
    p = args;
    end = args + args_len;
    for (index = 0; index < n_args && p < end; index++) {
        size_t remaining = (size_t)(end - p);
        size_t token_len = strnlen(p, remaining);
        if (token_len == remaining) break;
        argv[index] = (char *)p;
        p += token_len + 1;
    }
    if (index != n_args) {
        free(argv);
        *exitcode = EINVAL;
        return;
    }
    if (has_text(env_extra)) {
        child_env = env_with_overrides(env_extra);
        if (child_env == NULL) {
            free(argv);
            *exitcode = errno != 0 ? errno : ENOMEM;
            return;
        }
    }
    reaper_error = ensure_async_subreaper();
    if (reaper_error != 0) {
        free_env_copy(child_env);
        free(argv);
        *exitcode = reaper_error;
        return;
    }
    if (pipe(ready_pipe) != 0) {
        free_env_copy(child_env);
        free(argv);
        *exitcode = errno;
        return;
    }
    if (pipe(gate_pipe) != 0) {
        *exitcode = errno;
        close(ready_pipe[0]);
        close(ready_pipe[1]);
        free_env_copy(child_env);
        free(argv);
        return;
    }
    pid = fork();
    if (pid < 0) {
        *exitcode = errno;
        close(ready_pipe[0]); close(ready_pipe[1]);
        close(gate_pipe[0]); close(gate_pipe[1]);
        free_env_copy(child_env);
        free(argv);
        return;
    }
    if (pid == 0) {
        int fd, release, nested = 0;
        close(ready_pipe[0]);
        close(gate_pipe[1]);
        if (setsid() < 0) {
            /* The inherited owner filter keeps this child in its session.
               A fresh process group gives the nested job its own handle. */
            if (errno != EPERM || setpgid(0, 0) != 0) child_error = errno;
            else nested = 1;
        }
        if (child_error == 0 && nested) child_error = ensure_async_subreaper();
        if (child_error == 0)
            child_error = install_async_group_containment(nested);
        if (child_error == 0 && has_text(cwd) && chdir(cwd) != 0) {
            child_error = errno;
        }
        if (child_error == 0 && has_text(log_file)) {
            fd = open(log_file, O_WRONLY | O_CREAT | O_TRUNC, 0666);
            if (fd < 0) child_error = errno;
            else {
                if (dup2(fd, STDOUT_FILENO) < 0 ||
                    dup2(fd, STDERR_FILENO) < 0) child_error = errno;
                close(fd);
            }
        }
        if (write_exact(ready_pipe[1], &child_error, sizeof(child_error)) != 0) {
            _exit(126);
        }
        close(ready_pipe[1]);
        if (child_error != 0) _exit(126);
        if (read_exact(gate_pipe[0], &release, sizeof(release)) != 0) _exit(126);
        close(gate_pipe[0]);
        if (child_env != NULL) environ = child_env;
        execvp(argv[0], argv);
        _exit(errno == ENOENT ? 127 : 126);
    }

    close(ready_pipe[1]);
    close(gate_pipe[0]);
    if (read_exact(ready_pipe[0], &child_error, sizeof(child_error)) != 0 ||
        child_error != 0) {
        close(ready_pipe[0]); close(gate_pipe[1]);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(argv);
        *exitcode = child_error != 0 ? child_error : EIO;
        return;
    }
    close(ready_pipe[0]);
    identity = process_start_identity(pid);
#if defined(__linux__) || defined(__APPLE__)
    if (identity == 0) {
        close(gate_pipe[1]);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(argv);
        *exitcode = EIO;
        return;
    }
#endif
    if (getpgid(pid) != pid || getsid(pid) <= 0) {
        close(gate_pipe[1]);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(argv);
        *exitcode = EIO;
        return;
    }
    item = calloc(1, sizeof(*item));
    if (item == NULL) {
        close(gate_pipe[1]);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(argv);
        *exitcode = ENOMEM;
        return;
    }
    item->pid = pid;
    item->session = getsid(pid);
    item->owns_session = item->session == pid;
    item->start_identity = identity;
    item->next = async_processes;
    async_processes = item;
    {
        int release = 1;
        if (write_exact(gate_pipe[1], &release, sizeof(release)) != 0) {
            close(gate_pipe[1]);
            forget_async_process(item);
            (void)kill(pid, SIGKILL);
            while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
            }
            free_env_copy(child_env);
            free(argv);
            *exitcode = EIO;
            return;
        }
    }
    close(gate_pipe[1]);
    free_env_copy(child_env);
    free(argv);
    *pid_out = (int)pid;
}

void fo_c_start_argv_logged(const char *cwd, const char *args, int args_len,
                            int n_args, const char *log_file,
                            const char *env_extra, int *pid_out, int *exitcode) {
    start_argv_session(cwd, args, args_len, n_args, log_file, env_extra,
                       pid_out, exitcode);
}

void fo_c_start_fo_check(const char *project_dir, const char *mode,
                         const char *output_file, int *pid_out,
                         int *exitcode) {
    static const char args_agent[] = "fo\0check\0--agent\0";
    static const char args_json[] = "fo\0check\0--json\0";
    static const char args_full[] = "fo\0check\0--json=full\0";
    const char *packed;
    int args_len;

    if (!has_text(project_dir) || !has_text(output_file)) {
        *pid_out = 0;
        *exitcode = EINVAL;
        return;
    }
    if (strcmp(mode, "full") == 0 || strcmp(mode, "json=full") == 0) {
        packed = args_full;
        args_len = (int)sizeof(args_full) - 1;
    } else if (strcmp(mode, "json") == 0) {
        packed = args_json;
        args_len = (int)sizeof(args_json) - 1;
    } else {
        packed = args_agent;
        args_len = (int)sizeof(args_agent) - 1;
    }
    fo_c_start_argv_logged(project_dir, packed, args_len, 3, output_file, NULL,
                           pid_out, exitcode);
}

void fo_c_poll_pid(int pid, int *done, int *exitcode) {
    struct async_process *item;
    int error;

    *done = 0;
    *exitcode = 0;
    item = pid > 0 ? find_async_process((pid_t)pid) : NULL;
    if (item == NULL) {
        *done = 1;
        *exitcode = ESRCH;
        return;
    }
    if (!item->leader_done && !async_identity_matches(item)) {
        *done = 1;
        *exitcode = ESRCH;
        forget_async_process(item);
        return;
    }
    error = observe_async_leader(item);
    if (error != 0) {
        *done = 1;
        *exitcode = error;
        forget_async_process(item);
        return;
    }
    if (!item->leader_done) return;
    error = terminate_async_group(item);
    if (error != 0) {
        *exitcode = error;
        return;
    }
    *done = 1;
    *exitcode = item->exitcode;
    forget_async_process(item);
}

void fo_c_cancel_pid(int pid, int *exitcode) {
    struct async_process *item;
    int error;

    *exitcode = 0;
    item = pid > 0 ? find_async_process((pid_t)pid) : NULL;
    if (item == NULL) {
        *exitcode = ESRCH;
        return;
    }
    if (!item->leader_done && !async_identity_matches(item)) {
        *exitcode = ESRCH;
        forget_async_process(item);
        return;
    }
    error = terminate_async_group(item);
    if (error != 0) {
        *exitcode = error;
        return;
    }
    forget_async_process(item);
}

/* Progress output helpers. isatty(2) lets the caller pick an animated bar vs
 * plain lines; fo_c_write_stderr does a raw, unbuffered write(2) to fd 2 so a
 * carriage-return progress line renders cleanly without Fortran record
 * formatting, and a single write() is atomic enough for one-thread-at-a-time
 * (the caller serializes it in an OpenMP critical). No fork: forking from a
 * multithreaded region corrupts libgomp. */
int fo_c_isatty(int fd) { return isatty(fd) ? 1 : 0; }

void fo_c_write_stderr(const char *buf, int n) {
    int off = 0;
    if (n <= 0) return;
    while (off < n) {
        ssize_t w = write(2, buf + off, (size_t)(n - off));
        if (w < 0) {
            if (errno == EINTR) continue;
            break;
        }
        off += (int)w;
    }
}

#ifdef __linux__
static volatile sig_atomic_t monitor_stop = 0;

static void monitor_term(int signal_number) {
    (void)signal_number;
    monitor_stop = 1;
}

/* The monitor is a fresh exec and the only subreaper for this command.
   Double-forked orphans become its direct children, so no other command's
   descendants can be mistaken for this one's. */
static int monitor_kill_children(void) {
    char path[96];
    struct timespec start, now;
    int count;

    snprintf(path, sizeof(path), "/proc/self/task/%ld/children", (long)getpid());
    clock_gettime(CLOCK_MONOTONIC, &start);
    do {
        FILE *file = fopen(path, "r");
        long child;
        count = 0;
        if (file == NULL) return errno;
        while (fscanf(file, "%ld", &child) == 1) {
            if (child > 0 && child <= INT_MAX) {
                (void)kill((pid_t)child, SIGKILL);
                count++;
            }
        }
        fclose(file);
        while (waitpid(-1, NULL, WNOHANG) > 0) {
        }
        if (count == 0) return 0;
        clock_gettime(CLOCK_MONOTONIC, &now);
        if (now.tv_sec - start.tv_sec >= 2) return ETIMEDOUT;
        sleep_ms(10);
    } while (count > 0);
    return 0;
}

static int monitor_run(int argc, char **argv) {
    posix_spawn_file_actions_t actions;
    struct sigaction action = {0};
    struct rusage usage = {0};
    pid_t target = 0;
    int error, status = 1, code = 1;
    long long cpu_ms = -1;

    if (argc < 6 || prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0) return 126;
    action.sa_handler = monitor_term;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGTERM, &action, NULL) != 0) return 126;
    error = posix_spawn_file_actions_init(&actions);
    if (error != 0) return 126;
    if (has_text(argv[2]))
        error = posix_spawn_file_actions_addchdir_np(&actions, argv[2]);
    if (error == 0 && has_text(argv[3])) {
        int flags = O_WRONLY | O_CREAT |
                    (argv[4][0] == '1' ? O_APPEND : O_TRUNC);
        error = posix_spawn_file_actions_addopen(
            &actions, STDOUT_FILENO, argv[3], flags, 0666);
        if (error == 0)
            error = posix_spawn_file_actions_adddup2(
                &actions, STDOUT_FILENO, STDERR_FILENO);
    }
    if (error == 0)
        error = posix_spawn_file_actions_addclose(&actions, FO_MONITOR_FD);
    if (error == 0)
        error = posix_spawnp(&target, argv[5], &actions, NULL,
                             argv + 5, environ);
    posix_spawn_file_actions_destroy(&actions);
    if (error != 0) target = -(pid_t)error;
    (void)write_exact(FO_MONITOR_FD, &target, sizeof(target));
    if (error != 0) {
        emit_spawn_error(argv[3], "nested command", error);
        close(FO_MONITOR_FD);
        return error == ENOENT ? 127 : 126;
    }
    for (;;) {
        pid_t waited;
        if (monitor_stop) break;
        waited = wait4(target, &status, WNOHANG, &usage);
        if (waited == target) {
            cpu_ms = (long long)(usage.ru_utime.tv_sec +
                                 usage.ru_stime.tv_sec) * 1000LL +
                     (long long)(usage.ru_utime.tv_usec +
                                 usage.ru_stime.tv_usec) / 1000LL;
            code = WIFEXITED(status) ? WEXITSTATUS(status) :
                   WIFSIGNALED(status) ? 128 + WTERMSIG(status) : 1;
            break;
        }
        if (waited < 0 && errno != EINTR) break;
        sleep_ms(10);
    }
    if (monitor_kill_children() != 0) {
        if (getpgrp() == getppid()) (void)kill(-getpgrp(), SIGKILL);
        code = 126;
    }
    (void)write_exact(FO_MONITOR_FD, &cpu_ms, sizeof(cpu_ms));
    close(FO_MONITOR_FD);
    return code;
}

__attribute__((constructor))
static void fo_process_monitor_entry(int argc, char **argv, char **envp) {
    (void)envp;
    if (argc >= 6 && argv[1] != NULL &&
        strcmp(argv[1], FO_MONITOR_ARG) == 0 &&
        fcntl(FO_MONITOR_FD, F_GETFD) >= 0)
        _exit(monitor_run(argc, argv));
}
#endif
