#define _GNU_SOURCE

#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

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

/* SIGTERM the child's process group, give it up to three seconds to exit,
   then SIGKILL the group and reap the child. */
static void kill_group_and_reap(pid_t pid, int *status) {
    int reaped = 0;
    pid_t waited;

    kill(-pid, SIGTERM);
    for (int k = 0; k < 15; k++) {
        waited = waitpid(pid, status, WNOHANG);
        if (waited == pid) {
            reaped = 1;
            break;
        }
        if (waited < 0 && errno != EINTR) break;
        sleep_ms(200);
    }
    kill(-pid, SIGKILL);
    if (!reaped) {
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
    }
}

static int run_argv(const char *cwd, char *const argv[], const char *log_file,
                    int append, int jobs, int timeout_s, int heartbeat_s,
                    const char *env_extra, struct run_budget *budget) {
    pid_t pid;
    int status;
    int pid_fd = -1;
    int spawn_error;
    int attrs_ready = 0;
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
                long long own = budget != NULL ? child_cpu_ms(pid) : -1;

                while (wait4(pid, &status, 0, &usage) < 0) {
                    if (errno == EINTR) continue;
                    if (pid_fd >= 0) close(pid_fd);
                    return 1;
                }
                if (budget != NULL) {
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
                return 1;
            }

            clock_gettime(CLOCK_MONOTONIC, &now);
            if (timespec_at_or_after(&now, &deadline)) {
                record_timeout(budget, 2, child_cpu_ms(pid), &start, &now);
                kill_group_and_reap(pid, &status);
                if (pid_fd >= 0) close(pid_fd);
                return 124;
            }
            if (cpu_s > 0 && timespec_at_or_after(&now, &cpu_check)) {
                long long used = child_cpu_ms(pid);
                if (used < 0 || used >= (long long)cpu_s * 1000LL) {
                    record_timeout(budget, used < 0 ? 3 : 1, used, &start, &now);
                    kill_group_and_reap(pid, &status);
                    if (pid_fd >= 0) close(pid_fd);
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
                    return 1;
                }
            } else {
                sleep_ms(1);
            }
        }
    } else {
        while (waitpid(pid, &status, 0) < 0) {
            if (errno == EINTR) continue;
            return 1;
        }
    }
    if (pid_fd >= 0) close(pid_fd);
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

void fo_c_start_fo_check(const char *project_dir, const char *mode,
                         const char *output_file, int *pid_out,
                         int *exitcode) {
    pid_t pid;

    *pid_out = 0;
    *exitcode = 0;
    if (!has_text(project_dir) || !has_text(output_file)) {
        *exitcode = 1;
        return;
    }

    pid = fork();
    if (pid < 0) {
        *exitcode = 1;
        return;
    }

    if (pid == 0) {
        int fd;
        char *argv_agent[] = {"fo", "check", "--agent", NULL};
        char *argv_full[] = {"fo", "check", "--json=full", NULL};
        char *argv_json[] = {"fo", "check", "--json", NULL};
        char **argv = argv_agent;

        if (chdir(project_dir) != 0) _exit(127);
        fd = open(output_file, O_WRONLY | O_CREAT | O_TRUNC, 0666);
        if (fd < 0) _exit(126);
        if (dup2(fd, STDOUT_FILENO) < 0) _exit(126);
        if (dup2(fd, STDERR_FILENO) < 0) _exit(126);
        close(fd);

        if (strcmp(mode, "full") == 0 || strcmp(mode, "json=full") == 0) {
            argv = argv_full;
        } else if (strcmp(mode, "json") == 0) {
            argv = argv_json;
        }
        execvp(argv[0], argv);
        _exit(errno == ENOENT ? 127 : 126);
    }

    *pid_out = (int)pid;
}

void fo_c_poll_pid(int pid, int *done, int *exitcode) {
    int status;
    pid_t got;

    *done = 0;
    *exitcode = 0;
    if (pid <= 0) {
        *done = 1;
        *exitcode = 1;
        return;
    }

    got = waitpid((pid_t)pid, &status, WNOHANG);
    if (got == 0) return;
    *done = 1;
    if (got < 0) {
        *exitcode = 1;
    } else if (WIFEXITED(status)) {
        *exitcode = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        *exitcode = 128 + WTERMSIG(status);
    } else {
        *exitcode = 1;
    }
}

void fo_c_cancel_pid(int pid, int *exitcode) {
    int status;

    *exitcode = 0;
    if (pid <= 0) return;
    if (kill((pid_t)pid, SIGTERM) != 0 && errno != ESRCH) {
        *exitcode = 1;
        return;
    }
    if (waitpid((pid_t)pid, &status, 0) < 0 && errno != ECHILD) {
        *exitcode = 1;
    }
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
