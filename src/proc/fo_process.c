#ifdef __APPLE__
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#else
#define _GNU_SOURCE
#endif

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
#include <sys/proc.h>
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

struct owned_process_identity {
    pid_t pid;
    uint64_t start;
    struct owned_process_identity *next;
};

struct async_process {
    pid_t pid;
    pid_t session;
    int owns_session;
    int owns_group;
    uint64_t start_identity;
    int leader_done;
    int exitcode;
    char *registry_path;
    char registry_dir[PATH_MAX];
    char scope_owner_start[64];
    char start_identity_text[64];
    struct timespec last_tree_scan;
    struct owned_process_identity *members;
    struct async_process *next;
};

static struct async_process *async_processes = NULL;

int fo_gremlin_process_matches(int pid, const char *start);
static uint64_t process_start_identity(pid_t pid);
int fo_c_process_containment_required(void);

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

struct scan_directory_identity {
    dev_t device;
    ino_t inode;
};

struct scan_directory_set {
    struct scan_directory_identity *items;
    size_t n;
    size_t cap;
};

#define FO_SCAN_PATH_CAPACITY 4096

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

static int join_scan_path(char *path, size_t capacity, const char *dir,
                          const char *name) {
    int length = snprintf(path, capacity, "%s/%s", dir, name);
    if (length < 0 || (size_t)length >= capacity) {
        errno = ENAMETOOLONG;
        return -1;
    }
    return 0;
}

static void report_scan_path_too_long(void) {
    static const char message[] =
        "fo: source scan path exceeds supported length\n";
    (void)write(STDERR_FILENO, message, sizeof(message) - 1);
}

static int is_project_root(const char *dir, int *is_root) {
    char path[FO_SCAN_PATH_CAPACITY];
    struct stat st;

    if (join_scan_path(path, sizeof(path), dir, "fpm.toml") != 0) return -1;
    if (stat(path, &st) == 0) {
        *is_root = 1;
        return 0;
    }
    if (errno == ENAMETOOLONG) return -1;
    *is_root = 0;
    return 0;
}

/* Returns 1 for an already visited directory, 0 for a new one, and -1 on
   allocation failure. Device and inode identify the target reached through a
   directory symlink, so a cycle cannot recurse indefinitely. */
static int scan_directory_set_add(struct scan_directory_set *set,
                                  const struct stat *info) {
    struct scan_directory_identity *next;
    size_t i;

    for (i = 0; i < set->n; i++) {
        if (set->items[i].device == info->st_dev &&
            set->items[i].inode == info->st_ino) {
            return 1;
        }
    }
    if (set->n == set->cap) {
        size_t next_cap = set->cap == 0 ? 64 : set->cap * 2;
        if (next_cap < set->cap ||
            next_cap > SIZE_MAX / sizeof(*set->items)) {
            return -1;
        }
        next = realloc(set->items, next_cap * sizeof(*set->items));
        if (next == NULL) return -1;
        set->items = next;
        set->cap = next_cap;
    }
    set->items[set->n].device = info->st_dev;
    set->items[set->n].inode = info->st_ino;
    set->n++;
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
                                  struct scan_directory_set *visited,
                                  int required, int is_proj_root, int depth) {
    DIR *handle;
    struct dirent *entry;
    struct stat dir_info;
    int directory_fd, seen;

    handle = opendir(dir);
    if (handle == NULL) {
        if (errno == ENAMETOOLONG) {
            report_scan_path_too_long();
            return 1;
        }
        return required ? 1 : 0;
    }
    directory_fd = dirfd(handle);
    if (directory_fd < 0 || fstat(directory_fd, &dir_info) != 0) {
        closedir(handle);
        return 1;
    }
    seen = scan_directory_set_add(visited, &dir_info);
    if (seen != 0) {
        closedir(handle);
        return seen < 0 ? 1 : 0;
    }

    while ((entry = readdir(handle)) != NULL) {
        char path[FO_SCAN_PATH_CAPACITY];
        struct stat st;

        if (skip_dir_name(entry->d_name, is_proj_root, depth)) continue;
        if (join_scan_path(path, sizeof(path), dir, entry->d_name) != 0) {
            report_scan_path_too_long();
            closedir(handle);
            return 1;
        }
        if (stat(path, &st) != 0) {
            if (errno == ENAMETOOLONG) {
                report_scan_path_too_long();
                closedir(handle);
                return 1;
            }
            continue;
        }
        if (S_ISDIR(st.st_mode)) {
            int nested_project = 0;
            if (depth >= 0 && is_project_root(path, &nested_project) != 0) {
                report_scan_path_too_long();
                closedir(handle);
                return 1;
            }
            if (depth >= 0 && nested_project) continue;
            if (scan_sources_recursive(path, list, visited, 0, is_proj_root,
                                       depth + 1) != 0) {
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
    struct scan_directory_set visited = {0};
    FILE *out;
    size_t i;
    int proj_root;

    *exitcode = 0;
    if (!has_text(root) || !has_text(output_file)) {
        *exitcode = 1;
        return;
    }
    if (is_project_root(root, &proj_root) != 0) {
        report_scan_path_too_long();
        *exitcode = 1;
        return;
    }
    if (scan_sources_recursive(root, &list, &visited, 1, proj_root, 0) != 0) {
        free(visited.items);
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

    free(visited.items);
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

/* Build a complete environment from individual KEY=VALUE overrides. Unlike
   the older semicolon form, values here may contain semicolons. */
static char **env_with_vector(char *const overrides[]) {
    size_t base_count = 0, override_count = 0, used = 0, extras_start, i;
    char **env;

    while (environ[base_count] != NULL) base_count++;
    if (overrides != NULL)
        while (overrides[override_count] != NULL) override_count++;
    env = calloc(base_count + override_count + 1, sizeof(*env));
    if (env == NULL) return NULL;
    for (i = 0; i < base_count; i++) {
        size_t j;
        int replaced = 0;
        for (j = 0; j < override_count; j++) {
            const char *equals = strchr(overrides[j], '=');
            if (equals == NULL || equals == overrides[j]) goto invalid;
            if (env_name_matches(environ[i], overrides[j],
                                 (size_t)(equals - overrides[j]))) {
                replaced = 1;
                break;
            }
        }
        if (!replaced) {
            env[used] = strdup(environ[i]);
            if (env[used] == NULL) goto allocation_failed;
            used++;
        }
    }
    extras_start = used;
    for (i = 0; i < override_count; i++) {
        const char *equals = strchr(overrides[i], '=');
        size_t j, key_len;
        if (equals == NULL || equals == overrides[i]) goto invalid;
        key_len = (size_t)(equals - overrides[i]);
        for (j = extras_start; j < used; j++) {
            if (env_name_matches(env[j], overrides[i], key_len)) break;
        }
        if (j < used) {
            free(env[j]);
            env[j] = strdup(overrides[i]);
            if (env[j] == NULL) goto allocation_failed;
        } else {
            env[used] = strdup(overrides[i]);
            if (env[used] == NULL) goto allocation_failed;
            used++;
        }
    }
    return env;

invalid:
    errno = EINVAL;
allocation_failed:
    for (i = 0; i < used; i++) free(env[i]);
    free(env);
    return NULL;
}

static void free_env_vector(char **env) {
    size_t i;
    if (env == NULL) return;
    for (i = 0; env[i] != NULL; i++) free(env[i]);
    free(env);
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
                                 int capture_monitor,
                                 char **child_env, const char *monitor_executable,
                                 int stdin_fd, int stdout_fd, int stderr_fd,
                                 pid_t *monitor_pid,
                                 pid_t *target_pid, int *report_fd) {
    posix_spawn_file_actions_t actions;
    char **monitor_argv;
    int pipefd[2], stream_fd[3] = {-1, -1, -1};
    int error, ready, actions_ready = 0;
    size_t count = 0;

    *target_pid = 0;
    if (monitor_executable == NULL || monitor_executable[0] != '/') return EINVAL;
    if (stdin_fd >= 0 || stdout_fd >= 0 || stderr_fd >= 0) {
        if (stdin_fd < 0 || stdout_fd < 0 || stderr_fd < 0) return EINVAL;
        stream_fd[0] = fcntl(stdin_fd, F_DUPFD_CLOEXEC, 16);
        stream_fd[1] = fcntl(stdout_fd, F_DUPFD_CLOEXEC, 16);
        stream_fd[2] = fcntl(stderr_fd, F_DUPFD_CLOEXEC, 16);
        if (stream_fd[0] < 0 || stream_fd[1] < 0 || stream_fd[2] < 0) {
            error = errno;
            for (size_t i = 0; i < 3; i++) if (stream_fd[i] >= 0) close(stream_fd[i]);
            return error;
        }
    }
    while (argv[count] != NULL) count++;
    monitor_argv = calloc(count + 6, sizeof(*monitor_argv));
    if (monitor_argv == NULL) {
        for (size_t i = 0; i < 3; i++) if (stream_fd[i] >= 0) close(stream_fd[i]);
        return ENOMEM;
    }
    monitor_argv[0] = (char *)monitor_executable;
    monitor_argv[1] = FO_MONITOR_ARG;
    monitor_argv[2] = (char *)(cwd != NULL ? cwd : "");
    monitor_argv[3] = (char *)(log_file != NULL ? log_file : "");
    monitor_argv[4] = capture_monitor ? "C" : (append ? "1" : "0");
    for (size_t i = 0; i < count; i++) monitor_argv[i + 5] = argv[i];
    if (pipe2(pipefd, O_CLOEXEC) != 0) {
        error = errno;
        free(monitor_argv);
        for (size_t i = 0; i < 3; i++) if (stream_fd[i] >= 0) close(stream_fd[i]);
        return error;
    }
    for (int i = 0; i < 2; i++) {
        if (pipefd[i] == FO_MONITOR_FD) {
            int moved = fcntl(pipefd[i], F_DUPFD_CLOEXEC, FO_MONITOR_FD + 1);
            if (moved < 0) {
                error = errno;
                close(pipefd[0]); close(pipefd[1]);
                free(monitor_argv);
                for (size_t j = 0; j < 3; j++) if (stream_fd[j] >= 0) close(stream_fd[j]);
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
    for (int i = 0; error == 0 && i < 3; i++) {
        if (stream_fd[i] >= 0)
            error = posix_spawn_file_actions_adddup2(&actions, stream_fd[i], i);
    }
    for (int i = 0; error == 0 && i < 3; i++) {
        if (stream_fd[i] >= 0)
            error = posix_spawn_file_actions_addclose(&actions, stream_fd[i]);
    }
    if (error == 0)
        error = posix_spawn_file_actions_addclose(&actions, pipefd[0]);
    if (error == 0)
        error = posix_spawn_file_actions_addclose(&actions, pipefd[1]);
    if (error == 0)
        error = posix_spawn(monitor_pid, monitor_executable, &actions, NULL,
                            monitor_argv, child_env ? child_env : environ);
    if (actions_ready) posix_spawn_file_actions_destroy(&actions);
    free(monitor_argv);
    for (size_t i = 0; i < 3; i++) if (stream_fd[i] >= 0) close(stream_fd[i]);
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
        (void)kill(*monitor_pid, SIGTERM);
        while (waitpid(*monitor_pid, NULL, 0) < 0 && errno == EINTR) {
        }
        close(pipefd[0]);
        return launch_error;
    }
    *report_fd = pipefd[0];
    return 0;
}
#endif

#if defined(__APPLE__) || defined(__linux__)
struct owned_process_record {
    pid_t pid;
    pid_t parent;
    pid_t group;
    pid_t session;
    uint64_t start;
    uint64_t parent_start;
    int owned;
    int alive;
};

static int owned_process_details(pid_t pid, pid_t *parent, pid_t *group,
                                  pid_t *session, uint64_t *start, int *alive) {
#ifdef __APPLE__
    struct proc_bsdinfo info;
    pid_t sid;
    int bytes;

    if (pid <= 0) return ESRCH;
    bytes = proc_pidinfo((int)pid, PROC_PIDTBSDINFO, 0, &info,
                         (int)sizeof(info));
    if (bytes != (int)sizeof(info) || info.pbi_pid != (uint32_t)pid) return ESRCH;
    sid = getsid(pid);
    if (sid <= 0 && info.pbi_status != SZOMB)
        return errno != 0 ? errno : ESRCH;
    if (parent != NULL) *parent = (pid_t)info.pbi_ppid;
    if (group != NULL) *group = (pid_t)info.pbi_pgid;
    if (session != NULL) *session = sid;
    if (alive != NULL) *alive = info.pbi_status != SZOMB;
    if (start != NULL) {
        *start = (uint64_t)info.pbi_start_tvsec * 1000000ULL +
                 (uint64_t)info.pbi_start_tvusec;
        if (*start == 0) return ESRCH;
    }
    return 0;
#else
    char path[64], line[4096], *end, *cursor;
    char state;
    long ppid, pgid, sid;
    uint64_t identity = 0;
    int field;
    FILE *file;
    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    file = fopen(path, "r");
    if (file == NULL) return errno;
    if (fgets(line, sizeof(line), file) == NULL) {
        fclose(file);
        return ESRCH;
    }
    fclose(file);
    end = strrchr(line, ')');
    if (end == NULL || sscanf(end + 1, " %c %ld %ld %ld",
                              &state, &ppid, &pgid, &sid) != 4) return EIO;
    cursor = end + 1;
    for (field = 3; field <= 22; field++) {
        while (*cursor == ' ') cursor++;
        if (*cursor == '\0') return EIO;
        end = cursor;
        while (*end != '\0' && *end != ' ') end++;
        if (field == 22) {
            identity = strtoull(cursor, NULL, 10);
            break;
        }
        cursor = end;
    }
    if (identity == 0) return ESRCH;
    if (parent != NULL) *parent = (pid_t)ppid;
    if (group != NULL) *group = (pid_t)pgid;
    if (session != NULL) *session = (pid_t)sid;
    if (start != NULL) *start = identity;
    if (alive != NULL) *alive = state != 'Z' && state != 'X';
    return 0;
#endif
}

static int owned_list_processes(pid_t **pids_out, size_t *count_out) {
#ifdef __APPLE__
    pid_t *pids = NULL;
    size_t capacity;
    int required, bytes;

    required = proc_listpids(PROC_ALL_PIDS, 0, NULL, 0);
    if (required < 0) return errno != 0 ? errno : EIO;
    capacity = (size_t)required / sizeof(pid_t) + 64;
    if (capacity < 64) capacity = 64;
    for (;;) {
        pid_t *next;
        size_t buffer_size;
        if (capacity > (size_t)INT_MAX / sizeof(pid_t)) {
            free(pids);
            return EOVERFLOW;
        }
        buffer_size = capacity * sizeof(pid_t);
        next = realloc(pids, buffer_size);
        if (next == NULL) {
            free(pids);
            return ENOMEM;
        }
        pids = next;
        bytes = proc_listpids(PROC_ALL_PIDS, 0, pids, (int)buffer_size);
        if (bytes < 0) {
            free(pids);
            return errno != 0 ? errno : EIO;
        }
        if ((size_t)bytes < buffer_size) {
            if (bytes % (int)sizeof(pid_t) != 0) {
                free(pids);
                return EIO;
            }
            *pids_out = pids;
            *count_out = (size_t)bytes / sizeof(pid_t);
            return 0;
        }
        if (capacity > SIZE_MAX / 2) {
            free(pids);
            return EOVERFLOW;
        }
        capacity *= 2;
    }
#else
    DIR *directory = opendir("/proc");
    struct dirent *entry;
    pid_t *pids = NULL;
    size_t used = 0, capacity = 0;
    if (directory == NULL) return errno;
    while ((entry = readdir(directory)) != NULL) {
        char *end;
        long value = strtol(entry->d_name, &end, 10);
        if (*entry->d_name == '\0' || *end != '\0' ||
            value <= 0 || value > INT_MAX) continue;
        if (used == capacity) {
            size_t next_capacity = capacity == 0 ? 64 : capacity * 2;
            pid_t *next = realloc(pids, next_capacity * sizeof(*pids));
            if (next == NULL) { free(pids); closedir(directory); return ENOMEM; }
            pids = next;
            capacity = next_capacity;
        }
        pids[used++] = (pid_t)value;
    }
    closedir(directory);
    *pids_out = pids;
    *count_out = used;
    return 0;
#endif
}

static int owned_record_compare(const void *left, const void *right) {
    const struct owned_process_record *a = left, *b = right;
    return (a->pid > b->pid) - (a->pid < b->pid);
}

static size_t owned_find_record(const struct owned_process_record *records,
                                 size_t count, pid_t pid) {
    size_t low = 0, high = count;
    while (low < high) {
        size_t middle = low + (high - low) / 2;
        if (records[middle].pid < pid) low = middle + 1;
        else high = middle;
    }
    return low < count && records[low].pid == pid ? low : SIZE_MAX;
}

static int collect_owned_tree(pid_t root, uint64_t root_start,
                                     pid_t session, int owns_session,
                                     const struct owned_process_identity *seeds,
                                     struct owned_process_record **records_out,
                                     size_t *count_out) {
    pid_t *pids = NULL;
    struct owned_process_record *records = NULL;
    size_t count = 0, used = 0, root_index, i;
    int error = owned_list_processes(&pids, &count);

    if (error != 0) return error;
    records = calloc(count > 0 ? count : 1, sizeof(*records));
    if (records == NULL) { free(pids); return ENOMEM; }
    for (i = 0; i < count; i++) {
        struct owned_process_record item;
        if (owned_process_details(pids[i], &item.parent, &item.group,
                                   &item.session, &item.start, &item.alive) != 0) continue;
        item.pid = pids[i];
        item.owned = 0;
        records[used++] = item;
    }
    free(pids);
    qsort(records, used, sizeof(*records), owned_record_compare);
    root_index = owned_find_record(records, used, root);
    /* An absent or reused root never establishes numeric SID ownership. */
    if (root_index != SIZE_MAX && records[root_index].start != root_start)
        root_index = SIZE_MAX;
    for (i = 0; i < used; i++) {
        size_t parent_index = owned_find_record(records, used,
                                                 records[i].parent);
        records[i].parent_start = parent_index == SIZE_MAX ? 0 :
                                  records[parent_index].start;
    }
    if (root_index != SIZE_MAX) {
        records[root_index].owned = 1;
        if (owns_session && records[root_index].group == root &&
            (records[root_index].session == session ||
             (!records[root_index].alive && session == root))) {
            for (i = 0; i < used; i++) {
                if (records[i].session == session) records[i].owned = 1;
            }
        }
    }
    for (; seeds != NULL; seeds = seeds->next) {
        size_t index = owned_find_record(records, used, seeds->pid);
        if (index != SIZE_MAX && records[index].start == seeds->start)
            records[index].owned = 1;
    }
    for (size_t depth = 0; depth < used; depth++) {
        int changed = 0;
        for (i = 0; i < used; i++) {
            size_t parent_index;
            pid_t current_parent;
            uint64_t current_start, parent_start;
            if (records[i].owned) continue;
            parent_index = owned_find_record(records, used, records[i].parent);
            if (parent_index == SIZE_MAX || !records[parent_index].owned) continue;
            if (owned_process_details(records[i].pid, &current_parent, NULL, NULL,
                                       &current_start, NULL) != 0 ||
                current_start != records[i].start ||
                current_parent != records[i].parent ||
                owned_process_details(records[i].parent, NULL, NULL, NULL,
                                       &parent_start, NULL) != 0 ||
                parent_start != records[i].parent_start) continue;
            records[i].owned = 1;
            changed = 1;
        }
        if (!changed) break;
    }
    *records_out = records;
    *count_out = used;
    return 0;
}

/* Only a matching birth identity seeds ownership. Session membership is an
   additional edge while its exact leader is present; after leader exit use
   captured member identities and current, birth-validated PPID lineage. */
static int signal_owned_tree(pid_t root, uint64_t root_start,
                                 pid_t session, int owns_session,
                                 const struct owned_process_identity *seeds,
                                 int signal_number) {
    struct owned_process_record *records = NULL;
    size_t count = 0, i;
    int error = collect_owned_tree(root, root_start, session, owns_session, seeds,
                                         &records, &count);
    int matched = 0;

    if (error != 0) { errno = error; return -1; }
    for (i = 0; i < count; i++) {
        uint64_t current_start;
        if (!records[i].owned || !records[i].alive) continue;
        matched++;
        if (signal_number == 0) continue;
        if (owned_process_details(records[i].pid, NULL, NULL, NULL,
                                   &current_start, NULL) != 0 ||
            current_start != records[i].start) continue;
#ifdef __linux__
#if defined(SYS_pidfd_open) && defined(SYS_pidfd_send_signal)
        {
            int fd = (int)syscall(SYS_pidfd_open, records[i].pid, 0);
            if (fd < 0) {
                if (errno == ESRCH) continue;
                error = errno; free(records); errno = error; return -1;
            }
            if (process_start_identity(records[i].pid) == records[i].start &&
                syscall(SYS_pidfd_send_signal, fd, signal_number, NULL, 0) != 0 &&
                errno != ESRCH) {
                error = errno; close(fd); free(records); errno = error; return -1;
            }
            close(fd);
        }
#else
        free(records);
        errno = ENOTSUP;
        return -1;
#endif
#else
        if (kill(records[i].pid, signal_number) != 0 && errno != ESRCH) {
            error = errno;
            free(records);
            errno = error;
            return -1;
        }
#endif
    }
    free(records);
    return matched;
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
        spawn_error = start_command_monitor(cwd, argv, log_file, append, 0,
                                            child_env, "/proc/self/exe", -1, -1, -1,
                                            &pid, &accounted_pid,
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
#if defined(FO_ASYNC_TEST_AARCH64) || defined(FO_ASYNC_TEST_ARM) || \
    defined(FO_ASYNC_TEST_X86_64) || defined(FO_ASYNC_TEST_UNSUPPORTED)
#define FO_ASYNC_TEST_SYNTHETIC_TARGET 1
#endif
#if defined(FO_ASYNC_TEST_UNSUPPORTED)
#define FO_ASYNC_AUDIT_ARCH 0
#elif defined(FO_ASYNC_TEST_AARCH64) || \
    (!defined(FO_ASYNC_TEST_SYNTHETIC_TARGET) && defined(__aarch64__))
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
#elif defined(FO_ASYNC_TEST_ARM) || \
    (!defined(FO_ASYNC_TEST_SYNTHETIC_TARGET) && defined(__arm__))
#define FO_ASYNC_AUDIT_ARCH AUDIT_ARCH_ARM
#define FO_ASYNC_NATIVE_SETSID 66
#define FO_ASYNC_NATIVE_SETPGID 57
#if defined(__arm__) && (__NR_setsid != 66 || __NR_setpgid != 57)
#error "ARM session syscall numbers differ from the containment filter"
#endif
#elif defined(FO_ASYNC_TEST_X86_64) || \
    (!defined(FO_ASYNC_TEST_SYNTHETIC_TARGET) && defined(__x86_64__))
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
#elif !defined(FO_ASYNC_TEST_SYNTHETIC_TARGET) && defined(__i386__)
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
#undef FO_ASYNC_TEST_SYNTHETIC_TARGET
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
#ifdef __APPLE__
    /* A fresh Darwin session is enumerable by SID. Keep rejecting the
       fallback process-group mode, which has no session-wide containment. */
    if (!strict_group) return 0;
#endif
    /* Without an inherited group-escape barrier, do not claim tree ownership. */
    (void)strict_group;
    return ENOTSUP;
#endif
}

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

static int process_identity_value_text(uint64_t identity, char *out, size_t cap) {
    int n;
    if (identity == 0) return ESRCH;
#ifdef __APPLE__
    n = snprintf(out, cap, "%llu.%06llu",
                 (unsigned long long)(identity / 1000000ULL),
                 (unsigned long long)(identity % 1000000ULL));
#else
    n = snprintf(out, cap, "%llu", (unsigned long long)identity);
#endif
    return n < 0 || (size_t)n >= cap ? ENAMETOOLONG : 0;
}

static int process_identity_text(pid_t pid, char *out, size_t cap) {
    return process_identity_value_text(process_start_identity(pid), out, cap);
}

static int parse_identity_text(const char *text, uint64_t *identity) {
    char *end;
    unsigned long long first;
#ifdef __APPLE__
    unsigned long long second;
#endif
    if (!has_text(text)) return EINVAL;
#ifdef __APPLE__
    first = strtoull(text, &end, 10);
    if (end == text || *end++ != '.' || strlen(end) != 6) return EINVAL;
    second = strtoull(end, &end, 10);
    if (*end != '\0' || second >= 1000000ULL ||
        first > (UINT64_MAX - second) / 1000000ULL) return EINVAL;
    *identity = (uint64_t)first * 1000000ULL + (uint64_t)second;
#else
    first = strtoull(text, &end, 10);
    if (end == text || *end != '\0') return EINVAL;
    *identity = (uint64_t)first;
#endif
    return *identity == 0 ? EINVAL : 0;
}

static int private_directory(const char *path, int create) {
    struct stat st;
    if (create && mkdir(path, 0700) != 0 && errno != EEXIST) return errno;
    if (lstat(path, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0077) != 0) return EPERM;
    return 0;
}

static int async_scope_owner(char *state_dir, size_t dircap, pid_t *owner_pid,
                             char *owner_start, size_t startcap) {
    const char *dir = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
    const char *pid_text = getenv("FO_GREMLIN_PROCESS_SCOPE_PID");
    const char *start_text = getenv("FO_GREMLIN_PROCESS_SCOPE_START");
    char current[64], *end;
    long value;
    int e;

    if (!dir && !pid_text && !start_text) return ENOENT;
    if (!has_text(dir) || !has_text(pid_text) || !has_text(start_text)) return EINVAL;
    value = strtol(pid_text, &end, 10);
    if (end == pid_text || *end != '\0' || value <= 0 || value > INT_MAX)
        return EINVAL;
    if (strlen(dir) + 1 > dircap || strlen(start_text) + 1 > startcap)
        return ENAMETOOLONG;
    e = process_identity_text((pid_t)value, current, sizeof(current));
    if (e != 0 || strcmp(current, start_text) != 0) return ESTALE;
    e = private_directory(dir, 0);
    if (e != 0) return e;
    strcpy(state_dir, dir);
    strcpy(owner_start, start_text);
    *owner_pid = (pid_t)value;
    return 0;
}

static int async_owner_registry(const char *state_dir, pid_t owner_pid,
                                const char *owner_start, char *registry,
                                size_t cap, int create) {
    char base[PATH_MAX];
    int n, e;
    n = snprintf(base, sizeof(base), "%s/async-processes", state_dir);
    if (n < 0 || n >= (int)sizeof(base)) return ENAMETOOLONG;
    e = private_directory(base, create);
    if (e != 0) return e;
    n = snprintf(registry, cap, "%s/%ld-%s", base, (long)owner_pid,
                 owner_start);
    if (n < 0 || n >= (int)cap) return ENAMETOOLONG;
    return private_directory(registry, create);
}

static int async_member_record_equivalent(const char *existing,
                                         size_t existing_size,
                                         const char *requested,
                                         size_t requested_size);

static int async_record_matches(const char *path, const char *record,
                                size_t record_size, int member_record) {
    struct stat st;
    char current[512];
    size_t used = 0;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    if (fstat(fd, &st) != 0) { int e = errno; close(fd); return e; }
    if (!S_ISREG(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0077) != 0 || st.st_size < 0 ||
        (size_t)st.st_size >= sizeof(current) ||
        record_size >= sizeof(current) ||
        (!member_record && (size_t)st.st_size != record_size)) {
        close(fd);
        return ESTALE;
    }
    while (used < (size_t)st.st_size) {
        ssize_t n = read(fd, current + used, (size_t)st.st_size - used);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { int e = n < 0 ? errno : EIO; close(fd); return e; }
        used += (size_t)n;
    }
    if (close(fd) != 0) return errno;
    if (used == record_size && memcmp(current, record, record_size) == 0)
        return 0;
    if (member_record && async_member_record_equivalent(
            current, used, record, record_size)) return 0;
    return ESTALE;
}

static int async_publish_record(const char *registry, const char *path,
                                const char *stem, const char *record,
                                size_t record_size, int member_record) {
    char temporary[PATH_MAX];
    struct timespec ts;
    int e, n, fd, dirfd, created = 0;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) return errno;
    n = snprintf(temporary, sizeof(temporary), "%s/.%s.%ld.%lld.%09ld.tmp",
                 registry, stem, (long)getpid(), (long long)ts.tv_sec,
                 ts.tv_nsec);
    if (n < 0 || n >= (int)sizeof(temporary)) return ENAMETOOLONG;
    fd = open(temporary, O_CREAT | O_EXCL | O_WRONLY | O_CLOEXEC | O_NOFOLLOW,
              0600);
    if (fd < 0) return errno;
    e = write_exact(fd, record, record_size) == 0 ? 0 : errno;
    if (e == 0 && fsync(fd) != 0) e = errno;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e == 0) {
        if (link(temporary, path) == 0) created = 1;
        else if (errno == EEXIST) e = async_record_matches(
            path, record, record_size, member_record);
        else e = errno;
    }
    if (unlink(temporary) != 0 && e == 0) e = errno;
    if (e != 0) return e;
    if (created) {
        dirfd = open(registry, O_RDONLY | O_CLOEXEC);
        if (dirfd < 0) return errno;
        if (fsync(dirfd) != 0) e = errno;
        if (close(dirfd) != 0 && e == 0) e = errno;
    }
    return e;
}

static int register_async_session(struct async_process *item) {
    char state_dir[PATH_MAX], owner_start[64], registry[PATH_MAX];
    char child_start[64], path[PATH_MAX], record[256];
    pid_t owner_pid;
    uint64_t child_identity;
    int e, n;

    e = async_scope_owner(state_dir, sizeof(state_dir), &owner_pid,
                          owner_start, sizeof(owner_start));
    if (e == ENOENT) return 0;
    if (e != 0) return e;
    e = process_identity_text(item->pid, child_start, sizeof(child_start));
    if (e != 0 || parse_identity_text(child_start, &child_identity) != 0)
        return e != 0 ? e : EINVAL;
    if (child_identity != item->start_identity) return ESTALE;
    e = async_owner_registry(state_dir, owner_pid, owner_start, registry,
                             sizeof(registry), 1);
    if (e != 0) return e;
    n = snprintf(path, sizeof(path), "%s/%ld-%s.session", registry,
                 (long)item->pid, child_start);
    if (n < 0 || n >= (int)sizeof(path)) return ENAMETOOLONG;
    n = snprintf(record, sizeof(record), "%ld\n%s\n%ld\n%d\n%s\n",
                 (long)item->pid, child_start, (long)item->session,
                 item->owns_session, owner_start);
    if (n < 0 || n >= (int)sizeof(record)) return EOVERFLOW;
    e = async_publish_record(registry, path,
                             strrchr(path, '/') + 1, record, (size_t)n, 0);
    if (e != 0) {
        return e;
    }
    item->registry_path = strdup(path);
    if (item->registry_path == NULL) {
        unlink(path);
        return ENOMEM;
    }
    if (strlen(registry) + 1 > sizeof(item->registry_dir) ||
        strlen(owner_start) + 1 > sizeof(item->scope_owner_start)) {
        unlink(path);
        free(item->registry_path);
        item->registry_path = NULL;
        return ENAMETOOLONG;
    }
    strcpy(item->registry_dir, registry);
    strcpy(item->scope_owner_start, owner_start);
    strcpy(item->start_identity_text, child_start);
    return 0;
}

#if defined(__APPLE__) || defined(__linux__)
static int register_async_descendants(struct async_process *item,
                                               int force) {
    struct owned_process_record *records = NULL;
    struct timespec now;
    size_t count = 0, i;
    int e;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return errno;
    if (!force && item->last_tree_scan.tv_sec != 0 &&
        (now.tv_sec - item->last_tree_scan.tv_sec) * 1000L +
        (now.tv_nsec - item->last_tree_scan.tv_nsec) / 1000000L < 100L)
        return 0;
    e = collect_owned_tree(item->pid, item->start_identity,
                                 item->session, item->owns_session, item->members,
                                 &records, &count);
    if (e != 0) return e;
    for (i = 0; i < count; i++) {
        char member_start[64], parent_start[64], path[PATH_MAX], record[512];
        int n;
        struct owned_process_identity *member;
        size_t member_count = 0;
        if (!records[i].owned || !records[i].alive ||
            records[i].pid == item->pid) continue;
        for (member = item->members; member != NULL; member = member->next) {
            if (member->pid == records[i].pid && member->start == records[i].start)
                break;
            member_count++;
        }
        if (member == NULL) {
            if (member_count == 4096) { e = EOVERFLOW; break; }
            member = calloc(1, sizeof(*member));
            if (member == NULL) { e = ENOMEM; break; }
            member->pid = records[i].pid;
            member->start = records[i].start;
            member->next = item->members;
            item->members = member;
        }
        if (item->registry_dir[0] == '\0') continue;
        e = process_identity_value_text(records[i].start, member_start,
                                        sizeof(member_start));
        if (e != 0) break;
        if (records[i].parent_start == 0) continue;
        e = process_identity_value_text(records[i].parent_start, parent_start,
                                        sizeof(parent_start));
        if (e != 0) break;
        n = snprintf(path, sizeof(path),
                     "%s/%ld-%s--member-%ld-%s.member",
                     item->registry_dir, (long)item->pid,
                     item->start_identity_text, (long)records[i].pid,
                     member_start);
        if (n < 0 || n >= (int)sizeof(path)) { e = ENAMETOOLONG; break; }
        n = snprintf(record, sizeof(record), "%ld\n%s\n%ld\n%s\n%ld\n%ld\n%s\n%s\n",
                     (long)item->pid, item->start_identity_text,
                     (long)records[i].pid, member_start,
                     (long)records[i].session, (long)records[i].parent,
                     parent_start, item->scope_owner_start);
        if (n < 0 || n >= (int)sizeof(record)) { e = EOVERFLOW; break; }
        e = async_publish_record(item->registry_dir, path,
                                 strrchr(path, '/') + 1, record, (size_t)n, 1);
        if (e != 0) break;
    }
    free(records);
    if (e == 0) item->last_tree_scan = now;
    return e;
}
#endif

static void unregister_async_session(struct async_process *item) {
    if (!item || !item->registry_path) return;
#if defined(__APPLE__) || defined(__linux__)
    if (item->registry_dir[0] != '\0') {
        DIR *directory = opendir(item->registry_dir);
        struct dirent *entry;
        char prefix[160];
        int n = snprintf(prefix, sizeof(prefix), "%ld-%s--member-",
                         (long)item->pid, item->start_identity_text);
        if (directory != NULL && n > 0 && n < (int)sizeof(prefix)) {
            while ((entry = readdir(directory)) != NULL) {
                if (strncmp(entry->d_name, prefix, (size_t)n) == 0) {
                    char path[PATH_MAX];
                    int m = snprintf(path, sizeof(path), "%s/%s",
                                     item->registry_dir, entry->d_name);
                    if (m > 0 && m < (int)sizeof(path)) (void)unlink(path);
                }
            }
            closedir(directory);
        } else if (directory != NULL) closedir(directory);
    }
#endif
    (void)unlink(item->registry_path);
    free(item->registry_path);
    item->registry_path = NULL;
}

struct recovery_session {
    pid_t pid;
    pid_t session;
    uint64_t identity;
    int owns_session;
    int is_member;
    pid_t root_pid;
    pid_t parent_pid;
    uint64_t root_identity;
    char root_start[64];
    char start[64];
    char path[PATH_MAX];
};

static int copy_line(char **cursor, char *out, size_t cap) {
    char *end = strchr(*cursor, '\n');
    size_t n;
    if (end == NULL) return EINVAL;
    n = (size_t)(end - *cursor);
    if (n == 0 || n >= cap) return EINVAL;
    memcpy(out, *cursor, n);
    out[n] = '\0';
    *cursor = end + 1;
    return 0;
}

static int parse_positive_pid(const char *text, pid_t *pid) {
    char *end;
    long value = strtol(text, &end, 10);
    if (end == text || *end != '\0' || value <= 0 || value > INT_MAX)
        return EINVAL;
    *pid = (pid_t)value;
    return 0;
}

static int async_member_record_fields_valid(char fields[8][64]) {
    pid_t root_pid, member_pid, session, parent_pid;
    uint64_t root_identity, identity, parent_identity;

    if (parse_positive_pid(fields[0], &root_pid) != 0 ||
        parse_identity_text(fields[1], &root_identity) != 0 ||
        parse_positive_pid(fields[2], &member_pid) != 0 ||
        parse_identity_text(fields[3], &identity) != 0 ||
        parse_positive_pid(fields[4], &session) != 0 ||
        parse_positive_pid(fields[5], &parent_pid) != 0 ||
        parse_identity_text(fields[6], &parent_identity) != 0 ||
        parse_identity_text(fields[7], &identity) != 0) return 0;
    return parent_pid != root_pid || parent_identity == root_identity;
}

static int async_member_record_equivalent(const char *existing,
                                         size_t existing_size,
                                         const char *requested,
                                         size_t requested_size) {
    char existing_buffer[512], requested_buffer[512];
    char existing_fields[8][64], requested_fields[8][64];
    char *cursor;

    if (existing_size == 0 || existing_size >= sizeof(existing_buffer) ||
        requested_size == 0 || requested_size >= sizeof(requested_buffer))
        return 0;
    memcpy(existing_buffer, existing, existing_size);
    existing_buffer[existing_size] = '\0';
    memcpy(requested_buffer, requested, requested_size);
    requested_buffer[requested_size] = '\0';
    cursor = existing_buffer;
    for (size_t i = 0; i < 8; i++) {
        if (copy_line(&cursor, existing_fields[i], sizeof(existing_fields[i])) != 0)
            return 0;
    }
    if (*cursor != '\0') return 0;
    cursor = requested_buffer;
    for (size_t i = 0; i < 8; i++) {
        if (copy_line(&cursor, requested_fields[i], sizeof(requested_fields[i])) != 0)
            return 0;
    }
    if (*cursor != '\0') return 0;
    for (size_t i = 0; i < 4; i++) {
        if (strcmp(existing_fields[i], requested_fields[i]) != 0) return 0;
    }
    if (strcmp(existing_fields[7], requested_fields[7]) != 0) return 0;
    return async_member_record_fields_valid(existing_fields) &&
           async_member_record_fields_valid(requested_fields);
}

static int read_recovery_session(const char *registry, const char *name,
                                 const char *owner_start,
                                 struct recovery_session *item) {
    char path[PATH_MAX], buffer[512], pid_text[32], session_text[32];
    char owns_session_text[8];
    char saved_start[64], saved_owner[64], file_name_start[64];
    struct stat st;
    char *cursor = buffer;
    const char *suffix = ".session";
    size_t name_len = strlen(name), suffix_len = strlen(suffix);
    int fd, e;
    ssize_t n;

    if (name_len <= suffix_len ||
        strcmp(name + name_len - suffix_len, suffix) != 0) return EINVAL;
    if (snprintf(path, sizeof(path), "%s/%s", registry, name) >=
        (int)sizeof(path)) return ENAMETOOLONG;
    fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    if (fstat(fd, &st) != 0) { e = errno; close(fd); return e; }
    if (!S_ISREG(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0077) != 0) { close(fd); return EPERM; }
    do { n = read(fd, buffer, sizeof(buffer) - 1); } while (n < 0 && errno == EINTR);
    e = n < 0 ? errno : 0;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e != 0) return e;
    if (n <= 0 || (size_t)n >= sizeof(buffer) - 1) return EOVERFLOW;
    buffer[n] = '\0';
    e = copy_line(&cursor, pid_text, sizeof(pid_text));
    if (e == 0) e = copy_line(&cursor, saved_start, sizeof(saved_start));
    if (e == 0) e = copy_line(&cursor, session_text, sizeof(session_text));
    if (e == 0) e = copy_line(&cursor, owns_session_text, sizeof(owns_session_text));
    if (e == 0) e = copy_line(&cursor, saved_owner, sizeof(saved_owner));
    if (e != 0 || *cursor != '\0') return e != 0 ? e : EINVAL;
    e = parse_positive_pid(pid_text, &item->pid);
    if (e != 0 || parse_identity_text(saved_start, &item->identity) != 0)
        return EINVAL;
    e = parse_positive_pid(session_text, &item->session);
    if (e != 0 || (strcmp(owns_session_text, "0") != 0 &&
                   strcmp(owns_session_text, "1") != 0) ||
        strcmp(saved_owner, owner_start) != 0) return EINVAL;
    item->owns_session = strcmp(owns_session_text, "1") == 0;
    if (item->owns_session && item->session != item->pid) return EINVAL;

    const char *dash = strchr(name, '-');
    if (dash == NULL || dash == name) return EINVAL;
    char name_pid[32];
    size_t pid_len = (size_t)(dash - name);
    if (pid_len >= sizeof(name_pid)) return EINVAL;
    memcpy(name_pid, name, pid_len);
    name_pid[pid_len] = '\0';
    pid_t filename_pid;
    if (parse_positive_pid(name_pid, &filename_pid) != 0 ||
        filename_pid != item->pid) return EINVAL;
    size_t start_len = name_len - suffix_len - pid_len - 1;
    if (start_len == 0 || start_len >= sizeof(file_name_start)) return EINVAL;
    memcpy(file_name_start, dash + 1, start_len);
    file_name_start[start_len] = '\0';
    if (strcmp(file_name_start, saved_start) != 0 ||
        parse_identity_text(file_name_start, &item->identity) != 0) return EINVAL;
    item->is_member = 0;
    item->root_pid = item->pid;
    item->root_identity = item->identity;
    strcpy(item->root_start, saved_start);
    strcpy(item->start, saved_start);
    strcpy(item->path, path);
    return 0;
}

#if defined(__APPLE__) || defined(__linux__)
static int read_recovery_member(const char *registry, const char *name,
                                const char *owner_start,
                                struct recovery_session *item) {
    char path[PATH_MAX], buffer[512], root_pid_text[32], root_start[64];
    char pid_text[32], start[64], session_text[32], parent_text[32];
    char parent_start[64], saved_owner[64], expected_name[256];
    char root_name[128];
    char *cursor = buffer;
    struct stat st;
    ssize_t n;
    int fd, e, expected_size;
    uint64_t parent_identity;

    if (snprintf(path, sizeof(path), "%s/%s", registry, name) >=
        (int)sizeof(path)) return ENAMETOOLONG;
    fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    if (fstat(fd, &st) != 0) { e = errno; close(fd); return e; }
    if (!S_ISREG(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0077) != 0) { close(fd); return EPERM; }
    do { n = read(fd, buffer, sizeof(buffer) - 1); }
    while (n < 0 && errno == EINTR);
    e = n < 0 ? errno : 0;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e != 0) return e;
    if (n <= 0 || (size_t)n >= sizeof(buffer) - 1) return EOVERFLOW;
    buffer[n] = '\0';
    e = copy_line(&cursor, root_pid_text, sizeof(root_pid_text));
    if (e == 0) e = copy_line(&cursor, root_start, sizeof(root_start));
    if (e == 0) e = copy_line(&cursor, pid_text, sizeof(pid_text));
    if (e == 0) e = copy_line(&cursor, start, sizeof(start));
    if (e == 0) e = copy_line(&cursor, session_text, sizeof(session_text));
    if (e == 0) e = copy_line(&cursor, parent_text, sizeof(parent_text));
    if (e == 0) e = copy_line(&cursor, parent_start, sizeof(parent_start));
    if (e == 0) e = copy_line(&cursor, saved_owner, sizeof(saved_owner));
    if (e != 0 || *cursor != '\0') return e != 0 ? e : EINVAL;
    if (parse_positive_pid(root_pid_text, &item->root_pid) != 0 ||
        parse_identity_text(root_start, &item->root_identity) != 0 ||
        parse_positive_pid(pid_text, &item->pid) != 0 ||
        parse_identity_text(start, &item->identity) != 0 ||
        parse_positive_pid(session_text, &item->session) != 0 ||
        parse_positive_pid(parent_text, &item->parent_pid) != 0 ||
        parse_identity_text(parent_start, &parent_identity) != 0 ||
        parent_identity == 0 || strcmp(saved_owner, owner_start) != 0 ||
        (item->parent_pid == item->root_pid && parent_identity != item->root_identity))
        return EINVAL;
    item->is_member = 1;
    item->owns_session = 0;
    strcpy(item->root_start, root_start);
    strcpy(item->start, start);
    strcpy(item->path, path);
    expected_size = snprintf(expected_name, sizeof(expected_name),
        "%ld-%s--member-%ld-%s.member", (long)item->root_pid,
        item->root_start, (long)item->pid, item->start);
    if (expected_size < 0 || expected_size >= (int)sizeof(expected_name) ||
        strcmp(expected_name, name) != 0) return EINVAL;
    expected_size = snprintf(root_name, sizeof(root_name), "%ld-%s.session",
                             (long)item->root_pid, item->root_start);
    if (expected_size < 0 || expected_size >= (int)sizeof(root_name))
        return ENAMETOOLONG;
    {
        struct recovery_session root = {0};
        e = read_recovery_session(registry, root_name, owner_start, &root);
        if (e != 0 || root.pid != item->root_pid ||
            root.identity != item->root_identity) return e != 0 ? e : ESTALE;
    }
    return 0;
}
#endif

static int recovery_session_count(const struct recovery_session *item) {
#if defined(__APPLE__) || defined(__linux__)
    int count = signal_owned_tree(item->pid, item->identity, item->session,
                                   item->owns_session, NULL, 0);
    if (count == 0 && process_start_identity(item->pid) == item->identity) {
        int status;
        pid_t got;
        do { got = waitpid(item->pid, &status, WNOHANG); }
        while (got < 0 && errno == EINTR);
    }
    return count;
#else
    errno = ENOTSUP;
    return -1;
#endif
}

static int signal_recovery_session(const struct recovery_session *item,
                                   int signal_number) {
#if defined(__APPLE__) || defined(__linux__)
    return signal_owned_tree(item->pid, item->identity, item->session,
                               item->owns_session, NULL, signal_number) < 0 ? errno : 0;
#else
    return ENOTSUP;
#endif
}

int fo_c_recover_async_scope(const char *state_dir, int owner_pid,
                             const char *owner_start) {
    char registry[PATH_MAX], current_owner[64];
    struct recovery_session *items = NULL;
    struct dirent *entry;
    DIR *directory = NULL;
    size_t count = 0, capacity = 0;
    int e, dirfd;
    struct timespec now, deadline;

    uint64_t parsed_owner;
    if (!has_text(state_dir) || owner_pid <= 0 ||
        parse_identity_text(owner_start, &parsed_owner) != 0) return EINVAL;
    e = process_identity_text((pid_t)owner_pid, current_owner,
                              sizeof(current_owner));
    if (e == 0 && strcmp(current_owner, owner_start) == 0 &&
        fo_gremlin_process_matches(owner_pid, owner_start)) return EBUSY;
    e = private_directory(state_dir, 0);
    if (e != 0) return e;
    e = async_owner_registry(state_dir, (pid_t)owner_pid, owner_start,
                             registry, sizeof(registry), 0);
    if (e == ENOENT) return 0;
    if (e != 0) return e;
    directory = opendir(registry);
    if (directory == NULL) return errno;
    while ((entry = readdir(directory)) != NULL) {
        struct stat st;
        char path[PATH_MAX];
        size_t name_len = strlen(entry->d_name);
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
        if (snprintf(path, sizeof(path), "%s/%s", registry,
                     entry->d_name) >= (int)sizeof(path)) { e = ENAMETOOLONG; break; }
        if (entry->d_name[0] == '.' && name_len > 4 &&
            strcmp(entry->d_name + name_len - 4, ".tmp") == 0) {
            if (lstat(path, &st) != 0 || !S_ISREG(st.st_mode) ||
                st.st_uid != geteuid() || unlink(path) != 0) {
                e = errno != 0 ? errno : EPERM;
                break;
            }
            continue;
        }
        if (count == 4096) { e = EOVERFLOW; break; }
        if (count == capacity) {
            size_t next_capacity = capacity == 0 ? 16 : capacity * 2;
            struct recovery_session *next = realloc(items,
                next_capacity * sizeof(*items));
            if (next == NULL) { e = ENOMEM; break; }
            items = next;
            capacity = next_capacity;
        }
        {
            size_t suffix_len = strlen(entry->d_name);
#if defined(__APPLE__) || defined(__linux__)
            if (suffix_len >= 7 &&
                strcmp(entry->d_name + suffix_len - 7, ".member") == 0)
                e = read_recovery_member(registry, entry->d_name, owner_start,
                                         &items[count]);
            else
#endif
                e = read_recovery_session(registry, entry->d_name, owner_start,
                                          &items[count]);
        }
        if (e != 0) break;
        count++;
    }
    if (closedir(directory) != 0 && e == 0) e = errno;
    directory = NULL;
    if (e != 0) { free(items); return e; }

    for (size_t i = 0; i < count; i++) {
        e = signal_recovery_session(&items[i], SIGTERM);
        if (e != 0) { free(items); return e; }
    }
    if (clock_gettime(CLOCK_MONOTONIC, &deadline) != 0) {
        free(items); return errno;
    }
    add_seconds(&deadline, 2);
    for (;;) {
        int remaining = 0;
        for (size_t i = 0; i < count; i++) {
            int live = recovery_session_count(&items[i]);
            if (live < 0) { e = errno; free(items); return e; }
            remaining += live > 0;
        }
        if (remaining == 0) break;
        if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
            e = errno; free(items); return e;
        }
        if (timespec_at_or_after(&now, &deadline)) break;
        sleep_ms(20);
    }
    for (size_t i = 0; i < count; i++) {
        int live = recovery_session_count(&items[i]);
        if (live < 0) { e = errno; free(items); return e; }
        if (live > 0) {
            e = signal_recovery_session(&items[i], SIGKILL);
            if (e != 0) { free(items); return e; }
        }
    }
    if (clock_gettime(CLOCK_MONOTONIC, &deadline) != 0) {
        free(items); return errno;
    }
    add_seconds(&deadline, 1);
    for (;;) {
        int remaining = 0;
        for (size_t i = 0; i < count; i++) {
            int live = recovery_session_count(&items[i]);
            if (live < 0) { e = errno; free(items); return e; }
            remaining += live > 0;
        }
        if (remaining == 0) break;
        if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
            e = errno; free(items); return e;
        }
        if (timespec_at_or_after(&now, &deadline)) {
            free(items);
            return ETIMEDOUT;
        }
        sleep_ms(10);
    }
    for (size_t i = 0; i < count; i++) {
        if (unlink(items[i].path) != 0 && errno != ENOENT) {
            e = errno; free(items); return e;
        }
    }
    free(items);
    dirfd = open(registry, O_RDONLY | O_CLOEXEC);
    if (dirfd >= 0) {
        if (fsync(dirfd) != 0) { e = errno; close(dirfd); return e; }
        close(dirfd);
    }
    if (rmdir(registry) != 0 && errno != ENOENT) return errno;
    return 0;
}

int fo_c_process_set_async_scope(const char *state_dir, int owner_pid,
                                 const char *owner_start) {
    char current[64];
    char pid_text[32];
    int e;
    if (!has_text(state_dir) || owner_pid != (int)getpid() ||
        !has_text(owner_start)) return EINVAL;
    e = process_identity_text((pid_t)owner_pid, current, sizeof(current));
    if (e != 0) return e;
    if (strcmp(current, owner_start) != 0) return ESTALE;
    e = private_directory(state_dir, 0);
    if (e != 0) return e;
    if (snprintf(pid_text, sizeof(pid_text), "%d", owner_pid) >=
        (int)sizeof(pid_text)) return EOVERFLOW;
    if (setenv("FO_GREMLIN_PROCESS_SCOPE_DIR", state_dir, 1) != 0 ||
        setenv("FO_GREMLIN_PROCESS_SCOPE_PID", pid_text, 1) != 0 ||
        setenv("FO_GREMLIN_PROCESS_SCOPE_START", owner_start, 1) != 0) {
        e = errno;
        unsetenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
        unsetenv("FO_GREMLIN_PROCESS_SCOPE_PID");
        unsetenv("FO_GREMLIN_PROCESS_SCOPE_START");
        return e;
    }
    return 0;
}

static int async_owner_exists(const struct async_process *item) {
#if defined(__APPLE__) || defined(__linux__)
    int count = signal_owned_tree(item->pid, item->start_identity, item->session,
                                   item->owns_session, item->members, 0);
    return count < 0 ? -1 : count > 0;
#else
    errno = ENOTSUP;
    return -1;
#endif
}

static int signal_async_owner(const struct async_process *item, int signal_number) {
#if defined(__APPLE__) || defined(__linux__)
    return signal_owned_tree(item->pid, item->start_identity, item->session,
                               item->owns_session, item->members,
                               signal_number) < 0 ? errno : 0;
#else
    return ENOTSUP;
#endif
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
            struct owned_process_identity *member = item->members;
            *link = item->next;
            while (member != NULL) {
                struct owned_process_identity *next = member->next;
                free(member);
                member = next;
            }
            free(item->registry_path);
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
        if ((item->owns_group && group != item->pid) ||
            getsid(item->pid) != item->session) return 0;
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

/* Reap our exact child before asking the live-process API to prove its PID.
   Darwin removes exited children from libproc's session scan immediately, but
   waitpid still holds their exact identity and exit status until we reap them. */
static int verify_or_reap_async_leader(struct async_process *item) {
    int error;
#if defined(__APPLE__) || defined(__linux__)
    siginfo_t info;
    int force = 0;
    if (!item->leader_done) {
        memset(&info, 0, sizeof(info));
        if (waitid(P_PID, (id_t)item->pid, &info,
                    WEXITED | WNOHANG | WNOWAIT) == 0)
            force = info.si_pid == item->pid;
        else if (errno != ECHILD) return errno;
    }
    /* Capture exact descendants before reaping the last leader identity. */
    error = register_async_descendants(item, force);
    if (error != 0) return error;
#endif
    error = observe_async_leader(item);
    if (error != 0) return error;
    if (!item->leader_done && !async_identity_matches(item)) {
        error = observe_async_leader(item);
        if (error != 0) return error;
        if (!item->leader_done) return ESRCH;
    }
    return 0;
}

static void reap_owned_members(const struct async_process *item) {
    const struct owned_process_identity *member;
    for (member = item->members; member != NULL; member = member->next) {
        if (process_start_identity(member->pid) == member->start)
            (void)waitpid(member->pid, NULL, WNOHANG);
    }
}

static int terminate_async_group(struct async_process *item) {
    struct timespec now, deadline;
    int error, exists;

    error = verify_or_reap_async_leader(item);
    if (error != 0) return error;
#if defined(__APPLE__) || defined(__linux__)
    error = register_async_descendants(item, 1);
    if (error != 0) return error;
#endif
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
            reap_owned_members(item);
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
            reap_owned_members(item);
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
    char **monitor_argv = NULL;
    const char *p, *end;
    int index, ready_pipe[2], gate_pipe[2], child_error = 0, reaper_error;
    int containment_status, monitor_mode = 0;
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
    containment_status = fo_c_process_containment_required();
    if (containment_status < 0) {
        free(argv);
        *exitcode = EIO;
        return;
    }
    monitor_mode = containment_status > 0;
    if (monitor_mode) {
#ifdef __linux__
        /* The async handle remains the gated child PID. After publication it
           execs the existing fresh monitor, which owns only this command's
           descendants while borrowing the caller's process group. */
        monitor_argv = calloc((size_t)n_args + 6, sizeof(char *));
        if (monitor_argv == NULL) {
            free(argv);
            *exitcode = ENOMEM;
            return;
        }
        monitor_argv[0] = "/proc/self/exe";
        monitor_argv[1] = FO_MONITOR_ARG;
        monitor_argv[2] = "";
        monitor_argv[3] = "";
        monitor_argv[4] = "0";
        for (index = 0; index < n_args; index++)
            monitor_argv[index + 5] = argv[index];
#else
        free(argv);
        *exitcode = ENOTSUP;
        return;
#endif
    }
    if (has_text(env_extra)) {
        child_env = env_with_overrides(env_extra);
        if (child_env == NULL) {
            free(monitor_argv);
            free(argv);
            *exitcode = errno != 0 ? errno : ENOMEM;
            return;
        }
    }
    reaper_error = ensure_async_subreaper();
    if (reaper_error != 0) {
        free_env_copy(child_env);
        free(monitor_argv);
        free(argv);
        *exitcode = reaper_error;
        return;
    }
    if (pipe(ready_pipe) != 0) {
        free_env_copy(child_env);
        free(monitor_argv);
        free(argv);
        *exitcode = errno;
        return;
    }
    if (pipe(gate_pipe) != 0) {
        *exitcode = errno;
        close(ready_pipe[0]);
        close(ready_pipe[1]);
        free_env_copy(child_env);
        free(monitor_argv);
        free(argv);
        return;
    }
    pid = fork();
    if (pid < 0) {
        *exitcode = errno;
        close(ready_pipe[0]); close(ready_pipe[1]);
        close(gate_pipe[0]); close(gate_pipe[1]);
        free_env_copy(child_env);
        free(monitor_argv);
        free(argv);
        return;
    }
    if (pid == 0) {
        int fd, release, nested = 0;
        close(ready_pipe[0]);
        close(gate_pipe[1]);
        if (!monitor_mode && setsid() < 0) {
            /* The inherited owner filter keeps this child in its session.
               A fresh process group gives the nested job its own handle. */
            if (errno != EPERM || setpgid(0, 0) != 0) child_error = errno;
            else nested = 1;
        }
        if (!monitor_mode && child_error == 0 && nested)
            child_error = ensure_async_subreaper();
        if (!monitor_mode && child_error == 0)
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
        if (monitor_mode) {
#ifdef __linux__
            fd = open("/dev/null", O_WRONLY | O_CLOEXEC);
            if (fd < 0) _exit(126);
            if (fd == FO_MONITOR_FD) {
                int moved = fcntl(fd, F_DUPFD_CLOEXEC, FO_MONITOR_FD + 1);
                if (moved < 0) _exit(126);
                close(fd);
                fd = moved;
            }
            if (dup2(fd, FO_MONITOR_FD) < 0) _exit(126);
            close(fd);
            execv("/proc/self/exe", monitor_argv);
#endif
            _exit(errno == ENOENT ? 127 : 126);
        }
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
        free(monitor_argv);
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
        free(monitor_argv);
        free(argv);
        *exitcode = EIO;
        return;
    }
#endif
    if ((!monitor_mode && getpgid(pid) != pid) || getsid(pid) <= 0) {
        close(gate_pipe[1]);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(monitor_argv);
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
        free(monitor_argv);
        free(argv);
        *exitcode = ENOMEM;
        return;
    }
    item->pid = pid;
    item->session = getsid(pid);
    item->owns_session = item->session == pid;
    item->owns_group = !monitor_mode;
    item->start_identity = identity;
    item->next = async_processes;
    async_processes = item;
    reaper_error = register_async_session(item);
    if (reaper_error != 0) {
        close(gate_pipe[1]);
        forget_async_process(item);
        (void)kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
        }
        free_env_copy(child_env);
        free(monitor_argv);
        free(argv);
        *exitcode = reaper_error;
        return;
    }
    {
        int release = 1;
        if (write_exact(gate_pipe[1], &release, sizeof(release)) != 0) {
            close(gate_pipe[1]);
            unregister_async_session(item);
            forget_async_process(item);
            (void)kill(pid, SIGKILL);
            while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {
            }
            free_env_copy(child_env);
            free(monitor_argv);
            free(argv);
            *exitcode = EIO;
            return;
        }
    }
    close(gate_pipe[1]);
    free_env_copy(child_env);
    free(monitor_argv);
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
    error = verify_or_reap_async_leader(item);
    if (error != 0) {
        *exitcode = error;
        /* Retain exact ownership so the caller can retry or cancel safely. */
        return;
    }
    if (!item->leader_done) return;
    error = terminate_async_group(item);
    if (error != 0) {
        *exitcode = error;
        return;
    }
    unregister_async_session(item);
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
    error = verify_or_reap_async_leader(item);
    if (error != 0) {
        *exitcode = error;
        return;
    }
    error = terminate_async_group(item);
    if (error != 0) {
        *exitcode = error;
        return;
    }
    unregister_async_session(item);
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
static int capture_handoff_child(pid_t child) {
    char state_dir[PATH_MAX], owner_start[64], registry[PATH_MAX];
    char current_owner_start[64];
    pid_t owner_pid = 0, current_owner_pid = 0;
    uint64_t owner_identity = 0, child_identity = 0, observed_identity = 0;
    DIR *directory;
    struct dirent *entry;
    int e, owner_alive = 0, child_alive = 0;

    e = async_scope_owner(state_dir, sizeof(state_dir), &owner_pid,
                          owner_start, sizeof(owner_start));
    if (e != 0 || owner_pid == getpid() ||
        parse_identity_text(owner_start, &owner_identity) != 0 ||
        owned_process_details(owner_pid, NULL, NULL, NULL, &observed_identity,
                              &owner_alive) != 0 || !owner_alive ||
        observed_identity != owner_identity) return 0;
    e = async_owner_registry(state_dir, owner_pid, owner_start, registry,
                             sizeof(registry), 0);
    if (e != 0) return 0;
    directory = opendir(registry);
    if (directory == NULL) return 0;
    while ((entry = readdir(directory)) != NULL) {
        struct recovery_session session = {0};
        size_t length = strlen(entry->d_name);
        if (length <= 8 || strcmp(entry->d_name + length - 8, ".session") != 0)
            continue;
        if (read_recovery_session(registry, entry->d_name, owner_start,
                                  &session) != 0 ||
            session.pid != child ||
            owned_process_details(child, NULL, NULL, NULL, &child_identity,
                                  &child_alive) != 0 || !child_alive ||
            child_identity != session.identity || getsid(child) != session.session)
            continue;
        e = async_scope_owner(state_dir, sizeof(state_dir), &current_owner_pid,
                              current_owner_start, sizeof(current_owner_start));
        if (e == 0 && current_owner_pid == owner_pid &&
            strcmp(current_owner_start, owner_start) == 0 &&
            owned_process_details(owner_pid, NULL, NULL, NULL, &observed_identity,
                                  &owner_alive) == 0 && owner_alive &&
            observed_identity == owner_identity &&
            owned_process_details(child, NULL, NULL, NULL, &child_identity,
                                  &child_alive) == 0 && child_alive &&
            child_identity == session.identity && getsid(child) == session.session &&
            !monitor_stop) {
            closedir(directory);
            return 1;
        }
    }
    closedir(directory);
    return 0;
}

static int monitor_kill_children(int preserve_registered_sessions) {
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
                if (preserve_registered_sessions && !monitor_stop &&
                    capture_handoff_child((pid_t)child) && !monitor_stop) continue;
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
    posix_spawnattr_t attrs;
    struct sigaction action = {0};
    struct sigaction ignore_pipe = {0};
    struct rusage usage = {0};
    sigset_t default_signals;
    pid_t target = 0;
    int error, status = 1, code = 1, target_signal = 0, attrs_ready = 0;
    int target_done = 0, capture_monitor;
    long long cpu_ms = -1;

    if (argc < 6 || prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0) return 126;
    capture_monitor = strcmp(argv[4], "C") == 0;
    action.sa_handler = monitor_term;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGTERM, &action, NULL) != 0) return 126;
    ignore_pipe.sa_handler = SIG_IGN;
    sigemptyset(&ignore_pipe.sa_mask);
    if (sigaction(SIGPIPE, &ignore_pipe, NULL) != 0) return 126;
    error = posix_spawn_file_actions_init(&actions);
    if (error != 0) return 126;
    error = posix_spawnattr_init(&attrs);
    if (error == 0) attrs_ready = 1;
    if (error == 0) {
        sigemptyset(&default_signals);
        sigaddset(&default_signals, SIGPIPE);
        error = posix_spawnattr_setsigdefault(&attrs, &default_signals);
    }
    if (error == 0)
        error = posix_spawnattr_setflags(&attrs, POSIX_SPAWN_SETSIGDEF);
    if (has_text(argv[2]))
        if (error == 0) error = posix_spawn_file_actions_addchdir_np(&actions, argv[2]);
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
        error = posix_spawnp(&target, argv[5], &actions, &attrs,
                             argv + 5, environ);
    posix_spawn_file_actions_destroy(&actions);
    if (attrs_ready) posix_spawnattr_destroy(&attrs);
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
            target_done = 1;
            cpu_ms = (long long)(usage.ru_utime.tv_sec +
                                 usage.ru_stime.tv_sec) * 1000LL +
                     (long long)(usage.ru_utime.tv_usec +
                                 usage.ru_stime.tv_usec) / 1000LL;
            if (WIFEXITED(status)) {
                code = WEXITSTATUS(status);
            } else if (WIFSIGNALED(status)) {
                target_signal = WTERMSIG(status);
                code = 128 + target_signal;
            }
            break;
        }
        if (waited < 0 && errno != EINTR) break;
        sleep_ms(10);
    }
    if (monitor_kill_children(capture_monitor && target_done &&
                              !monitor_stop && WIFEXITED(status) &&
                              WEXITSTATUS(status) == 0) != 0) code = 126;
    (void)write_exact(FO_MONITOR_FD, &cpu_ms, sizeof(cpu_ms));
    close(FO_MONITOR_FD);
    if (target_signal > 0 && code != 126) {
        struct sigaction default_action = {0};
        default_action.sa_handler = SIG_DFL;
        sigemptyset(&default_action.sa_mask);
        (void)sigaction(target_signal, &default_action, NULL);
        (void)kill(getpid(), target_signal);
    }
    return code;
}

int fo_c_process_containment_required(void) {
    pid_t probe_pid;
    pid_t waited;
    int status = 0;
    int seccomp_mode;

    seccomp_mode = prctl(PR_GET_SECCOMP, 0, 0, 0, 0);
    if (seccomp_mode < 0) return -1;
    if (seccomp_mode == 0) return 0;
    if (seccomp_mode != 2) return -1;
    probe_pid = fork();
    if (probe_pid < 0) return -1;
    if (probe_pid == 0) {
        if (setpgid(0, 0) == 0) _exit(0);
        _exit(errno == EPERM ? 1 : 2);
    }
    do {
        waited = waitpid(probe_pid, &status, 0);
    } while (waited < 0 && errno == EINTR);
    if (waited != probe_pid || !WIFEXITED(status)) return -1;
    if (WEXITSTATUS(status) == 0) return 0;
    if (WEXITSTATUS(status) == 1) return 1;
    return -1;
}

int fo_c_start_capture_monitor(const char *monitor_executable, const char *cwd,
                               char *const argv[], char *const overrides[],
                               int stdin_fd, int stdout_fd, int stderr_fd) {
    char **child_env;
    pid_t monitor_pid = 0, target_pid = 0;
    int report_fd = -1;
    int error;

    if (monitor_executable == NULL || monitor_executable[0] != '/' ||
        argv == NULL || argv[0] == NULL) return -EINVAL;
    if (access(monitor_executable, X_OK) != 0) return -errno;
    child_env = env_with_vector(overrides);
    if (child_env == NULL) return -(errno != 0 ? errno : ENOMEM);
    error = start_command_monitor(cwd, argv, "", 0, 1, child_env,
                                  monitor_executable, stdin_fd, stdout_fd,
                                  stderr_fd, &monitor_pid, &target_pid,
                                  &report_fd);
    free_env_vector(child_env);
    if (error != 0) return -error;
    if (report_fd >= 0) close(report_fd);
    return (int)monitor_pid;
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

#ifndef __linux__
int fo_c_process_containment_required(void) { return 0; }

int fo_c_start_capture_monitor(const char *monitor_executable, const char *cwd,
                               char *const argv[], char *const overrides[],
                               int stdin_fd, int stdout_fd, int stderr_fd) {
    (void)monitor_executable;
    (void)cwd;
    (void)argv;
    (void)overrides;
    (void)stdin_fd;
    (void)stdout_fd;
    (void)stderr_fd;
    return -ENOTSUP;
}
#endif
