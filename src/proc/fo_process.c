#define _GNU_SOURCE

#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static int has_text(const char *text) { return text != NULL && text[0] != '\0'; }

struct watchdog_state {
    pthread_mutex_t mu;
    pthread_cond_t cv;
    pid_t pgid;
    int timeout_s;
    int done;
    int fired;
};

static void add_seconds(struct timespec *ts, int seconds) {
    ts->tv_sec += seconds;
}

static void sleep_ms(int ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (long)(ms % 1000) * 1000000L;
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {
    }
}

static void *timeout_watchdog(void *arg) {
    struct watchdog_state *wd = (struct watchdog_state *)arg;
    struct timespec deadline;
    int rc;

    clock_gettime(CLOCK_REALTIME, &deadline);
    add_seconds(&deadline, wd->timeout_s);

    pthread_mutex_lock(&wd->mu);
    while (!wd->done) {
        rc = pthread_cond_timedwait(&wd->cv, &wd->mu, &deadline);
        if (rc == ETIMEDOUT && !wd->done) {
            wd->fired = 1;
            pthread_mutex_unlock(&wd->mu);
            kill(-wd->pgid, SIGTERM);
            for (int k = 0; k < 15; k++) {
                if (kill(-wd->pgid, 0) != 0 && errno == ESRCH) return NULL;
                sleep_ms(200);
            }
            kill(-wd->pgid, SIGKILL);
            return NULL;
        }
    }
    pthread_mutex_unlock(&wd->mu);
    return NULL;
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

/* Build environ + one extra "KEY=VALUE" entry, in the PARENT so the child does
 * no malloc (async-signal-safe). Returns NULL on alloc failure. */
static char **env_with_extra(const char *extra) {
    int n = 0, i;
    char **e;
    while (environ[n]) n++;
    e = (char **)malloc((size_t)(n + 2) * sizeof(char *));
    if (!e) return NULL;
    for (i = 0; i < n; i++) e[i] = environ[i];
    e[n] = (char *)extra;
    e[n + 1] = NULL;
    return e;
}

static int run_argv(const char *cwd, char *const argv[], const char *log_file,
                    int append, int jobs, int timeout_s, const char *env_extra) {
    pid_t pid;
    int status;
    char **child_env = NULL;

    /* Built before fork: setenv in the child is not async-signal-safe and
     * corrupts libgomp when forked from an OpenMP thread. Pointing environ at
     * this env before execvp keeps the spawn safe inside the parallel
     * build/test loops. */
    if (has_text(env_extra)) {
        child_env = env_with_extra(env_extra);
        if (!child_env) return 1;
    }

    pid = fork();
    if (pid < 0) { free(child_env); return 1; }

    if (pid == 0) {
        /* own process group so a timeout can kill the whole subtree */
        setpgid(0, 0);
        if (has_text(cwd) && chdir(cwd) != 0) _exit(127);
        if (jobs > 0) {
            char jobs_text[32];
            snprintf(jobs_text, sizeof(jobs_text), "%d", jobs);
            setenv("OMP_NUM_THREADS", jobs_text, 1);
        }
        if (has_text(log_file)) {
            int flags = O_WRONLY | O_CREAT | (append ? O_APPEND : O_TRUNC);
            int fd = open(log_file, flags, 0666);
            if (fd < 0) _exit(126);
            if (dup2(fd, STDOUT_FILENO) < 0) _exit(126);
            if (dup2(fd, STDERR_FILENO) < 0) _exit(126);
            close(fd);
        }
        if (child_env) environ = child_env;
        execvp(argv[0], argv);
        _exit(errno == ENOENT ? 127 : 126);
    }
    free(child_env);

    /* race-free: also set the group from the parent side */
    setpgid(pid, pid);

    if (timeout_s > 0) {
        pthread_t watchdog;
        struct watchdog_state wd;
        int thread_ok;

        wd.pgid = pid;
        wd.timeout_s = timeout_s;
        wd.done = 0;
        wd.fired = 0;
        pthread_mutex_init(&wd.mu, NULL);
        pthread_cond_init(&wd.cv, NULL);
        thread_ok = pthread_create(&watchdog, NULL, timeout_watchdog, &wd) == 0;
        if (!thread_ok) {
            pthread_cond_destroy(&wd.cv);
            pthread_mutex_destroy(&wd.mu);
            kill(-pid, SIGKILL);
            waitpid(pid, NULL, 0);
            return 1;
        }

        while (waitpid(pid, &status, 0) < 0) {
            if (errno == EINTR) continue;
            pthread_mutex_lock(&wd.mu);
            wd.done = 1;
            pthread_cond_signal(&wd.cv);
            pthread_mutex_unlock(&wd.mu);
            pthread_join(watchdog, NULL);
            pthread_cond_destroy(&wd.cv);
            pthread_mutex_destroy(&wd.mu);
            return 1;
        }

        pthread_mutex_lock(&wd.mu);
        wd.done = 1;
        pthread_cond_signal(&wd.cv);
        pthread_mutex_unlock(&wd.mu);
        pthread_join(watchdog, NULL);
        pthread_mutex_lock(&wd.mu);
        thread_ok = !wd.fired;
        pthread_mutex_unlock(&wd.mu);
        pthread_cond_destroy(&wd.cv);
        pthread_mutex_destroy(&wd.mu);
        if (!thread_ok) return 124;
    } else {
        while (waitpid(pid, &status, 0) < 0) {
            if (errno == EINTR) continue;
            return 1;
        }
    }
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

void fo_c_detect_nproc(int *nproc) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 1) n = 1;
    *nproc = (int)n;
}

void fo_c_getpid(int *pid_out) {
    *pid_out = (int)getpid();
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
                         timeout_s, has_text(env_extra) ? env_extra : NULL);
}

/* Run an arbitrary command given as an argv vector, with no shell. args is a
   buffer of n_args NUL-terminated strings packed back-to-back (args_len bytes
   total); argv[0] is the program. This is the quote-proof, async-signal-safe
   path for compile/link invocations inside the OpenMP build loop: fork+execve
   with no /bin/sh, so quoting never breaks and libgomp is never corrupted. */
void fo_c_run_argv_logged(const char *cwd, const char *args, int args_len,
                          int n_args, const char *log_file, int append,
                          int timeout_s, const char *env_extra, int *exitcode) {
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
                         timeout_s, has_text(env_extra) ? env_extra : NULL);
    free(argv);
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
