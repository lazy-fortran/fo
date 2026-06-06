#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
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
    snprintf(path, sizeof(path), "%s/CMakeLists.txt", dir);
    if (stat(path, &st) == 0) return 1;
    return 0;
}

static int skip_dir_name(const char *name, int is_proj_root, int depth) {
    if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) return 1;
    if (strcmp(name, ".git") == 0) return 1;
    if (strcmp(name, ".claude") == 0) return 1;
    if (strcmp(name, ".codex") == 0) return 1;
    if (strcmp(name, ".cache") == 0) return 1;
    if (strcmp(name, "node_modules") == 0) return 1;
    /* skip 'build' only at depth 0 of a project root scan */
    if (is_proj_root && depth == 0 && strcmp(name, "build") == 0) return 1;
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

static int run_argv(const char *cwd, char *const argv[], const char *log_file,
                    int append, int jobs, int timeout_s) {
    pid_t pid;
    int status;

    pid = fork();
    if (pid < 0) return 1;

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
        execvp(argv[0], argv);
        _exit(errno == ENOENT ? 127 : 126);
    }

    /* race-free: also set the group from the parent side */
    setpgid(pid, pid);

    if (timeout_s <= 0) {
        if (waitpid(pid, &status, 0) < 0) return 1;
    } else {
        int elapsed = 0;
        struct timespec ts = {1, 0};
        for (;;) {
            pid_t got = waitpid(pid, &status, WNOHANG);
            if (got > 0) break;
            if (got < 0) {
                kill(-pid, SIGKILL);
                waitpid(pid, NULL, 0);
                return 1;
            }
            if (elapsed >= timeout_s) {
                /* hard timeout: TERM the whole group, then KILL on grace expiry */
                int reaped = 0, k;
                struct timespec g = {0, 200000000L};  /* 200 ms */
                kill(-pid, SIGTERM);
                for (k = 0; k < 15 && !reaped; k++) {
                    if (waitpid(pid, &status, WNOHANG) > 0) reaped = 1;
                    else nanosleep(&g, NULL);
                }
                if (!reaped) {
                    kill(-pid, SIGKILL);
                    waitpid(pid, &status, 0);
                }
                kill(-pid, SIGKILL);  /* sweep any straggler grandchildren */
                return 124;  /* timeout — same as GNU timeout exit code */
            }
            nanosleep(&ts, NULL);
            elapsed++;
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
                     int append, int timeout_s, int *exitcode) {
    char *argv[2];

    if (!has_text(exe_path)) {
        *exitcode = 127;
        return;
    }
    argv[0] = (char *)exe_path;
    argv[1] = NULL;
    *exitcode = run_argv(has_text(cwd) ? cwd : NULL, argv, log_file, append, 0,
                         timeout_s);
}

void fo_c_fpm_build(const char *project_dir, const char *flags, int jobs,
                    const char *log_file, int *exitcode) {
    char *argv_with_flags[] = {"fpm", "build", "--flag", (char *)flags, NULL};
    char *argv_plain[] = {"fpm", "build", NULL};
    int timeout = env_timeout("FO_BUILD_TIMEOUT", 300);

    if (has_text(flags)) {
        *exitcode = run_argv(project_dir, argv_with_flags, log_file, 0, jobs,
                             timeout);
    } else {
        *exitcode = run_argv(project_dir, argv_plain, log_file, 0, jobs,
                             timeout);
    }
}

void fo_c_fpm_test_list(const char *project_dir, const char *log_file,
                        int *exitcode) {
    char *argv[] = {"fpm", "test", "--list", NULL};

    *exitcode = run_argv(project_dir, argv, log_file, 0, 0,
                         env_timeout("FO_TEST_TIMEOUT", 30));
}

void fo_c_fpm_test_all(const char *project_dir, int jobs, const char *log_file,
                       int *exitcode) {
    char *argv[] = {"fpm", "test", NULL};

    *exitcode = run_argv(project_dir, argv, log_file, 0, jobs,
                         env_timeout("FO_TEST_TIMEOUT", 120));
}

void fo_c_fpm_test_names(const char *project_dir, const char *names, int jobs,
                         const char *log_file, int *exitcode) {
    char *copy;
    char *items[512];
    char **argv;
    char *p;
    int n = 0;
    int i;

    copy = strdup(has_text(names) ? names : "");
    if (copy == NULL) {
        *exitcode = 1;
        return;
    }

    p = copy;
    while (*p != '\0' && n < 512) {
        char *start = p;
        char *end = strchr(p, '\n');
        if (end != NULL) *end = '\0';
        if (start[0] != '\0') items[n++] = start;
        if (end == NULL) break;
        p = end + 1;
    }

    argv = calloc((size_t)n + 4, sizeof(char *));
    if (argv == NULL) {
        free(copy);
        *exitcode = 1;
        return;
    }
    argv[0] = "fpm";
    argv[1] = "test";
    for (i = 0; i < n; i++) argv[i + 2] = items[i];
    argv[n + 2] = NULL;

    *exitcode = run_argv(project_dir, argv, log_file, 0, jobs,
                         env_timeout("FO_TEST_TIMEOUT", 120));
    free(argv);
    free(copy);
}

void fo_c_cmake_build(const char *project_dir, const char *flags, int jobs,
                      const char *log_file, int *exitcode) {
    char jobs_text[32];
    char flag_arg[2048];
    char *configure_plain[] = {"cmake", "-S", ".", "-B", "build", "-G",
                               "Ninja", NULL};
    char *configure_flags[] = {"cmake", "-S", ".", "-B", "build", "-G",
                               "Ninja", flag_arg, NULL};
    char *build_argv[] = {"cmake", "--build", "build", "-j", jobs_text, NULL};

    snprintf(jobs_text, sizeof(jobs_text), "%d", jobs > 0 ? jobs : 1);
    int build_timeout = env_timeout("FO_BUILD_TIMEOUT", 300);
    if (has_text(flags)) {
        snprintf(flag_arg, sizeof(flag_arg), "-DCMAKE_Fortran_FLAGS=%s", flags);
        *exitcode = run_argv(project_dir, configure_flags, log_file, 0, 0,
                             build_timeout);
    } else {
        *exitcode = run_argv(project_dir, configure_plain, log_file, 0, 0,
                             build_timeout);
    }
    if (*exitcode != 0) return;
    *exitcode = run_argv(project_dir, build_argv, log_file, 1, 0, build_timeout);
}

void fo_c_ctest(const char *project_dir, int jobs, const char *regex,
                int include_slow, const char *log_file, int *exitcode) {
    char build_dir[4096];
    char jobs_text[32];
    char *argv[12];
    int n = 0;

    snprintf(build_dir, sizeof(build_dir), "%s/build", project_dir);
    snprintf(jobs_text, sizeof(jobs_text), "%d", jobs > 0 ? jobs : 1);

    argv[n++] = "ctest";
    argv[n++] = "--output-on-failure";
    argv[n++] = "-j";
    argv[n++] = jobs_text;
    if (has_text(regex)) {
        argv[n++] = "-R";
        argv[n++] = (char *)regex;
    }
    if (!include_slow) {
        argv[n++] = "-LE";
        argv[n++] = "slow|regression|performance|scalability";
    }
    argv[n] = NULL;

    *exitcode = run_argv(build_dir, argv, log_file, 0, 0,
                         env_timeout("FO_TEST_TIMEOUT", 120));
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
