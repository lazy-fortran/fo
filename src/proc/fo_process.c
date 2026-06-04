#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static int has_text(const char *text) { return text != NULL && text[0] != '\0'; }

static int run_argv(const char *cwd, char *const argv[], const char *log_file,
                    int append, int jobs) {
    pid_t pid;
    int status;

    pid = fork();
    if (pid < 0) return 1;

    if (pid == 0) {
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

    if (waitpid(pid, &status, 0) < 0) return 1;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 1;
}

void fo_c_detect_nproc(int *nproc) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 1) n = 1;
    *nproc = (int)n;
}

void fo_c_fpm_build(const char *project_dir, const char *flags, int jobs,
                    const char *log_file, int *exitcode) {
    char *argv_with_flags[] = {"fpm", "build", "--flag", (char *)flags, NULL};
    char *argv_plain[] = {"fpm", "build", NULL};

    if (has_text(flags)) {
        *exitcode = run_argv(project_dir, argv_with_flags, log_file, 0, jobs);
    } else {
        *exitcode = run_argv(project_dir, argv_plain, log_file, 0, jobs);
    }
}

void fo_c_fpm_test_list(const char *project_dir, const char *log_file,
                        int *exitcode) {
    char *argv[] = {"fpm", "test", "--list", NULL};

    *exitcode = run_argv(project_dir, argv, log_file, 0, 0);
}

void fo_c_fpm_test_all(const char *project_dir, int jobs, const char *log_file,
                       int *exitcode) {
    char *argv[] = {"fpm", "test", NULL};

    *exitcode = run_argv(project_dir, argv, log_file, 0, jobs);
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

    *exitcode = run_argv(project_dir, argv, log_file, 0, jobs);
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
    if (has_text(flags)) {
        snprintf(flag_arg, sizeof(flag_arg), "-DCMAKE_Fortran_FLAGS=%s", flags);
        *exitcode = run_argv(project_dir, configure_flags, log_file, 0, 0);
    } else {
        *exitcode = run_argv(project_dir, configure_plain, log_file, 0, 0);
    }
    if (*exitcode != 0) return;
    *exitcode = run_argv(project_dir, build_argv, log_file, 1, 0);
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
        argv[n++] = "slow";
    }
    argv[n] = NULL;

    *exitcode = run_argv(build_dir, argv, log_file, 0, 0);
}
