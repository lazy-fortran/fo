/* A filtered owner times out a forked command, then cancels one nested handle.
   Build with cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function. */
#include "../src/proc/fo_process.c"

static void write_line(const char *path, const char *line) {
    int fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0666);
    if (fd < 0 || write(fd, line, strlen(line)) != (ssize_t)strlen(line))
        _exit(90);
    close(fd);
}

static void path_join(char *output, size_t size, const char *root,
                      const char *name) {
    snprintf(output, size, "%s/%s", root, name);
}

static void packed_start(const char *exe, const char *mode, const char *path,
                         int *pid, int *error) {
    char args[3 * PATH_MAX];
    const char *parts[] = {exe, mode, path};
    size_t used = 0;
    for (size_t i = 0; i < 3; i++) {
        size_t length = strlen(parts[i]) + 1;
        memcpy(args + used, parts[i], length);
        used += length;
    }
    fo_c_start_argv_logged("", args, (int)used, 3, "/dev/null", NULL,
                           pid, error);
}

static int read_state(const char *path, int *parent, int *child, int *escape) {
    FILE *file = fopen(path, "r");
    char line[64];
    if (file == NULL) return 0;
    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[0] == 'p') *parent = atoi(line + 2);
        if (line[0] == 'c') *child = atoi(line + 2);
        if (line[0] == 'e') *escape = atoi(line + 2);
    }
    fclose(file);
    return *parent > 0 && *child > 0 && *escape != -1;
}

static int wait_for_file(const char *path, int seconds) {
    struct timespec start, now;
    struct stat info;
    clock_gettime(CLOCK_MONOTONIC, &start);
    for (;;) {
        if (stat(path, &info) == 0 && info.st_size > 0) return 1;
        clock_gettime(CLOCK_MONOTONIC, &now);
        if (now.tv_sec - start.tv_sec >= seconds) return 0;
        sleep_ms(25);
    }
}

static int alive(int pid) {
    return pid > 0 && kill(pid, 0) == 0;
}

int main(int argc, char **argv) {
    char root[] = "/var/tmp/fo-nested-cancel-XXXXXX";
    char target[PATH_MAX], sibling[PATH_MAX], trigger[PATH_MAX];
    char result[PATH_MAX], log_file[PATH_MAX], timeout_file[PATH_MAX];
    int outer = 0, error = 0, target_pid = 0, target_child = 0;
    int sibling_pid = 0, sibling_child = 0;
    int target_escape = -1, sibling_escape = -1, success = 0;
    int target_child_survived = -1, sibling_child_survived = -1;
    int timeout_child = 0, timeout_child_survived = -1;
    struct stat target_before, target_after, sibling_before, sibling_after;
    struct stat timeout_before, timeout_after;

    if (argc == 3 && strcmp(argv[1], "timeout") == 0) {
        pid_t child = fork();
        if (child < 0) return 96;
        if (child == 0) {
            char line[64];
            signal(SIGTERM, SIG_IGN);
            snprintf(line, sizeof(line), "%ld\n", (long)getpid());
            for (;;) {
                write_line(argv[2], line);
                sleep_ms(50);
            }
        }
        sleep(3);
        return 0;
    }

    if (argc == 3 && strcmp(argv[1], "nested") == 0) {
        char line[64];
        pid_t child;
        if (strstr(argv[2], "/target") != NULL) {
            char command[3 * PATH_MAX], heartbeat[PATH_MAX];
            const char *parts[] = {argv[0], "timeout", heartbeat};
            size_t used = 0;
            int timeout_code;
            snprintf(heartbeat, sizeof(heartbeat), "%s-timeout", argv[2]);
            for (size_t i = 0; i < 3; i++) {
                size_t length = strlen(parts[i]) + 1;
                memcpy(command + used, parts[i], length);
                used += length;
            }
            fo_c_run_argv_logged("", command, (int)used, 3,
                                 "/dev/null", 0, 1, 0, NULL, &timeout_code);
            if (timeout_code != 124) return 95;
        }
        child = fork();
        if (child < 0) return 91;
        if (child == 0) {
            int group_error = setpgid(0, 0) == 0 ? 0 : errno;
            snprintf(line, sizeof(line), "e:%d\n", group_error);
            write_line(argv[2], line);
            signal(SIGTERM, SIG_IGN);
            snprintf(line, sizeof(line), "c:%ld\n", (long)getpid());
        } else {
            snprintf(line, sizeof(line), "p:%ld\n", (long)getpid());
        }
        for (;;) {
            write_line(argv[2], line);
            sleep_ms(50);
        }
    }
    if (argc == 3 && strcmp(argv[1], "outer") == 0) {
        int first, second, first_error, second_error;
        char first_file[PATH_MAX], second_file[PATH_MAX];
        char command[PATH_MAX], outcome[PATH_MAX], line[32];
        path_join(first_file, sizeof(first_file), argv[2], "target");
        path_join(second_file, sizeof(second_file), argv[2], "sibling");
        path_join(command, sizeof(command), argv[2], "cancel");
        path_join(outcome, sizeof(outcome), argv[2], "result");
        packed_start(argv[0], "nested", first_file, &first, &first_error);
        packed_start(argv[0], "nested", second_file, &second, &second_error);
        if (first_error != 0 || second_error != 0) return 92;
        if (!wait_for_file(command, 5)) return 93;
        fo_c_cancel_pid(first, &error);
        snprintf(line, sizeof(line), "%d\n", error);
        write_line(outcome, line);
        for (;;) sleep(1);
    }
    if (argc != 1 || mkdtemp(root) == NULL) return 94;
    path_join(target, sizeof(target), root, "target");
    path_join(sibling, sizeof(sibling), root, "sibling");
    path_join(trigger, sizeof(trigger), root, "cancel");
    path_join(result, sizeof(result), root, "result");
    path_join(log_file, sizeof(log_file), root, "outer.log");
    path_join(timeout_file, sizeof(timeout_file), root, "target-timeout");
    packed_start(argv[0], "outer", root, &outer, &error);
    if (error != 0 || outer <= 0) goto cleanup;
    {
        struct timespec start, now;
        clock_gettime(CLOCK_MONOTONIC, &start);
        for (;;) {
            int ready_a = read_state(target, &target_pid, &target_child,
                                     &target_escape);
            int ready_b = read_state(sibling, &sibling_pid, &sibling_child,
                                     &sibling_escape);
            if (ready_a && ready_b) break;
            clock_gettime(CLOCK_MONOTONIC, &now);
            if (now.tv_sec - start.tv_sec >= 5) goto cleanup;
            sleep_ms(25);
        }
    }
    {
        FILE *file = fopen(timeout_file, "r");
        char line[64];
        if (file == NULL || fgets(line, sizeof(line), file) == NULL) {
            if (file != NULL) fclose(file);
            goto cleanup;
        }
        timeout_child = atoi(line);
        fclose(file);
    }
    if (stat(timeout_file, &timeout_before) != 0 ||
        stat(sibling, &sibling_before) != 0) goto cleanup;
    sleep_ms(200);
    if (stat(timeout_file, &timeout_after) != 0 ||
        stat(sibling, &sibling_after) != 0) goto cleanup;
    timeout_child_survived = alive(timeout_child);
    if (timeout_child_survived ||
        timeout_before.st_size != timeout_after.st_size ||
        sibling_after.st_size <= sibling_before.st_size) goto cleanup;
    write_line(trigger, "go\n");
    if (!wait_for_file(result, 5)) goto cleanup;
    {
        FILE *file = fopen(result, "r");
        char line[32];
        if (file == NULL || fgets(line, sizeof(line), file) == NULL) {
            if (file != NULL) fclose(file);
            goto cleanup;
        }
        error = atoi(line);
        fclose(file);
    }
    if (stat(target, &target_before) != 0 ||
        stat(sibling, &sibling_before) != 0) goto cleanup;
    sleep_ms(200);
    if (stat(target, &target_after) != 0 ||
        stat(sibling, &sibling_after) != 0) goto cleanup;
    target_child_survived = alive(target_child);
    sibling_child_survived = alive(sibling_child);
    success = error == 0 && target_escape == EPERM &&
              sibling_escape == EPERM &&
              !alive(target_pid) && !target_child_survived &&
              alive(sibling_pid) && sibling_child_survived && alive(outer) &&
              target_before.st_size == target_after.st_size &&
              sibling_after.st_size > sibling_before.st_size;

cleanup:
    if (outer > 0) fo_c_cancel_pid(outer, &error);
    if (!success) {
        fprintf(stderr, "nested cancellation failed: timeout_child_survived=%d "
                "escape=%d "
                "target_child_survived=%d sibling_child_survived=%d "
                "files=%s\n", timeout_child_survived, target_escape,
                target_child_survived,
                sibling_child_survived, root);
        return 1;
    }
    if (alive(sibling_pid) || alive(sibling_child)) return 2;
    unlink(target);
    unlink(sibling);
    unlink(trigger);
    unlink(result);
    unlink(timeout_file);
    unlink(log_file);
    rmdir(root);
    puts("nested-only cancellation and sibling survival: PASS");
    return 0;
}
