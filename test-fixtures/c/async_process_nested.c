/* Direct process-provider regression: a filtered async owner starts commands
   through the same provider, and cancellation reaches its nested children.
   Build: cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function -o
   /var/tmp/fo-nested-process-regression \
   test-fixtures/c/async_process_nested.c */
#include "../../src/proc/fo_process.c"

static void append_text(const char *path, const char *text) {
    int fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0666);
    if (fd < 0 || write(fd, text, strlen(text)) != (ssize_t)strlen(text))
        _exit(90);
    close(fd);
}

static int read_probe(const char *path) {
    char buffer[32];
    int fd = open(path, O_RDONLY);
    ssize_t n;
    if (fd < 0) return -1;
    n = read(fd, buffer, sizeof(buffer) - 1);
    close(fd);
    if (n <= 0) return -1;
    buffer[n] = '\0';
    if (strcmp(buffer, "0\n") == 0) return 0;
    return atoi(buffer) > 0 ? atoi(buffer) : -1;
}

int main(int argc, char **argv) {
    char directory[] = "/var/tmp/fo-nested-process-XXXXXX";
    char probe[PATH_MAX], heartbeat[PATH_MAX], log_file[PATH_MAX];
    char packed[3 * PATH_MAX];
    int pid, error, code, done, status, nested_pid = 0;
    int child_pid = 0, grandchild_pid = 0;
    pid_t sentinel;
    int sentinel_alive;
    struct timespec before, after;
    FILE *file;

    if (argc == 3 && strcmp(argv[1], "heartbeat") == 0) {
        char line[64];
        pid_t descendant = fork();
        if (descendant < 0) return 100;
        if (descendant == 0) signal(SIGTERM, SIG_IGN);
        snprintf(line, sizeof(line), "%c:%ld\n",
                 descendant == 0 ? 'g' : 'c', (long)getpid());
        for (;;) {
            append_text(argv[2], line);
            sleep_ms(50);
        }
    }
    if (argc == 3 && strcmp(argv[1], "nested-owner") == 0) {
        char line[64], args[3 * PATH_MAX];
        const char *parts[] = {argv[0], "heartbeat", argv[2]};
        size_t used = 0;
        snprintf(line, sizeof(line), "n:%ld\n", (long)getpid());
        append_text(argv[2], line);
        for (size_t i = 0; i < 3; i++) {
            size_t size = strlen(parts[i]) + 1;
            memcpy(args + used, parts[i], size);
            used += size;
        }
        fo_c_run_argv_logged("", args, (int)used, 3, "/dev/null", 0,
                             30, 0, NULL, &code);
        return code;
    }
    if (argc == 4 && strcmp(argv[1], "owner") == 0) {
        char args[3 * PATH_MAX];
        size_t used = 0;
        const char *parts[] = {argv[0], "nested-owner", argv[3]};
        for (size_t i = 0; i < 3; i++) {
            size_t size = strlen(parts[i]) + 1;
            memcpy(args + used, parts[i], size);
            used += size;
        }
        fo_c_start_argv_logged("", args, (int)used, 3, "/dev/null", NULL,
                               &pid, &error);
        {
            char line[32];
            snprintf(line, sizeof(line), "%d\n", error);
            append_text(argv[2], line);
        }
        if (error != 0 || pid <= 0) return 103;
        for (;;) sleep(1);
    }
    if (argc != 1 || mkdtemp(directory) == NULL) return 92;
    snprintf(probe, sizeof(probe), "%s/probe", directory);
    snprintf(heartbeat, sizeof(heartbeat), "%s/heartbeat", directory);
    snprintf(log_file, sizeof(log_file), "%s/owner.log", directory);
    {
        size_t offset = 0;
        const char *parts[] = {argv[0], "owner", probe, heartbeat};
        for (size_t i = 0; i < 4; i++) {
            size_t size = strlen(parts[i]) + 1;
            memcpy(packed + offset, parts[i], size);
            offset += size;
        }
        fo_c_start_argv_logged("", packed, (int)offset, 4, log_file, NULL,
                               &pid, &error);
    }
    if (error != 0 || pid <= 0) return 93;
    clock_gettime(CLOCK_MONOTONIC, &before);
    for (;;) {
        char line[64];
        file = fopen(heartbeat, "r");
        if (file != NULL) {
            while (fgets(line, sizeof(line), file) != NULL) {
                if (line[0] == 'c') child_pid = atoi(line + 2);
                if (line[0] == 'g') grandchild_pid = atoi(line + 2);
                if (line[0] == 'n') nested_pid = atoi(line + 2);
            }
            fclose(file);
        }
        if (nested_pid > 0 && child_pid > 0 && grandchild_pid > 0) break;
        clock_gettime(CLOCK_MONOTONIC, &after);
        if (after.tv_sec - before.tv_sec >= 5) {
            fo_c_cancel_pid(pid, &error);
            fprintf(stderr, "nested child did not execute; probe=%d\n",
                    read_probe(probe));
            return 94;
        }
        sleep_ms(25);
    }
    if (read_probe(probe) != 0) {
        fo_c_cancel_pid(pid, &error);
        return 95;
    }
    fo_c_poll_pid(pid, &done, &status);
    if (done) {
        fo_c_cancel_pid(pid, &error);
        return 96;
    }
    sentinel = fork();
    if (sentinel < 0) return 105;
    if (sentinel == 0) for (;;) pause();
    fo_c_cancel_pid(pid, &error);
    sentinel_alive = kill(sentinel, 0) == 0;
    kill(sentinel, SIGKILL);
    while (waitpid(sentinel, NULL, 0) < 0 && errno == EINTR) {
    }
    if (!sentinel_alive) return 106;
    if (error != 0 || kill(nested_pid, 0) == 0 || errno != ESRCH) return 104;
    if (error != 0 || kill(child_pid, 0) == 0 || errno != ESRCH) return 97;
    if (kill(grandchild_pid, 0) == 0 || errno != ESRCH) return 101;
    {
        struct stat first, second;
        if (stat(heartbeat, &first) != 0) return 98;
        sleep_ms(200);
        if (stat(heartbeat, &second) != 0 || first.st_size != second.st_size)
            return 99;
    }
    unlink(probe);
    unlink(heartbeat);
    unlink(log_file);
    rmdir(directory);
    puts("nested provider launch and owner-tree cancellation: PASS");
    return 0;
}
