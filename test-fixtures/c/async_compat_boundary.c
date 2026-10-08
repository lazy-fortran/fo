/* Runtime ABI controls must run directly, outside Fo's inherited containment.
 * The same executable then exercises the real owned process provider. */
#define _GNU_SOURCE
#include <stdio.h>
#if defined(__linux__) && defined(__x86_64__)
#include "../../src/proc/fo_process.c"
#include <stdint.h>
struct compat_observation { int32_t returned; pid_t pid, group; };
static int32_t compat_syscall(int x32, int pgid) {
    long value;
    if (x32) {
        long number = 0x40000000L | (pgid ? 109 : 112);
        __asm__ volatile("syscall" : "=a"(value) : "a"(number), "D"(0L), "S"(0L)
                         : "rcx", "r11", "memory");
    } else {
        __asm__ volatile("int $0x80" : "=a"(value) : "a"(pgid ? 57L : 66L),
                         "b"(0L), "c"(0L) : "memory");
    }
    return (int32_t)value;
}
static int control(int x32, int pgid) {
    struct compat_observation observed;
    int descriptors[2], status;
    pid_t child;
    ssize_t count;
    if (pipe(descriptors)) return -1;
    child = fork();
    if (child < 0) { close(descriptors[0]); close(descriptors[1]); return -1; }
    if (!child) {
        close(descriptors[0]);
        observed.returned = compat_syscall(x32, pgid);
        observed.pid = getpid();
        observed.group = pgid ? getpgid(0) : getsid(0);
        if (write(descriptors[1], &observed, sizeof(observed)) != sizeof(observed)) _exit(90);
        _exit(0);
    }
    close(descriptors[1]);
    do { count = read(descriptors[0], &observed, sizeof(observed)); } while (count < 0 && errno == EINTR);
    close(descriptors[0]);
    while (waitpid(child, &status, 0) < 0 && errno == EINTR) {}
    if (count != sizeof(observed) || !WIFEXITED(status) || WEXITSTATUS(status)) return -1;
    if (observed.returned == -ENOSYS) {
        printf("SKIP %s %s unavailable in independent control (ENOSYS)\n",
               x32 ? "x32" : "i386", pgid ? "setpgid" : "setsid");
        return 0;
    }
    if (observed.returned != (pgid ? 0 : observed.pid) || observed.group != observed.pid) {
        fprintf(stderr, "FAIL independent %s %s control returned=%d group=%d pid=%d; "
                "run outside inherited Fo containment\n", x32 ? "x32" : "i386",
                pgid ? "setpgid" : "setsid", observed.returned, observed.group, observed.pid);
        return -1;
    }
    printf("PASS independent %s %s changed the actual OS group/session\n",
           x32 ? "x32" : "i386", pgid ? "setpgid" : "setsid");
    return 1;
}
static void heartbeat(const char *path, const char *label, int value) {
    FILE *file = fopen(path, "a");
    if (!file) _exit(91);
    fprintf(file, "%s %d %d\n", label, (int)getpid(), value);
    if (fclose(file)) _exit(92);
}
static int owned_helper(const char *path, int x32, int pgid) {
    pid_t child = fork();
    int result = 0;
    if (child < 0) return 93;
    if (!child) result = compat_syscall(x32, pgid);
    for (;;) {
        heartbeat(path, child ? "parent" : "descendant", result);
        sleep_ms(50);
    }
}
static int read_heartbeat(const char *path, int *parent, int *descendant, int *result) {
    char label[32];
    int pid, value;
    FILE *file = fopen(path, "r");
    if (!file) return 0;
    while (fscanf(file, "%31s %d %d", label, &pid, &value) == 3) {
        if (!strcmp(label, "parent")) *parent = pid;
        if (!strcmp(label, "descendant")) { *descendant = pid; *result = value; }
    }
    fclose(file); return *parent > 0 && *descendant > 0;
}
static int contained(const char *image, const char *path, int x32, int pgid) {
    const char *parts[] = {image, "--owned", path, x32 ? "x32" : "i386", pgid ? "setpgid" : "setsid"};
    char packed[3 * PATH_MAX];
    size_t used = 0;
    int owner = 0, parent = 0, descendant = 0, result = 0, error, ok = 0;
    uint64_t birth = 0;
    struct stat before = {0}, after = {0};
    for (size_t index = 0; index < 5; ++index) {
        size_t length = strlen(parts[index]) + 1;
        if (used + length > sizeof(packed)) return 0;
        memcpy(packed + used, parts[index], length); used += length;
    }
    fo_c_start_argv_logged("", packed, (int)used, 5, "/dev/null", NULL, &owner, &error);
    if (error || owner <= 0) return 0;
    for (int attempt = 0; attempt < 200; ++attempt) {
        if (read_heartbeat(path, &parent, &descendant, &result)) break;
        sleep_ms(25);
    }
    if (descendant > 0) birth = process_start_identity(descendant);
    ok = parent == owner && descendant > 0 && result == -EPERM;
    fo_c_cancel_pid(owner, &error);
    ok = ok && !error && kill(parent, 0) == -1 && errno == ESRCH &&
         kill(descendant, 0) == -1 && errno == ESRCH;
    if (stat(path, &before)) ok = 0;
    sleep_ms(300);
    if (stat(path, &after) || before.st_size != after.st_size) ok = 0;
    /* Only this fixture's independently recorded child may need failure cleanup. */
    if (descendant > 0 && birth && process_start_identity(descendant) == birth) {
        kill(descendant, SIGKILL);
        while (waitpid(descendant, NULL, 0) < 0 && errno == EINTR) {}
    }
    printf("%s contained %s %s: EPERM, both owned processes reaped, heartbeat stopped\n",
           ok ? "PASS" : "FAIL", x32 ? "x32" : "i386", pgid ? "setpgid" : "setsid");
    return ok;
}
int main(int argc, char **argv) {
    char directory[PATH_MAX], image[PATH_MAX], path[PATH_MAX];
    const char *temporary = getenv("TMPDIR");
    int ok = 1;
    if (argc == 5 && !strcmp(argv[1], "--owned"))
        return owned_helper(argv[2], !strcmp(argv[3], "x32"), !strcmp(argv[4], "setpgid"));
    if (argc != 1 || !realpath(argv[0], image)) return 2;
    if (!temporary || !*temporary) temporary = "/var/tmp";
    if (snprintf(directory, sizeof(directory), "%s/fo-compat-XXXXXX", temporary) >= (int)sizeof(directory) ||
        !mkdtemp(directory)) return 3;
    for (int x32 = 0; x32 < 2; ++x32) for (int pgid = 0; pgid < 2; ++pgid) {
        int supported = control(x32, pgid);
        if (supported < 0) { ok = 0; continue; }
        if (!supported) continue;
        if (snprintf(path, sizeof(path), "%s/%d-%d.heartbeat", directory, x32, pgid) >= (int)sizeof(path)) {
            ok = 0; continue;
        }
        if (!contained(image, path, x32, pgid)) ok = 0;
        if (unlink(path)) ok = 0;
    }
    if (rmdir(directory)) { perror("remove owned compat scratch"); ok = 0; }
    return ok ? 0 : 1;
}
#else
int main(void) {
    puts("SKIP compat session controls require Linux x86-64");
    return 0;
}
#endif
