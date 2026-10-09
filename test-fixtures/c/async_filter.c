/* Evaluate the filter against syscall records and, on native x86-64 Linux,
   probe real compatibility syscalls inside an asynchronously owned child. */
#if !defined(FO_ASYNC_TEST_AARCH64) && !defined(FO_ASYNC_TEST_ARM) && \
    !defined(FO_ASYNC_TEST_X86_64) && !defined(FO_ASYNC_TEST_UNSUPPORTED)
#define FO_ASYNC_TEST_RUNTIME 1
#endif
#include "../../src/proc/fo_process.c"

#if !defined(FO_ASYNC_TEST_AARCH64) && !defined(FO_ASYNC_TEST_ARM) && \
    !defined(FO_ASYNC_TEST_X86_64) && !defined(FO_ASYNC_TEST_UNSUPPORTED)
#if defined(__linux__) && defined(__aarch64__)
#define FO_ASYNC_TEST_AARCH64
#elif defined(__linux__) && defined(__arm__)
#define FO_ASYNC_TEST_ARM
#elif defined(__linux__) && defined(__x86_64__)
#define FO_ASYNC_TEST_X86_64
#endif
#endif

#if defined(__linux__) && !defined(FO_ASYNC_TEST_UNSUPPORTED) && \
    (defined(FO_ASYNC_TEST_AARCH64) || defined(FO_ASYNC_TEST_ARM) || \
     defined(FO_ASYNC_TEST_X86_64) || \
     (!defined(FO_ASYNC_TEST_AARCH64) && !defined(FO_ASYNC_TEST_ARM) && \
      !defined(FO_ASYNC_TEST_X86_64) && \
      (defined(__aarch64__) || defined(__arm__) || defined(__x86_64__))))
static unsigned int decision(unsigned int arch, int nr) {
    struct seccomp_data input = { .nr = nr, .arch = arch };
    unsigned int accumulator = 0;
    size_t pc = 0;

    for (;;) {
        const struct sock_filter *instruction = &async_group_filter[pc];
        switch (instruction->code) {
        case BPF_LD | BPF_W | BPF_ABS:
            if (instruction->k == offsetof(struct seccomp_data, arch))
                accumulator = input.arch;
            else if (instruction->k == offsetof(struct seccomp_data, nr))
                accumulator = (unsigned int)input.nr;
            else abort();
            pc++;
            break;
        case BPF_ALU | BPF_AND | BPF_K:
            accumulator &= instruction->k;
            pc++;
            break;
        case BPF_JMP | BPF_JEQ | BPF_K:
            pc += 1 + (accumulator == instruction->k
                       ? instruction->jt : instruction->jf);
            break;
        case BPF_JMP | BPF_JSET | BPF_K:
            pc += 1 + ((accumulator & instruction->k) != 0
                       ? instruction->jt : instruction->jf);
            break;
        case BPF_RET | BPF_K:
            return instruction->k;
        default:
            abort();
        }
        if (pc >= sizeof(async_group_filter) / sizeof(async_group_filter[0]))
            abort();
    }
}

static void expect(unsigned int arch, int nr, unsigned int result) {
    unsigned int actual = decision(arch, nr);
    if (actual != result) {
        fprintf(stderr, "arch=%#x syscall=%d: got %#x, expected %#x\n",
                arch, nr, actual, result);
        exit(1);
    }
}
#endif

#if defined(FO_ASYNC_TEST_RUNTIME) && defined(__linux__) && defined(__x86_64__)
/* Issue compatibility syscalls directly: libc always uses the native ABI. */
static long compat_session_call(int x32, int group) {
    if (x32) {
        long result;
        long number = (group ? 109L : 112L) | 0x40000000L;
        __asm__ volatile("syscall" : "=a"(result)
                         : "0"(number), "D"(0L), "S"(0L)
                         : "rcx", "r11", "memory", "cc");
        return result;
    } else {
        int result;
        int number = group ? 57 : 66;
        __asm__ volatile("int $0x80" : "=a"(result)
                         : "0"(number), "b"(0), "c"(0)
                         : "memory", "cc");
        return result;
    }
}

static int compat_probe(int x32, int group, int filtered) {
    long result = compat_session_call(x32, group);
    if (filtered) {
        if (result == -EPERM) return 0;
        fprintf(stderr, "%s %s under async containment returned %ld, expected %d\n",
                x32 ? "x32" : "i386", group ? "setpgid" : "setsid",
                result, -EPERM);
        return 1;
    }
    if (result != (group ? 0 : getpid())) return 77;
    return (group ? getpgid(0) : getsid(0)) == getpid() ? 0 : 77;
}

static int runtime_compat(const char *executable) {
    for (int x32 = 0; x32 < 2; x32++) {
        for (int group = 0; group < 2; group++) {
            int status, pid, error, done = 0, code = -1;
            pid_t probe = fork();
            if (probe < 0) return 1;
            if (probe == 0) _exit(compat_probe(x32, group, 0));
            while (waitpid(probe, &status, 0) < 0) {
                if (errno != EINTR) return 1;
            }
            if (WIFEXITED(status) && WEXITSTATUS(status) == 77) {
                printf("SKIP: unfiltered %s %s is unavailable\n",
                       x32 ? "x32" : "i386", group ? "setpgid" : "setsid");
                continue;
            }
            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) return 1;
            {
                char args[PATH_MAX + 32];
                const char *parts[] = {executable, "--compat",
                    x32 ? "x32" : "i386", group ? "setpgid" : "setsid"};
                size_t used = 0;
                for (size_t i = 0; i < 4; i++) {
                    size_t length = strlen(parts[i]) + 1;
                    if (used + length > sizeof(args)) return 1;
                    memcpy(args + used, parts[i], length);
                    used += length;
                }
                fo_c_start_argv_logged("", args, (int)used, 4, "/dev/null", NULL,
                                       &pid, &error);
            }
            if (error != 0 || pid <= 0) {
                fprintf(stderr, "contained compatibility probe launch failed: %d\n",
                        error);
                return 1;
            }
            for (int attempt = 0; attempt < 200; attempt++) {
                fo_c_poll_pid(pid, &done, &code);
                if (done) break;
                sleep_ms(10);
            }
            if (!done) fo_c_cancel_pid(pid, &error);
            if (!done || code != 0) {
                fprintf(stderr, "FAIL: contained %s %s probe done=%d status=%d\n",
                        x32 ? "x32" : "i386", group ? "setpgid" : "setsid",
                        done, code);
                return 1;
            }
            printf("PASS: kernel denies contained %s %s with EPERM\n",
                   x32 ? "x32" : "i386", group ? "setpgid" : "setsid");
        }
    }
    return 0;
}
#endif

int main(int argc, char **argv) {
#if defined(FO_ASYNC_TEST_RUNTIME) && defined(__linux__) && defined(__x86_64__)
    if (argc == 4 && strcmp(argv[1], "--compat") == 0)
        return compat_probe(strcmp(argv[2], "x32") == 0,
                            strcmp(argv[3], "setpgid") == 0, 1);
#endif
    (void)argc;
    (void)argv;
#if defined(FO_ASYNC_TEST_UNSUPPORTED)
    if (install_async_group_containment(0) != ENOTSUP) return 1;
    puts("unsupported architecture: ENOTSUP");
#elif defined(__linux__) && \
    (defined(FO_ASYNC_TEST_AARCH64) || defined(FO_ASYNC_TEST_ARM))
    const unsigned int denied = SECCOMP_RET_ERRNO | EPERM;
#if defined(FO_ASYNC_TEST_AARCH64) || \
    (!defined(FO_ASYNC_TEST_ARM) && !defined(FO_ASYNC_TEST_X86_64) && \
     defined(__aarch64__))
    expect(AUDIT_ARCH_AARCH64, 157, SECCOMP_RET_ALLOW); /* owned native session */
    expect(AUDIT_ARCH_AARCH64, 154, SECCOMP_RET_ALLOW); /* nested group */
    expect(AUDIT_ARCH_AARCH64, 0, SECCOMP_RET_ALLOW);
    expect(AUDIT_ARCH_ARM, 66, denied); /* compat setsid */
    expect(AUDIT_ARCH_ARM, 57, denied); /* compat setpgid */
    expect(AUDIT_ARCH_ARM, 0, SECCOMP_RET_ALLOW);
#else
    expect(AUDIT_ARCH_ARM, 66, SECCOMP_RET_ALLOW); /* owned native session */
    expect(AUDIT_ARCH_ARM, 57, SECCOMP_RET_ALLOW);
    expect(AUDIT_ARCH_ARM, 0, SECCOMP_RET_ALLOW);
#endif
    expect(AUDIT_ARCH_X86_64, 0, denied);
#if defined(FO_ASYNC_TEST_AARCH64) || \
    (!defined(FO_ASYNC_TEST_ARM) && !defined(FO_ASYNC_TEST_X86_64) && \
     defined(__aarch64__))
    puts("AArch64 filter syscall numbers and unknown architecture: PASS");
#else
    puts("ARM filter syscall numbers and unknown architecture: PASS");
#endif
#elif defined(__linux__) && defined(FO_ASYNC_TEST_X86_64)
    const unsigned int denied = SECCOMP_RET_ERRNO | EPERM;
    expect(AUDIT_ARCH_X86_64, 112, SECCOMP_RET_ALLOW); /* owned native session */
    expect(AUDIT_ARCH_X86_64, 109, SECCOMP_RET_ALLOW); /* native setpgid */
    expect(AUDIT_ARCH_X86_64, 112 | 0x40000000U, denied); /* x32 setsid */
    expect(AUDIT_ARCH_X86_64, 109 | 0x40000000U, denied); /* x32 setpgid */
    expect(AUDIT_ARCH_I386, 66, denied);
    expect(AUDIT_ARCH_I386, 57, denied);
    expect(AUDIT_ARCH_ARM, 0, denied);
    puts("x86-64 native and compat filter: PASS");
#else
    puts("unsupported host: async filter oracle skipped");
#endif
#if defined(FO_ASYNC_TEST_RUNTIME) && defined(__linux__) && defined(__x86_64__)
    return runtime_compat(argv[0]);
#endif
    return 0;
}
