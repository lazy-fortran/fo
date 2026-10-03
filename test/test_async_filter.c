/* Evaluate the installed classic-BPF program against synthetic syscall records.
   Build once each with FO_ASYNC_TEST_AARCH64, FO_ASYNC_TEST_ARM,
   FO_ASYNC_TEST_X86_64 and FO_ASYNC_TEST_UNSUPPORTED. */
#include "../src/proc/fo_process.c"

#if defined(__linux__) && !defined(FO_ASYNC_TEST_UNSUPPORTED)
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

int main(void) {
#if defined(FO_ASYNC_TEST_UNSUPPORTED)
    if (install_async_group_containment(0) != ENOTSUP) return 1;
    puts("unsupported architecture: ENOTSUP");
#elif defined(FO_ASYNC_TEST_AARCH64) || defined(FO_ASYNC_TEST_ARM)
    const unsigned int denied = SECCOMP_RET_ERRNO | EPERM;
#ifdef FO_ASYNC_TEST_AARCH64
    expect(AUDIT_ARCH_AARCH64, 157, denied); /* setsid */
    expect(AUDIT_ARCH_AARCH64, 154, SECCOMP_RET_ALLOW); /* nested group */
    expect(AUDIT_ARCH_AARCH64, 0, SECCOMP_RET_ALLOW);
    expect(AUDIT_ARCH_ARM, 66, denied); /* compat setsid */
    expect(AUDIT_ARCH_ARM, 57, denied); /* compat setpgid */
    expect(AUDIT_ARCH_ARM, 0, SECCOMP_RET_ALLOW);
#else
    expect(AUDIT_ARCH_ARM, 66, denied);
    expect(AUDIT_ARCH_ARM, 57, SECCOMP_RET_ALLOW);
    expect(AUDIT_ARCH_ARM, 0, SECCOMP_RET_ALLOW);
#endif
    expect(AUDIT_ARCH_X86_64, 0, denied);
    puts("ARM filter syscall numbers and unknown architecture: PASS");
#elif defined(FO_ASYNC_TEST_X86_64)
    const unsigned int denied = SECCOMP_RET_ERRNO | EPERM;
    expect(AUDIT_ARCH_X86_64, 112, denied); /* native setsid */
    expect(AUDIT_ARCH_X86_64, 109, SECCOMP_RET_ALLOW); /* native setpgid */
    expect(AUDIT_ARCH_X86_64, 112 | 0x40000000U, denied); /* x32 setsid */
    expect(AUDIT_ARCH_X86_64, 109 | 0x40000000U, denied); /* x32 setpgid */
    expect(AUDIT_ARCH_I386, 66, denied);
    expect(AUDIT_ARCH_I386, 57, denied);
    expect(AUDIT_ARCH_ARM, 0, denied);
    puts("x86-64 native and compat filter: PASS");
#else
#error "Select a synthetic ARM target"
#endif
    return 0;
}
