/* Raw POSIX identities for the Linux adopted-child oracle. Windows has no
   forked children or zombies; its fixture exercises the native Job lifecycle. */
#include <errno.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
int fo_test_adopted_fork(void) { errno = ENOTSUP; return -1; }
int fo_test_adopted_wait(int pid, int *status, int options) {
    (void)pid; (void)status; (void)options;
    errno = ENOTSUP; return -1;
}
#else
#include <unistd.h>
#include <sys/wait.h>
int fo_test_adopted_fork(void) { return (int)fork(); }
int fo_test_adopted_wait(int pid, int *status, int options) {
    int result;
    do { result = (int)waitpid(pid, status, options); }
    while (result < 0 && errno == EINTR);
    return result;
}
#endif
