#if defined(FO_TEST_OLD_FEATURE_MACROS)
#define _XOPEN_SOURCE 700
#define _POSIX_C_SOURCE 200809L
#elif defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#else
#define _XOPEN_SOURCE 700
#define _POSIX_C_SOURCE 200809L
#endif

#include <fcntl.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __APPLE__
#include <libproc.h>
#endif

int fo_gremlin_feature_macro_probe(void) {
    int fd = open("/", O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    int lock_result = flock(fd, LOCK_EX | LOCK_NB);
#ifdef __APPLE__
    struct proc_bsdinfo info;
    u_int unsigned_int = 0;
    u_short unsigned_short = 0;
    u_long unsigned_long = 0;
    rusage_info_t usage = (rusage_info_t)0;
    int (*read_usage)(int, int, rusage_info_t *) = proc_pid_rusage;
    int process_info = proc_pidinfo(getpid(), PROC_PIDTBSDINFO, 0, &info,
                                    sizeof(info));
    return process_info + lock_result + (int)sizeof(info) + unsigned_int +
           unsigned_short + (int)unsigned_long + (read_usage == 0) +
           (usage != 0);
#else
    return lock_result;
#endif
}
