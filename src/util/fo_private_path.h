/* Ownership checks use native owner SID/DACLs instead of simulated Unix IDs. */
#ifndef FO_PRIVATE_PATH_H
#define FO_PRIVATE_PATH_H
static inline int fo_private_path(const char *path) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return 0;
    int private_owned = fx_win_current_owned(fd);
    close(fd);
    return private_owned == 1;
#else
    struct stat st;
    return lstat(path, &st) == 0 && st.st_uid == geteuid();
#endif
}
static inline int fo_private_fd(int fd) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    return fx_win_private_owned(fd) == 1;
#else
    struct stat st;
    return fstat(fd, &st) == 0 && st.st_uid == geteuid() && (st.st_mode & 0077) == 0;
#endif
}
#endif
