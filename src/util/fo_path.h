#ifndef FO_PATH_H
#define FO_PATH_H
/* Internal records use normalized forward slashes; native absolute paths also
   have a drive or UNC share rather than a fabricated POSIX mount spelling. */
static inline int fo_path_is_absolute(const char *path) {
    if (!path) return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    return (((path[0] >= 'A' && path[0] <= 'Z') ||
             (path[0] >= 'a' && path[0] <= 'z')) && path[1] == ':' &&
            path[2] == '/') || (path[0] == '/' && path[1] == '/');
#else
    return path[0] == '/';
#endif
}
#endif
