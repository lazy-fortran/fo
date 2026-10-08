#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif

/* NUL-separated immediate version directories. Never silently truncate. */
int fo_c_registry_versions(const char *path, char *out, int cap) {
    DIR *directory = opendir(path);
    struct dirent *entry;
    int used = 0, fd;
    if (directory == NULL) return -1;
    fd = dirfd(directory);
    errno = 0;
    while ((entry = readdir(directory)) != NULL) {
        struct stat info;
        size_t length;
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (fstatat(fd, entry->d_name, &info, 0) != 0) {
            closedir(directory);
            return -1;
        }
        if (!S_ISDIR(info.st_mode)) continue;
        length = strlen(entry->d_name) + 1;
        if (length > (size_t)(cap - used)) {
            closedir(directory);
            return -1;
        }
        memcpy(out + used, entry->d_name, length);
        used += (int)length;
        errno = 0;
    }
    if (errno != 0) {
        closedir(directory);
        return -1;
    }
    if (closedir(directory) != 0) return -1;
    return used;
}
