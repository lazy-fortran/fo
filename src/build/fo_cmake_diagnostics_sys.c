/* Fixed-capacity, nonrecursive diagnostic enumeration. Never follow aliases. */
#include <errno.h>
#include <stddef.h>
#include <string.h>
#ifndef _WIN32
#include <dirent.h>
#include <sys/stat.h>
#include <stdio.h>
#endif
int fo_cmake_diagnostic_files(const char *root, char *out, int capacity,
                             int limit, int json_only, int *count)
{
    *count = 0;
#ifdef _WIN32
    (void)root; (void)out; (void)capacity; (void)limit; (void)json_only;
    return -1;
#else
    int used = 0, truncated = 0;
    for (int pass = 0; pass < (json_only ? 2 : 1); ++pass) {
        DIR *directory = opendir(root);
        if (!directory) return errno == ENOENT ? 2 : -1;
        int scanned = 0;
        struct dirent *entry;
        while (1) {
            errno = 0;
            entry = readdir(directory);
            if (!entry) {
                int read_error = errno;
                if (read_error) { closedir(directory); return -1; }
                break;
            }
            if (++scanned > 4096) { truncated = 1; break; }
            const char *name = entry->d_name;
            if (!strcmp(name, ".") || !strcmp(name, "..")) continue;
            size_t length = strlen(name);
            if (json_only) {
                if (length < 5 || strcmp(name + length - 5, ".json")) continue;
                int index = !strncmp(name, "index-", 6);
                if ((pass == 0) != index) continue;
            }
            char path[4096];
            int n = snprintf(path, sizeof path, "%s/%s", root, name);
            if (n < 0 || (size_t)n >= sizeof path) { truncated = 1; continue; }
            struct stat st;
            if (lstat(path, &st)) {
                if (errno == ENOENT) continue;
                closedir(directory); return -1;
            }
            if (!S_ISREG(st.st_mode)) continue;
            if (*count >= limit || n + 1 > capacity - used) {
                truncated = 1; break;
            }
            memcpy(out + used, path, (size_t)n + 1);
            used += n + 1;
            ++*count;
        }
        closedir(directory);
        if (*count >= limit) { truncated = 1; break; }
    }
    return truncated;
#endif
}
