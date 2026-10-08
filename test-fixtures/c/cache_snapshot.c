/* Filesystem enumeration only. Fortran hashes contents and asserts stability. */
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static int visit(const char *path) {
    struct dirent **entries = NULL;
    int count = scandir(path, &entries, NULL, alphasort), result = 0;
    if (count < 0) return -1;
    for (int index = 0; index < count; ++index) {
        const char *name = entries[index]->d_name;
        if (!strcmp(name, ".") || !strcmp(name, "..")) { free(entries[index]); continue; }
        size_t length = strlen(path) + strlen(name) + 2;
        char *full = malloc(length);
        if (!full) { result = -1; free(entries[index]); continue; }
        snprintf(full, length, "%s/%s", path, name);
        struct stat value;
        if (lstat(full, &value)) { result = -1; free(full); free(entries[index]); continue; }
#if defined(__APPLE__)
        struct timespec modified = value.st_mtimespec, changed = value.st_ctimespec;
#else
        struct timespec modified = value.st_mtim, changed = value.st_ctim;
#endif
        char type = S_ISREG(value.st_mode) ? 'F' : S_ISDIR(value.st_mode) ? 'D' :
            S_ISLNK(value.st_mode) ? 'L' : 'O';
        printf("%c\t%llu %lld %lld.%09ld %lld.%09ld\t%s\n", type,
            (unsigned long long)value.st_mode, (long long)value.st_size,
            (long long)modified.tv_sec, modified.tv_nsec,
            (long long)changed.tv_sec, changed.tv_nsec, full);
        if (type == 'L') {
            char target[8192];
            ssize_t used = readlink(full, target, sizeof(target));
            if (used < 0 || used == sizeof(target)) result = -1;
            else { fputs("T\t", stdout); fwrite(target, 1, (size_t)used, stdout); putchar('\n'); }
        }
        if (type == 'D' && visit(full)) result = -1;
        free(full); free(entries[index]);
    }
    free(entries);
    return result;
}

int main(int argc, char **argv) {
    if (argc != 2) return 2;
    return visit(argv[1]) ? 1 : 0;
}
