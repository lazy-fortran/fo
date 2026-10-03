#define _GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define FO_GENERATION_FORTRAN_PATH_LEN 4096

static int excluded_name(const char *name) {
    return strcmp(name, ".git") == 0 || strcmp(name, "build") == 0 ||
           strcmp(name, ".hg") == 0 || strcmp(name, ".svn") == 0 ||
           strcmp(name, ".bzr") == 0 || strcmp(name, ".gremlin") == 0;
}

static int validate_tree_root(const char *root) {
    char *copy = strdup(root);
    size_t length;
    struct stat st;
    if (copy == NULL) return -1;
    length = strlen(copy);
    while (length > 1 && copy[length - 1] == '/') copy[--length] = '\0';
    if (lstat(copy, &st) != 0) {
        free(copy);
        return -1;
    }
    if (S_ISLNK(st.st_mode) || !S_ISDIR(st.st_mode)) {
        errno = ENOTDIR;
        free(copy);
        return -1;
    }
    free(copy);
    return 0;
}

static int make_dirs(const char *path) {
    char *copy = strdup(path);
    char *p;
    struct stat st;
    if (copy == NULL) return -1;
    for (p = copy + 1; *p != '\0'; ++p) {
        if (*p != '/') continue;
        *p = '\0';
        if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
            free(copy);
            return -1;
        }
        *p = '/';
    }
    if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
        free(copy);
        return -1;
    }
    if (stat(copy, &st) != 0 || !S_ISDIR(st.st_mode)) {
        free(copy);
        errno = ENOTDIR;
        return -1;
    }
    free(copy);
    return 0;
}

static int write_path(FILE *manifest, char kind, unsigned int executable,
                      const char *rel) {
    if (strlen(rel) >= FO_GENERATION_FORTRAN_PATH_LEN - 6) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (strchr(rel, '\n') != NULL || strchr(rel, '\r') != NULL) {
        errno = EINVAL;
        return -1;
    }
    return fprintf(manifest, "%c %03u %s\n", kind, executable, rel) < 0 ?
               -1 : 0;
}

static int compare_names(const struct dirent **lhs, const struct dirent **rhs) {
    return strcmp((*lhs)->d_name, (*rhs)->d_name);
}

static int make_parent(const char *path) {
    char *copy = strdup(path);
    char *slash;
    int rc;
    if (copy == NULL) return -1;
    slash = strrchr(copy, '/');
    if (slash == NULL) {
        free(copy);
        return 0;
    }
    *slash = '\0';
    rc = make_dirs(copy);
    free(copy);
    return rc;
}

static int walk_tree(const char *root, const char *rel, const char *dest,
                     FILE *manifest, int copy_files) {
    char path[8192], target[8192];
    struct stat st;
    size_t root_len = strlen(root), rel_len = strlen(rel);
    if (root_len + (rel_len == 0 ? 0 : rel_len + 1) >=
        FO_GENERATION_FORTRAN_PATH_LEN) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (rel_len == 0) {
        if (snprintf(path, sizeof(path), "%s", root) >= (int)sizeof(path)) {
            errno = ENAMETOOLONG;
            return -1;
        }
    } else if (snprintf(path, sizeof(path), "%s/%s", root, rel) >=
               (int)sizeof(path)) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (lstat(path, &st) != 0) return -1;
    if (S_ISLNK(st.st_mode)) {
        errno = ELOOP;
        return -1;
    }
    if (S_ISDIR(st.st_mode)) {
        struct dirent **entries = NULL;
        int count, i;
        if (copy_files) {
            if (snprintf(target, sizeof(target), "%s/%s", dest, rel) >=
                (int)sizeof(target)) {
                errno = ENAMETOOLONG;
                return -1;
            }
            if (make_dirs(target) != 0) return -1;
        }
        if (rel[0] != '\0' && write_path(manifest, 'D', 0, rel) != 0) {
            return -1;
        }
        count = scandir(path, &entries, NULL, compare_names);
        if (count < 0) return -1;
        for (i = 0; i < count; ++i) {
            char child[8192];
            int rc;
            const char *name = entries[i]->d_name;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0 ||
                excluded_name(name)) {
                free(entries[i]);
                continue;
            }
            if (snprintf(child, sizeof(child), "%s%s%s", rel,
                         rel[0] == '\0' ? "" : "/", name) >=
                (int)sizeof(child)) {
                free(entries[i]);
                while (++i < count) free(entries[i]);
                free(entries);
                errno = ENAMETOOLONG;
                return -1;
            }
            rc = walk_tree(root, child, dest, manifest, copy_files);
            free(entries[i]);
            if (rc != 0) {
                while (++i < count) free(entries[i]);
                free(entries);
                return rc;
            }
        }
        free(entries);
        return 0;
    }
    if (!S_ISREG(st.st_mode)) {
        errno = EINVAL;
        return -1;
    }
    if (write_path(manifest, 'F', (st.st_mode & 0111) != 0, rel) != 0) {
        return -1;
    }
    if (!copy_files) return 0;
    {
        int input, output;
        char buffer[65536];
        ssize_t n;
        struct stat after;
        if (snprintf(target, sizeof(target), "%s/%s", dest, rel) >=
            (int)sizeof(target)) {
            errno = ENAMETOOLONG;
            return -1;
        }
        if (make_parent(target) != 0) return -1;
        input = open(path, O_RDONLY | O_NOFOLLOW);
        if (input < 0) return -1;
        if (fstat(input, &st) != 0 || !S_ISREG(st.st_mode)) {
            close(input);
            errno = EINVAL;
            return -1;
        }
        output = open(target, O_WRONLY | O_CREAT | O_EXCL, st.st_mode & 0777);
        if (output < 0) {
            close(input);
            return -1;
        }
        while ((n = read(input, buffer, sizeof(buffer))) > 0) {
            ssize_t at = 0;
            while (at < n) {
                ssize_t written = write(output, buffer + at, (size_t)(n - at));
                if (written < 0 && errno == EINTR) continue;
                if (written <= 0) {
                    close(input);
                    close(output);
                    return -1;
                }
                at += written;
            }
        }
        if (n < 0 || fstat(input, &after) != 0 ||
            st.st_size != after.st_size || st.st_mtime != after.st_mtime ||
            st.st_ino != after.st_ino || st.st_dev != after.st_dev) {
            close(input);
            close(output);
            errno = EAGAIN;
            return -1;
        }
        if (fchmod(output, st.st_mode & 0777) != 0) {
            close(input);
            close(output);
            return -1;
        }
        close(input);
        if (fsync(output) != 0 || close(output) != 0) return -1;
    }
    return 0;
}

int fo_c_generation_list_tree(const char *root, const char *manifest) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree(root, "", "", out, 0);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

int fo_c_generation_copy_tree(const char *root, const char *dest,
                              const char *manifest) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    if (make_dirs(dest) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree(root, "", dest, out, 1);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

static int freeze_tree_at(const char *root, const char *rel) {
    char path[8192];
    struct stat st;
    if (rel[0] == '\0') {
        if (snprintf(path, sizeof(path), "%s", root) >= (int)sizeof(path)) {
            errno = ENAMETOOLONG;
            return -1;
        }
    } else if (snprintf(path, sizeof(path), "%s/%s", root, rel) >=
               (int)sizeof(path)) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (lstat(path, &st) != 0) return -1;
    if (S_ISDIR(st.st_mode)) {
        struct dirent **entries = NULL;
        int count = scandir(path, &entries, NULL, compare_names);
        int i;
        if (count < 0) return -1;
        for (i = 0; i < count; ++i) {
            char child[8192];
            const char *name = entries[i]->d_name;
            int rc = 0;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0 ||
                excluded_name(name)) {
                free(entries[i]);
                continue;
            }
            if (snprintf(child, sizeof(child), "%s%s%s", rel,
                         rel[0] == '\0' ? "" : "/", name) >=
                (int)sizeof(child)) {
                errno = ENAMETOOLONG;
                rc = -1;
            } else {
                rc = freeze_tree_at(root, child);
            }
            free(entries[i]);
            if (rc != 0) {
                while (++i < count) free(entries[i]);
                free(entries);
                return rc;
            }
        }
        free(entries);
        return chmod(path, 0555);
    }
    if (!S_ISREG(st.st_mode)) {
        errno = EINVAL;
        return -1;
    }
    return chmod(path, (st.st_mode & 0111) ? 0555 : 0444);
}

int fo_c_generation_freeze_tree(const char *root) {
    return freeze_tree_at(root, "") == 0 ? 0 : (errno == 0 ? 1 : errno);
}

int fo_c_generation_lock(const char *path) {
    int fd = open(path, O_CREAT | O_RDWR, 0666);
    if (fd < 0) return -errno;
    if (flock(fd, LOCK_EX) != 0) {
        int error = errno;
        close(fd);
        return -error;
    }
    return fd;
}

void fo_c_generation_unlock(int fd) {
    if (fd >= 0) close(fd);
}

int fo_c_generation_publish(const char *stage, const char *target) {
    struct stat st;
    if (lstat(target, &st) == 0) return 1;
    if (errno != ENOENT) return -errno;
    if (rename(stage, target) != 0) return -errno;
    if (chmod(target, 0555) != 0) return -errno;
    return 0;
}

static int remove_frozen(const char *path) {
    struct stat st;
    DIR *dir;
    struct dirent *entry;
    char child[8192];
    if (lstat(path, &st) != 0) return errno == ENOENT ? 0 : -1;
    if (!S_ISDIR(st.st_mode)) return unlink(path);
    if (chmod(path, 0700) != 0) return -1;
    dir = opendir(path);
    if (dir == NULL) return -1;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) {
            closedir(dir);
            errno = ENAMETOOLONG;
            return -1;
        }
        if (remove_frozen(child) != 0) {
            closedir(dir);
            return -1;
        }
    }
    closedir(dir);
    return rmdir(path);
}

int fo_c_generation_remove_stage(const char *path) {
    return remove_frozen(path) == 0 ? 0 : (errno == 0 ? 1 : errno);
}
