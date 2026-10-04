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

static int excluded_entry(const char *parent_rel, const char *name) {
    if (strcmp(name, ".git") == 0 || strcmp(name, ".hg") == 0 ||
        strcmp(name, ".svn") == 0 || strcmp(name, ".bzr") == 0 ||
        strcmp(name, ".gremlin") == 0) {
        return 1;
    }
    /* A build directory is an output only at the root of an input tree. */
    return parent_rel[0] == '\0' && strcmp(name, "build") == 0;
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

static int inventory_path_is_valid(const char *rel) {
    const char *p;
    for (p = rel; *p != '\0'; ++p) {
        if (*p == '\n' || *p == '\r' ||
            (*p == ' ' && (p[1] == '/' || p[1] == '\0'))) {
            errno = EINVAL;
            return 0;
        }
    }
    return 1;
}

static int write_path(FILE *manifest, char kind, unsigned int executable,
                      const char *rel) {
    if (strlen(rel) >= FO_GENERATION_FORTRAN_PATH_LEN - 6) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (!inventory_path_is_valid(rel)) return -1;
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

static int normalize_link_target(const char *link_rel, const char *raw,
                                 char *normalized, size_t capacity) {
    char combined[8192];
    const char *slash = strrchr(link_rel, '/');
    size_t parent_len = slash == NULL ? 0 : (size_t)(slash - link_rel);
    size_t used = 0;
    int n;

    if (raw[0] == '/') {
        errno = EXDEV;
        return -1;
    }
    n = snprintf(combined, sizeof(combined), "%.*s/%s", (int)parent_len,
                 link_rel, raw);
    if (n < 0 || (size_t)n >= sizeof(combined)) {
        errno = ENAMETOOLONG;
        return -1;
    }
    for (const char *p = combined; *p != '\0';) {
        size_t len;
        while (*p == '/') ++p;
        if (*p == '\0') break;
        len = strcspn(p, "/");
        if (len == 1 && p[0] == '.') {
            p += len;
            continue;
        }
        if (len == 2 && p[0] == '.' && p[1] == '.') {
            if (used == 0) {
                errno = EXDEV;
                return -1;
            }
            while (used > 0 && normalized[used - 1] != '/') --used;
            if (used > 0) --used;
            normalized[used] = '\0';
            p += len;
            continue;
        }
        if (used + len + (used == 0 ? 0 : 1) >= capacity) {
            errno = ENAMETOOLONG;
            return -1;
        }
        if (used != 0) normalized[used++] = '/';
        memcpy(normalized + used, p, len);
        used += len;
        normalized[used] = '\0';
        p += len;
    }
    if (used == 0) {
        errno = EINVAL;
        return -1;
    }
    return 0;
}

static int relative_path_is_excluded(const char *rel) {
    char parent[8192] = "";
    const char *p = rel;
    size_t parent_len = 0;
    while (*p != '\0') {
        char name[8192];
        size_t len = strcspn(p, "/");
        if (len >= sizeof(name)) return 1;
        memcpy(name, p, len);
        name[len] = '\0';
        if (excluded_entry(parent, name)) return 1;
        if (parent_len + len + (parent_len == 0 ? 0 : 1) >= sizeof(parent)) {
            return 1;
        }
        if (parent_len != 0) parent[parent_len++] = '/';
        memcpy(parent + parent_len, name, len + 1);
        parent_len += len;
        p += len;
        if (*p == '/') ++p;
    }
    return 0;
}

static int open_regular_at(int root_fd, const char *rel, struct stat *st) {
    char copy[8192];
    char *part, *next;
    int dir_fd = dup(root_fd), file_fd = -1;
    if (dir_fd < 0) return -1;
    if (strlen(rel) >= sizeof(copy)) {
        close(dir_fd);
        errno = ENAMETOOLONG;
        return -1;
    }
    strcpy(copy, rel);
    part = copy;
    while ((next = strchr(part, '/')) != NULL) {
        *next = '\0';
        file_fd = openat(dir_fd, part,
                         O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        close(dir_fd);
        if (file_fd < 0) return -1;
        dir_fd = file_fd;
        part = next + 1;
    }
    file_fd = openat(dir_fd, part, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    close(dir_fd);
    if (file_fd < 0) return -1;
    if (fstat(file_fd, st) != 0 || !S_ISREG(st->st_mode)) {
        close(file_fd);
        errno = EINVAL;
        return -1;
    }
    close(file_fd);
    return 0;
}

static int write_link_path(FILE *manifest, const char *rel,
                           const char *target, size_t target_len) {
    static const char hex[] = "0123456789abcdef";
    size_t path_len = strlen(rel), i;
    if (!inventory_path_is_valid(rel)) return -1;
    if (path_len >= FO_GENERATION_FORTRAN_PATH_LEN - 25 ||
        target_len > (FO_GENERATION_FORTRAN_PATH_LEN - 25 - path_len) / 2 ||
        strchr(rel, '\n') != NULL || strchr(rel, '\r') != NULL) {
        errno = ENAMETOOLONG;
        return -1;
    }
    if (fprintf(manifest, "L 000 %08zu %08zu %s", path_len, target_len,
                rel) < 0) return -1;
    for (i = 0; i < target_len; ++i) {
        unsigned char c = (unsigned char)target[i];
        if (fputc(hex[c >> 4], manifest) == EOF ||
            fputc(hex[c & 15], manifest) == EOF) return -1;
    }
    return fputc('\n', manifest) == EOF ? -1 : 0;
}

struct name_list {
    char **items;
    size_t count;
};

static int compare_name_strings(const void *lhs, const void *rhs) {
    return strcmp(*(char *const *)lhs, *(char *const *)rhs);
}

static void free_names(struct name_list *names) {
    size_t i;
    for (i = 0; i < names->count; ++i) free(names->items[i]);
    free(names->items);
    names->items = NULL;
    names->count = 0;
}

static int list_names(int dir_fd, struct name_list *names) {
    DIR *dir;
    struct dirent *entry;
    size_t capacity = 0;
    int scan_fd = dup(dir_fd);
    names->items = NULL;
    names->count = 0;
    if (scan_fd < 0) return -1;
    dir = fdopendir(scan_fd);
    if (dir == NULL) {
        close(scan_fd);
        return -1;
    }
    for (;;) {
        char **grown;
        errno = 0;
        entry = readdir(dir);
        if (entry == NULL) {
            int scan_error = errno;
            if (closedir(dir) != 0 && scan_error == 0) scan_error = errno;
            if (scan_error != 0) {
                free_names(names);
                errno = scan_error;
                return -1;
            }
            break;
        }
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) continue;
        if (names->count == capacity) {
            capacity = capacity == 0 ? 16 : capacity * 2;
            grown = realloc(names->items, capacity * sizeof(*grown));
            if (grown == NULL) {
                closedir(dir);
                free_names(names);
                return -1;
            }
            names->items = grown;
        }
        names->items[names->count] = strdup(entry->d_name);
        if (names->items[names->count] == NULL) {
            closedir(dir);
            free_names(names);
            return -1;
        }
        ++names->count;
    }
    if (names->count > 1) {
        qsort(names->items, names->count, sizeof(*names->items),
              compare_name_strings);
    }
    return 0;
}

static int copy_regular_file(int input, const char *target,
                             const struct stat *before, mode_t output_mode) {
    int output;
    char buffer[65536];
    ssize_t n;
    struct stat after;
    output = open(target, O_WRONLY | O_CREAT | O_EXCL, output_mode & 0777);
    if (output < 0) return -1;
    while ((n = read(input, buffer, sizeof(buffer))) > 0) {
        ssize_t at = 0;
        while (at < n) {
            ssize_t written = write(output, buffer + at, (size_t)(n - at));
            if (written < 0 && errno == EINTR) continue;
            if (written <= 0) goto fail;
            at += written;
        }
    }
    if (n < 0 || fstat(input, &after) != 0 ||
        before->st_size != after.st_size || before->st_mtime != after.st_mtime ||
        before->st_ctime != after.st_ctime ||
        before->st_ino != after.st_ino || before->st_dev != after.st_dev) {
        errno = EAGAIN;
        goto fail;
    }
    if (fchmod(output, output_mode & 0777) != 0) goto fail;
    if (fsync(output) != 0) {
        int saved = errno;
        close(output);
        errno = saved;
        return -1;
    }
    return close(output);
fail:
    {
        int saved = errno;
        close(output);
        errno = saved;
        return -1;
    }
}

static int walk_directory_at(int root_fd, int dir_fd, const char *rel,
                              const char *dest, FILE *manifest,
                              int copy_files) {
    struct name_list names;
    size_t i;
    if (rel[0] != '\0' && write_path(manifest, 'D', 0, rel) != 0) return -1;
    if (copy_files && rel[0] != '\0') {
        char target[8192];
        if (snprintf(target, sizeof(target), "%s/%s", dest, rel) >=
            (int)sizeof(target)) {
            errno = ENAMETOOLONG;
            return -1;
        }
        if (make_dirs(target) != 0) return -1;
    }
    if (list_names(dir_fd, &names) != 0) return -1;
    for (i = 0; i < names.count; ++i) {
        const char *name = names.items[i];
        char child[8192], target[8192], link_target[8192];
        struct stat st;
        int rc = 0;
        if (excluded_entry(rel, name)) continue;
        if (snprintf(child, sizeof(child), "%s%s%s", rel,
                     rel[0] == '\0' ? "" : "/", name) >=
            (int)sizeof(child)) {
            errno = ENAMETOOLONG;
            rc = -1;
        } else if (fstatat(dir_fd, name, &st, AT_SYMLINK_NOFOLLOW) != 0) {
            rc = -1;
        } else if (S_ISDIR(st.st_mode)) {
            int child_fd = openat(dir_fd, name,
                O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
            if (child_fd < 0) {
                rc = -1;
            } else {
                rc = walk_directory_at(root_fd, child_fd, child, dest,
                                       manifest, copy_files);
                close(child_fd);
            }
        } else if (S_ISLNK(st.st_mode)) {
            ssize_t n = readlinkat(dir_fd, name, link_target,
                                   sizeof(link_target) - 1);
            char resolved[8192];
            if (n < 0 || (size_t)n >= sizeof(link_target) - 1) {
                errno = n < 0 ? errno : ENAMETOOLONG;
                rc = -1;
            } else {
                link_target[n] = '\0';
                if (normalize_link_target(child, link_target, resolved,
                                          sizeof(resolved)) != 0) {
                    rc = -1;
                } else if (relative_path_is_excluded(resolved)) {
                    errno = EPERM;
                    rc = -1;
                } else if (strchr(link_target, '/') != NULL ||
                           strcmp(link_target, ".") == 0 ||
                           strcmp(link_target, "..") == 0) {
                    errno = EINVAL;
                    rc = -1;
                } else if (open_regular_at(root_fd, resolved, &st) != 0) {
                    rc = -1;
                } else if (write_link_path(manifest, child, link_target,
                                           (size_t)n) != 0) {
                    rc = -1;
                } else if (copy_files) {
                    if (snprintf(target, sizeof(target), "%s/%s", dest,
                                 child) >= (int)sizeof(target)) {
                        errno = ENAMETOOLONG;
                        rc = -1;
                    } else if (make_parent(target) != 0 ||
                               symlink(link_target, target) != 0) {
                        rc = -1;
                    }
                }
            }
        } else if (S_ISREG(st.st_mode)) {
            int input = openat(dir_fd, name, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
            if (input < 0 || fstat(input, &st) != 0 || !S_ISREG(st.st_mode)) {
                if (input >= 0) close(input);
                errno = EINVAL;
                rc = -1;
            } else if (write_path(manifest, 'F',
                                  (st.st_mode & 0111) != 0, child) != 0) {
                close(input);
                rc = -1;
            } else if (copy_files) {
                if (snprintf(target, sizeof(target), "%s/%s", dest, child) >=
                    (int)sizeof(target)) {
                    errno = ENAMETOOLONG;
                    rc = -1;
                } else if (make_parent(target) != 0 ||
                           copy_regular_file(input, target, &st,
                                             st.st_mode & 0777) != 0) {
                    rc = -1;
                }
                close(input);
            } else {
                close(input);
            }
        } else {
            errno = EINVAL;
            rc = -1;
        }
        if (rc != 0) {
            free_names(&names);
            return rc;
        }
    }
    free_names(&names);
    return 0;
}

static int walk_tree(const char *root, const char *dest, FILE *manifest,
                     int copy_files) {
    int root_fd = open(root, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    int rc;
    if (root_fd < 0) return -1;
    rc = walk_directory_at(root_fd, root_fd, "", dest, manifest, copy_files);
    close(root_fd);
    return rc;
}

int fo_c_generation_list_tree(const char *root, const char *manifest) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree(root, "", out, 0);
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
    rc = walk_tree(root, dest, out, 1);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

/* Materialize one declared regular input without linking it to its source. */
int fo_c_generation_copy_declared_file(const char *source, const char *dest,
                                       int writable) {
    int input, rc, saved;
    struct stat before;
    mode_t mode;

    if (source == NULL || dest == NULL || source[0] == '\0' || dest[0] == '\0') {
        errno = EINVAL;
        return errno;
    }
    input = open(source, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    if (input < 0) return errno == 0 ? 1 : errno;
    if (fstat(input, &before) != 0 || !S_ISREG(before.st_mode)) {
        close(input);
        errno = EINVAL;
        return errno;
    }
    mode = before.st_mode & 0777;
    if (writable) mode |= S_IWUSR;
    else mode &= ~(S_IWUSR | S_IWGRP | S_IWOTH);
    if (make_parent(dest) != 0) {
        saved = errno;
        close(input);
        errno = saved;
        return errno == 0 ? 1 : errno;
    }
    rc = copy_regular_file(input, dest, &before, mode);
    saved = errno;
    close(input);
    if (rc != 0) {
        unlink(dest);
        errno = saved;
        return errno == 0 ? 1 : errno;
    }
    return 0;
}

/* Capture one inventoried file through its physical root without following
 * directory or leaf symlinks. The inventory's root identity also guards ABA. */
int fo_c_generation_capture_file(const char *root, const char *relative,
                                 const char *destination, long long device,
                                 long long inode) {
    char copy[8192];
    char *part, *next;
    int root_fd = -1, dir_fd = -1, input_fd = -1, rc = -1, saved;
    struct stat root_st, before;
    if (root == NULL || relative == NULL || destination == NULL ||
        root[0] == '\0' || relative[0] == '\0' || relative[0] == '/' ||
        strlen(relative) >= sizeof(copy)) {
        errno = EINVAL;
        return errno;
    }
    strcpy(copy, relative);
    for (part = copy; *part != '\0';) {
        next = strchr(part, '/');
        if (next != NULL) *next = '\0';
        if (*part == '\0' || strcmp(part, ".") == 0 ||
            strcmp(part, "..") == 0) {
            errno = EINVAL;
            return errno;
        }
        if (next == NULL) break;
        part = next + 1;
    }
    root_fd = open(root, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (root_fd < 0) goto done;
    if (fstat(root_fd, &root_st) != 0 || root_st.st_dev != device ||
        root_st.st_ino != inode) {
        errno = EAGAIN;
        goto done;
    }
    dir_fd = dup(root_fd);
    if (dir_fd < 0) goto done;
    part = copy;
    while ((next = strchr(part, '/')) != NULL) {
        *next = '\0';
        input_fd = openat(dir_fd, part,
                          O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (input_fd < 0) goto done;
        close(dir_fd);
        dir_fd = input_fd;
        input_fd = -1;
        part = next + 1;
    }
    input_fd = openat(dir_fd, part, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    if (input_fd < 0) goto done;
    if (fstat(input_fd, &before) != 0 || !S_ISREG(before.st_mode)) {
        errno = EINVAL;
        goto done;
    }
    rc = copy_regular_file(input_fd, destination, &before,
                           before.st_mode & 0777);
done:
    saved = errno;
    if (input_fd >= 0) close(input_fd);
    if (dir_fd >= 0) close(dir_fd);
    if (root_fd >= 0) close(root_fd);
    if (rc != 0) unlink(destination);
    errno = saved;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

/* The inventory provider currently admits only leaf symlinks to regular files. */
int fo_c_generation_create_link(const char *path, const char *target) {
    if (path == NULL || target == NULL || path[0] == '\0' ||
        target[0] == '\0' || strchr(target, '/') != NULL ||
        strcmp(target, ".") == 0 || strcmp(target, "..") == 0 ||
        target[0] == '/') {
        errno = EINVAL;
        return errno;
    }
    if (make_parent(path) != 0 || symlink(target, path) != 0)
        return errno == 0 ? 1 : errno;
    return 0;
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
                excluded_entry(rel, name)) {
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
    if (S_ISLNK(st.st_mode)) return 0;
    if (!S_ISREG(st.st_mode)) {
        errno = EINVAL;
        return -1;
    }
    return chmod(path, (st.st_mode & 0111) ? 0555 : 0444);
}

int fo_c_generation_freeze_tree(const char *root) {
    return freeze_tree_at(root, "") == 0 ? 0 : (errno == 0 ? 1 : errno);
}

int fo_c_generation_freeze_directory(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return errno == 0 ? 1 : errno;
    if (!S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode)) {
        errno = ENOTDIR;
        return errno;
    }
    return chmod(path, 0555) == 0 ? 0 : (errno == 0 ? 1 : errno);
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
