#define _GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#if defined(__APPLE__)
#include <sys/clonefile.h>
#elif defined(__linux__)
#include <linux/fs.h>
#include <sys/ioctl.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdatomic.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif
#include "../util/fo_path.h"
#include "../util/fo_sys.h"

/* Exposed only as a focused native oracle for copy durability behavior. */
static atomic_ulong generation_copy_sync_count;

unsigned long fo_c_generation_copy_sync_count(int reset) {
    if (reset) return atomic_exchange(&generation_copy_sync_count, 0);
    return atomic_load(&generation_copy_sync_count);
}

/* Bytes physically written by read/write copies and files shared by block
 * clone; the clone path writes no data. Native oracles for warm-view cost. */
static atomic_ulong generation_copy_bytes;
static atomic_ulong generation_clone_count;

unsigned long fo_c_generation_copy_bytes(int reset) {
    if (reset) return atomic_exchange(&generation_copy_bytes, 0);
    return atomic_load(&generation_copy_bytes);
}

unsigned long fo_c_generation_clone_count(int reset) {
    if (reset) return atomic_exchange(&generation_clone_count, 0);
    return atomic_load(&generation_clone_count);
}

#define FO_GENERATION_FORTRAN_PATH_LEN 4096

static int excluded_entry(const char *parent_rel, const char *name,
                          int exclusion_policy) {
    if (strcmp(name, ".git") == 0 || strcmp(name, ".hg") == 0 ||
        strcmp(name, ".svn") == 0 || strcmp(name, ".bzr") == 0 ||
        strcmp(name, ".gremlin") == 0) {
        return 1;
    }
    if (parent_rel[0] != '\0') return 0;
    /* Build is always excluded at a whole-tree scan root. */
    if (exclusion_policy >= 1 && strcmp(name, "build") == 0) return 1;
    /* Input inventories also skip other root output/cache directories. */
    return exclusion_policy >= 2 &&
           (strcmp(name, ".cache") == 0 || strcmp(name, ".venv") == 0 ||
            strcmp(name, "cache") == 0 ||
            strcmp(name, "caches") == 0 || strcmp(name, "session") == 0 ||
            strcmp(name, "sessions") == 0 || strcmp(name, "log") == 0 ||
            strcmp(name, "logs") == 0);
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

int fo_c_generation_validate_root(const char *root) {
    return validate_tree_root(root) == 0 ? 0 : (errno == 0 ? 1 : errno);
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
    rc = fo_c_mkdir_p(copy);
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

    if (fo_path_is_absolute(raw) || raw[0] == '/') {
        errno = EXDEV;
        return -1;
    }
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (strchr(raw, '\\') || strchr(raw, ':')) {
        errno = EXDEV;
        return -1;
    }
#endif
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

static int relative_path_is_excluded(const char *rel, int exclusion_policy) {
    char parent[8192] = "";
    const char *p = rel;
    size_t parent_len = 0;
    while (*p != '\0') {
        char name[8192];
        size_t len = strcspn(p, "/");
        if (len >= sizeof(name)) return 1;
        memcpy(name, p, len);
        name[len] = '\0';
        if (excluded_entry(parent, name, exclusion_policy)) return 1;
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

static int open_regular_at(int root_fd, const char *rel, struct stat *st,
                           int exclusion_policy) {
    char copy[8192], current[8192], raw[8192], resolved[8192];
    char *part, *next;
    int links;
    if (strlen(rel) >= sizeof(current)) {
        errno = ENAMETOOLONG;
        return -1;
    }
    strcpy(current, rel);
    /* Resolve only file aliases, with a bounded hop count. Every hop retains
     * root confinement, output exclusions and no-follow directory traversal. */
    for (links = 0; links < 40; ++links) {
        int dir_fd = dup(root_fd), file_fd;
        if (dir_fd < 0) return -1;
        strcpy(copy, current);
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
        if (file_fd < 0) {
            int saved = errno;
            ssize_t n;
            if (saved != ELOOP) {
                close(dir_fd);
                errno = saved;
                return -1;
            }
            n = readlinkat(dir_fd, part, raw, sizeof(raw) - 1);
            saved = errno;
            close(dir_fd);
            if (n < 0 || (size_t)n >= sizeof(raw) - 1) {
                errno = n < 0 ? saved : ENAMETOOLONG;
                return -1;
            }
            raw[n] = '\0';
            if (normalize_link_target(current, raw, resolved,
                                      sizeof(resolved)) != 0) return -1;
            if (relative_path_is_excluded(resolved, exclusion_policy)) {
                errno = EPERM;
                return -1;
            }
            strcpy(current, resolved);
            continue;
        }
        close(dir_fd);
        if (fstat(file_fd, st) != 0 || !S_ISREG(st->st_mode)) {
            close(file_fd);
            errno = EINVAL;
            return -1;
        }
        close(file_fd);
        return 0;
    }
    errno = ELOOP;
    return -1;
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

/* Share the source's data blocks (APFS clonefile, Linux FICLONE) instead of
 * writing them. Returns 0 with the target created, or -1 with nothing left
 * behind so the caller falls back to a byte copy. The new file is private:
 * later writes to either side are copy-on-write. */
static int clone_regular_file(int input, const char *target) {
#if defined(__APPLE__)
    return fclonefileat(input, AT_FDCWD, target, CLONE_NOOWNERCOPY);
#elif defined(__linux__) && defined(FICLONE)
    int output = open(target, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (output < 0) return -1;
    if (ioctl(output, FICLONE, input) != 0) {
        int saved = errno;
        close(output);
        unlink(target);
        errno = saved;
        return -1;
    }
    return close(output);
#else
    (void)input;
    (void)target;
    return -1;
#endif
}

static int copy_regular_file(int input, const char *target,
                             const struct stat *before, mode_t output_mode,
                             int sync_output, int share_blocks) {
    int output;
    char buffer[65536];
    ssize_t n;
    struct stat after;
    /* Rebuildable copies (no durability request) of immutable sources share
     * blocks when possible. A live project input is never cloned: APFS reports
     * the clone as an event on the source, which the change watcher would
     * take for an edit and recapture. */
    if (share_blocks && !sync_output && clone_regular_file(input, target) == 0) {
        if (fstat(input, &after) != 0 ||
            before->st_size != after.st_size ||
            before->st_mtime != after.st_mtime ||
            before->st_ctime != after.st_ctime ||
            before->st_ino != after.st_ino || before->st_dev != after.st_dev ||
            chmod(target, output_mode & 0777) != 0) {
            int saved = errno == 0 ? EAGAIN : errno;
            unlink(target);
            errno = saved;
            return -1;
        }
        atomic_fetch_add(&generation_clone_count, 1);
        return 0;
    }
    output = open(target, O_WRONLY | O_CREAT | O_EXCL, output_mode & 0777);
    if (output < 0) return -1;
    while ((n = read(input, buffer, sizeof(buffer))) > 0) {
        ssize_t at = 0;
        atomic_fetch_add(&generation_copy_bytes, (unsigned long)n);
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
    if (sync_output) {
        atomic_fetch_add(&generation_copy_sync_count, 1);
        if (fsync(output) != 0) {
            int saved = errno;
            close(output);
            errno = saved;
            return -1;
        }
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
                              int copy_files, int exclusion_policy,
                              int sync_output, const char *excluded_directory) {
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
        if (fo_c_mkdir_p(target) != 0) return -1;
    }
    if (list_names(dir_fd, &names) != 0) return -1;
    for (i = 0; i < names.count; ++i) {
        const char *name = names.items[i];
        char child[8192], target[8192], link_target[8192];
        struct stat st;
        int rc = 0;
        if (excluded_entry(rel, name, exclusion_policy)) continue;
        if (snprintf(child, sizeof(child), "%s%s%s", rel,
                     rel[0] == '\0' ? "" : "/", name) >=
            (int)sizeof(child)) {
            errno = ENAMETOOLONG;
            rc = -1;
        } else if (excluded_directory != NULL &&
                   strcmp(child, excluded_directory) == 0) {
            continue;
        } else if (fstatat(dir_fd, name, &st, AT_SYMLINK_NOFOLLOW) != 0) {
            rc = -1;
        } else if (S_ISDIR(st.st_mode)) {
            int child_fd = openat(dir_fd, name,
                O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
            if (child_fd < 0) {
                rc = -1;
            } else {
                rc = walk_directory_at(root_fd, child_fd, child, dest,
                                       manifest, copy_files,
                                       exclusion_policy, sync_output, excluded_directory);
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
                                          sizeof(resolved)) != 0 ||
                    relative_path_is_excluded(resolved, exclusion_policy) ||
                    open_regular_at(root_fd, resolved, &st, exclusion_policy) != 0) {
                    /* Delegated CMake checks referenced File API paths
                     * against the resulting inventory. Unused invalid aliases
                     * are not inputs and must not escape or block capture. */
                    if (exclusion_policy == 3) {
                        errno = 0;
                    } else {
                        if (errno == 0) errno = EPERM;
                        rc = -1;
                    }
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
                                             st.st_mode & 0777,
                                             sync_output, 1) != 0) {
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

static int walk_tree_excluding(const char *root, const char *dest, FILE *manifest,
                               int copy_files, int exclusion_policy, int sync_output,
                               const char *excluded_directory) {
    int root_fd = open(root, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    int rc;
    if (root_fd < 0) return -1;
    rc = walk_directory_at(root_fd, root_fd, "", dest, manifest, copy_files,
                           exclusion_policy, sync_output, excluded_directory);
    close(root_fd);
    return rc;
}

static int walk_tree(const char *root, const char *dest, FILE *manifest,
                     int copy_files, int exclusion_policy, int sync_output) {
    return walk_tree_excluding(root, dest, manifest, copy_files, exclusion_policy,
                               sync_output, NULL);
}

int fo_c_generation_list_tree(const char *root, const char *manifest) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree(root, "", out, 0, 1, 1);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

static int list_input_tree(const char *root, const char *manifest,
                           int exclude_root_outputs, const char *excluded_directory) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree_excluding(root, "", out, 0, exclude_root_outputs == 2 ? 3 :
                   (exclude_root_outputs != 0 ? 2 : 0), 1, excluded_directory);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

int fo_c_generation_list_input_tree(const char *root, const char *manifest,
                                    int exclude_root_outputs) {
    return list_input_tree(root, manifest, exclude_root_outputs, NULL);
}

int fo_c_generation_list_cmake_input_tree(const char *root, const char *manifest,
                                          const char *build_directory) {
    return list_input_tree(root, manifest, 2, build_directory);
}

/* A missing include search directory is harmless only beneath a captured
 * project tree. Inspect every existing component without following links, so
 * a dangling link, non-directory or permission error is never called absent. */
int fo_c_generation_project_path_state(const char *root, const char *relative) {
    char *copy = strdup(relative), *part, *save = NULL;
    int fd, child, missing = 0;
    if (copy == NULL) return -1;
    fd = open(root, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) {
        free(copy);
        return -1;
    }
    for (part = strtok_r(copy, "/", &save); part != NULL;
         part = strtok_r(NULL, "/", &save)) {
        child = openat(fd, part, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (child < 0) {
            missing = errno == ENOENT ? 1 : -1;
            break;
        }
        close(fd);
        fd = child;
    }
    close(fd);
    free(copy);
    return missing;
}

/* Reconstructable invocation views need complete bytes, not durable writes. */
int fo_c_generation_copy_tree_ephemeral(const char *root, const char *dest,
                                       const char *manifest) {
    FILE *out;
    int rc;
    if (validate_tree_root(root) != 0) return errno == 0 ? 1 : errno;
    if (fo_c_mkdir_p(dest) != 0) return errno == 0 ? 1 : errno;
    out = fopen(manifest, "w");
    if (out == NULL) return errno == 0 ? 1 : errno;
    rc = walk_tree(root, dest, out, 1, 1, 0);
    if (fclose(out) != 0 && rc == 0) rc = -1;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

/* The invocation copy has private inodes; retain the authored mode rather than
 * the immutable store's read-only mode (which copy tools would propagate). */
int fo_c_generation_restore_private_mode(const char *path, int mode) {
    struct stat st;
    if (lstat(path, &st) != 0) return errno;
    if (!S_ISREG(st.st_mode)) return EINVAL;
    if (chmod(path, (mode_t)(mode & 0777)) != 0) return errno;
    return 0;
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
    rc = copy_regular_file(input, dest, &before, mode, 1, 0);
    saved = errno;
    close(input);
    if (rc != 0) {
        unlink(dest);
        errno = saved;
        return errno == 0 ? 1 : errno;
    }
    return 0;
}

/* Open one inventoried file through its physical root without following
 * directory or leaf symlinks. The inventory's root identity also guards ABA. */
static int open_inventoried_input(const char *root, const char *relative,
                                  long long device, long long inode,
                                  struct stat *before) {
    char copy[8192];
    char *part, *next;
    int root_fd = -1, dir_fd = -1, input_fd = -1, result = -1, saved;
    struct stat root_st;
    if (root == NULL || relative == NULL ||
        root[0] == '\0' || relative[0] == '\0' || relative[0] == '/' ||
        strlen(relative) >= sizeof(copy)) {
        errno = EINVAL;
        return -1;
    }
    strcpy(copy, relative);
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (strchr(relative, '\\') || strchr(relative, ':')) {
        errno = EINVAL;
        return -1;
    }
#endif
    for (part = copy; *part != '\0';) {
        next = strchr(part, '/');
        if (next != NULL) *next = '\0';
        if (*part == '\0' || strcmp(part, ".") == 0 ||
            strcmp(part, "..") == 0) {
            errno = EINVAL;
            return -1;
        }
        if (next == NULL) break;
        *next = '/';
        part = next + 1;
    }
    /* FPM path dependencies may declare a symlink as their root. Following
     * that one root link is safe here: the captured directory identity must
     * still match the inventory, and all descendants remain no-follow. */
    root_fd = open(root, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (root_fd < 0) goto done;
    if (fstat(root_fd, &root_st) != 0 ||
        (unsigned long long)root_st.st_dev != (unsigned long long)device ||
        (unsigned long long)root_st.st_ino != (unsigned long long)inode) {
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
    if (fstat(input_fd, before) != 0 || !S_ISREG(before->st_mode)) {
        errno = EINVAL;
        goto done;
    }
    result = input_fd;
    input_fd = -1;
done:
    saved = errno;
    if (input_fd >= 0) close(input_fd);
    if (dir_fd >= 0) close(dir_fd);
    if (root_fd >= 0) close(root_fd);
    errno = saved;
    return result;
}

/* Descriptor for hashing an inventoried file in place; release it with
 * fo_c_generation_close_fd. Returns the descriptor or a negative errno. */
int fo_c_generation_open_input(const char *root, const char *relative,
                               long long device, long long inode) {
    struct stat before;
    int fd = open_inventoried_input(root, relative, device, inode, &before);
    return fd >= 0 ? fd : -(errno == 0 ? 1 : errno);
}

int fo_c_generation_close_fd(int fd) {
    return close(fd);
}

/* Capture one inventoried file into a private temporary file. */
int fo_c_generation_capture_file(const char *root, const char *relative,
                                 const char *destination, long long device,
                                 long long inode) {
    struct stat before;
    int input_fd, rc, saved;
    if (destination == NULL) {
        errno = EINVAL;
        return errno;
    }
    input_fd = open_inventoried_input(root, relative, device, inode, &before);
    if (input_fd < 0) return errno == 0 ? 1 : errno;
    rc = copy_regular_file(input_fd, destination, &before,
                           before.st_mode & 0777, 0, 0);
    saved = errno;
    close(input_fd);
    if (rc != 0) unlink(destination);
    errno = saved;
    return rc == 0 ? 0 : (errno == 0 ? 1 : errno);
}

/* Enumeration validates every target component with openat/O_NOFOLLOW.
 * Materialization repeats root-relative confinement before preserving text. */
int fo_c_generation_create_link(const char *path, const char *target,
                                 const char *relative) {
    char normalized[8192];
    if (path == NULL || target == NULL || relative == NULL || path[0] == '\0' ||
        target[0] == '\0' ||
        normalize_link_target(relative, target, normalized, sizeof(normalized)) != 0) {
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
        DIR *directory = opendir(path);
        struct dirent *entry;
        int result = 0;
        if (directory == NULL) return -1;
        for (;;) {
            char child[8192];
            errno = 0;
            entry = readdir(directory);
            if (entry == NULL) { result = errno ? -1 : 0; break; }
            const char *name = entry->d_name;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0 ||
                excluded_entry(rel, name, 1)) continue;
            if (snprintf(child, sizeof(child), "%s%s%s", rel,
                         rel[0] == '\0' ? "" : "/", name) >= (int)sizeof(child)) {
                errno = ENAMETOOLONG;
                result = -1;
                break;
            }
            if (freeze_tree_at(root, child) != 0) { result = -1; break; }
        }
        int saved = errno;
        if (closedir(directory) != 0 && result == 0) return -1;
        errno = saved;
        if (result != 0) return result;
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

int fo_c_generation_lock(const char *path) {
    return fo_sys_lock_file(path, 0, 0666, LOCK_EX);
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

int fo_c_generation_remove_stage(const char *path) {
    return fo_c_rm_rf(path) == 0 ? 0 : (errno == 0 ? 1 : errno);
}

int fo_c_generation_remove_empty_stage(const char *path) {
    return rmdir(path) == 0 ? 0 : (errno == 0 ? 1 : errno);
}
