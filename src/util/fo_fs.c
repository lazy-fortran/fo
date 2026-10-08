/* Filesystem primitives with no shell: replacements for the rm/mkdir/find
   shell-outs fo used to make. Every operation here is a direct libc syscall,
   so nothing forks /bin/sh and nothing is corrupted when called from an
   OpenMP parallel region. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int fo_has(const char *s) { return s != NULL && s[0] != '\0'; }

int fo_c_is_windows(void) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    return 1;
#else
    return 0;
#endif
}

int fo_c_temp_directory(char *out, int capacity) {
    if (out == NULL || capacity <= 0) { errno = EINVAL; return -1; }
#if defined(_WIN32) && !defined(__CYGWIN__)
    typedef DWORD (WINAPI *temp_path_fn)(DWORD, wchar_t *);
    temp_path_fn path_fn = (temp_path_fn)(void *)GetProcAddress(
        GetModuleHandleW(L"kernel32.dll"), "GetTempPath2W");
    wchar_t wide[32768];
    DWORD length = path_fn ? path_fn(32768, wide) : GetTempPathW(32768, wide);
    if (!length || length >= 32768) {
        if (length) errno = ENAMETOOLONG;
        else fx_win32_errno(GetLastError());
        return -1;
    }
    char *path = fx_win32_utf8(wide);
    if (!path) return fx_win32_errno(GetLastError());
    size_t size = strlen(path);
    if (size >= (size_t)capacity) { free(path); errno = ENAMETOOLONG; return -1; }
    for (char *p = path; *p; ++p) if (*p == '\\') *p = '/';
    memcpy(out, path, size + 1);
    free(path);
#else
    if (capacity < 9) { errno = ENAMETOOLONG; return -1; }
    memcpy(out, "/var/tmp", 9);
#endif
    return 0;
}

int fo_c_realpath(const char *path, char *resolved, int capacity) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (path == NULL || resolved == NULL || capacity <= 0) return EINVAL;
    if (fx_win_resolve_root(path, resolved, (size_t)capacity) != 0) return errno;
    return 0;
#else
    char *canonical;
    size_t length;
    if (path == NULL || resolved == NULL || capacity <= 0) return EINVAL;
    canonical = realpath(path, NULL);
    if (canonical == NULL) return errno == 0 ? EIO : errno;
    length = strlen(canonical);
    if (length >= (size_t)capacity) {
        free(canonical);
        return ENAMETOOLONG;
    }
    memcpy(resolved, canonical, length + 1);
    free(canonical);
    return 0;
#endif
}

/* Recursively delete a file or directory tree. Missing path is success
   (mirrors rm -rf). Returns 0 on success, -1 on error. */
int fo_c_rm_rf(const char *path) {
    struct stat st;
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];

    if (!fo_has(path)) return 0;
    if (lstat(path, &st) != 0) {
        return (errno == ENOENT) ? 0 : -1;
    }
    if (!S_ISDIR(st.st_mode)) {
        if (unlink(path) != 0 && errno != ENOENT) return -1;
        return 0;
    }

    dir = opendir(path);
    if (dir == NULL) return -1;
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (
#else
    if (st.st_uid == geteuid() &&
#endif
        ((st.st_mode & S_IWUSR) == 0 || (st.st_mode & S_IXUSR) == 0) &&
        fchmod(dirfd(dir), st.st_mode | S_IWUSR | S_IXUSR) != 0) {
        int chmod_error = errno;
        closedir(dir);
        errno = chmod_error;
        return -1;
    }
    for (;;) {
        int read_error;
        errno = 0;
        ent = readdir(dir);
        if (ent == NULL) {
            read_error = errno;
            if (closedir(dir) != 0 && read_error == 0) read_error = errno;
            if (read_error != 0) {
                errno = read_error;
                return -1;
            }
            break;
        }
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, ent->d_name) >=
            (int)sizeof(child)) {
            closedir(dir);
            errno = ENAMETOOLONG;
            return -1;
        }
        if (fo_c_rm_rf(child) != 0) {
            int child_error = errno;
            closedir(dir);
            errno = child_error;
            return -1;
        }
    }
    if (rmdir(path) != 0 && errno != ENOENT) return -1;
    return 0;
}

/* Delete a single file. Missing file is success (mirrors rm -f). */
int fo_c_rm_file(const char *path) {
    if (!fo_has(path)) return 0;
    if (unlink(path) != 0 && errno != ENOENT) return -1;
    return 0;
}

/* mkdir -p: create path and all missing parents. */
int fo_c_mkdir_p(const char *path) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (!fo_has(path)) { errno = EINVAL; return -1; }
    return fx_win_mkdirs(path, 0);
#else
    char clean[PATH_MAX];
    char parent[PATH_MAX];
    char *slash;
    struct stat st;
    size_t len, plen;

    if (!fo_has(path)) return -1;
    len = strlen(path);
    while (len > 1 && path[len - 1] == '/') len--;
    if (len >= sizeof(clean)) return -1;
    memcpy(clean, path, len);
    clean[len] = '\0';
    if (strcmp(clean, "/") == 0) return 0;
    if (stat(clean, &st) == 0) return S_ISDIR(st.st_mode) ? 0 : -1;

    slash = strrchr(clean, '/');
    if (slash != NULL && slash != clean) {
        plen = (size_t)(slash - clean);
        if (plen >= sizeof(parent)) return -1;
        memcpy(parent, clean, plen);
        parent[plen] = '\0';
        if (fo_c_mkdir_p(parent) != 0) return -1;
    }
    if (mkdir(clean, 0777) != 0 && errno != EEXIST) return -1;
    return 0;
#endif
}

/* Delete every regular file under root whose name ends with suffix. When
   recursive is nonzero, descends subdirectories (replacing find -name -delete).
   Returns the number of files removed, or -1 on a hard error. */
int fo_c_delete_suffix(const char *root, const char *suffix, int recursive) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];
    struct stat st;
    size_t slen, nlen;
    int removed = 0, sub;

    if (!fo_has(root) || !fo_has(suffix)) return 0;
    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : -1;
    slen = strlen(suffix);
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", root, ent->d_name) >=
            (int)sizeof(child)) {
            continue;
        }
        if (lstat(child, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            if (recursive) {
                sub = fo_c_delete_suffix(child, suffix, recursive);
                if (sub >= 0) removed += sub;
            }
            continue;
        }
        nlen = strlen(ent->d_name);
        if (nlen >= slen && strcmp(ent->d_name + (nlen - slen), suffix) == 0) {
            if (unlink(child) == 0) removed++;
        }
    }
    closedir(dir);
    return removed;
}

static int fo_str_contains(const char *hay, const char *needle) {
    if (needle == NULL || needle[0] == '\0') return 1;
    return strstr(hay, needle) != NULL;
}

/* Profile filters are matched against the directory portion of a path.  A
   compiler name is also allowed in a source basename (for example
   semantic_analyzer_nvfortran_wrappers.f90), so matching the whole path would
   make a GNU profile look like an NVHPC profile. */
static int fo_directory_contains(const char *path, const char *needle) {
    const char *last_slash;
    const char *match;
    size_t needle_len;

    if (needle == NULL || needle[0] == '\0') return 1;
    last_slash = strrchr(path, '/');
    if (last_slash == NULL) return 0;
    needle_len = strlen(needle);
    match = path;
    while ((match = strstr(match, needle)) != NULL) {
        if (match + needle_len <= last_slash + 1) return 1;
        match++;
    }
    return 0;
}

/* Recursively collect regular files under root whose basename contains infix
   and ends with suffix, and whose directory path contains path_needle (when set).
   Matches are written to out as NUL-separated paths; returns the count, or -1
   if the buffer overflows or a hard error occurs. Replaces a find pipeline. */
static int fo_collect_rec(const char *root, const char *infix,
                          const char *suffix, const char *path_needle,
                          int recursive, char *out, int cap, int *used,
                          int reject_aliases) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];
    struct stat st;
    size_t nlen, slen, plen;
    int count = 0, sub;

    if (reject_aliases) {
        if (lstat(root, &st) != 0) return (errno == ENOENT) ? 0 : -1;
        if (S_ISLNK(st.st_mode)) return -2;
    }
    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : -1;
    slen = (suffix != NULL) ? strlen(suffix) : 0;
    for (;;) {
        errno = 0;
        ent = readdir(dir);
        if (ent == NULL) {
            int read_error = errno;
            closedir(dir);
            return read_error ? -1 : count;
        }
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", root, ent->d_name) >=
            (int)sizeof(child)) {
            closedir(dir);
            return -1;
        }
        if (lstat(child, &st) != 0) {
            if (errno == ENOENT) continue;
            closedir(dir);
            return -1;
        }
        if (reject_aliases && S_ISLNK(st.st_mode)) {
            closedir(dir);
            return -2;
        }
        if (S_ISDIR(st.st_mode)) {
            if (!recursive) continue;
            sub = fo_collect_rec(child, infix, suffix, path_needle, recursive,
                                 out, cap, used, reject_aliases);
            if (sub < 0) { closedir(dir); return sub; }
            count += sub;
            continue;
        }
        if (!S_ISREG(st.st_mode)) continue;
        nlen = strlen(ent->d_name);
        if (slen > 0 && (nlen < slen ||
            strcmp(ent->d_name + (nlen - slen), suffix) != 0)) continue;
        if (!fo_str_contains(ent->d_name, infix)) continue;
        if (!fo_directory_contains(child, path_needle)) continue;
        plen = strlen(child);
        if (*used + (int)plen + 1 > cap) { closedir(dir); return -1; }
        memcpy(out + *used, child, plen);
        out[*used + (int)plen] = '\0';
        *used += (int)plen + 1;
        count++;
    }
}

int fo_c_collect_files(const char *root, const char *infix, const char *suffix,
                       const char *path_needle, int recursive, char *out,
                       int cap, int reject_aliases) {
    int used = 0;
    if (!fo_has(root)) return 0;
    return fo_collect_rec(root, infix, suffix, path_needle, recursive, out, cap,
                          &used, reject_aliases);
}

/* List direct dependency checkouts without traversing their Git object stores.
   A .git file covers linked worktrees as well as ordinary .git directories. */
int fo_c_collect_git_checkouts(const char *root, char *out, int cap,
                               int item_cap) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX], marker[PATH_MAX];
    struct stat st;
    int used = 0, count = 0, length;

    if (!fo_has(root) || out == NULL || cap <= 0 || item_cap <= 0) return -1;
    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : -1;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        length = snprintf(child, sizeof(child), "%s/%s", root, ent->d_name);
        if (length < 0 || length >= (int)sizeof(child)) goto fail;
        if (lstat(child, &st) != 0) goto fail;
        if (!S_ISDIR(st.st_mode)) continue;
        if (snprintf(marker, sizeof(marker), "%s/.git", child) >=
            (int)sizeof(marker)) goto fail;
        if (lstat(marker, &st) != 0) {
            if (errno == ENOENT) continue;
            goto fail;
        }
        if (!S_ISDIR(st.st_mode) && !S_ISREG(st.st_mode)) continue;
        if (length >= item_cap || used + length + 1 > cap) goto fail;
        memcpy(out + used, child, (size_t)length);
        out[used + length] = '\0';
        used += length + 1;
        count++;
    }
    closedir(dir);
    return count;

fail:
    closedir(dir);
    return -1;
}

/* Atomic exclusive directory create, used as a cross-process lock: returns 0
   when this caller created the directory, 1 when it already existed, -1 on a
   hard error. */
int fo_c_mkdir_excl(const char *path) {
    if (!fo_has(path)) return -1;
    if (mkdir(path, 0777) == 0) return 0;
    if (errno == EEXIST) return 1;
    return -1;
}

#include <signal.h>
/* Return 1 if a process with this pid exists, 0 otherwise (kill -0). */
int fo_c_pid_alive(int pid) {
    if (pid <= 0) return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!process) return GetLastError() == ERROR_ACCESS_DENIED;
    DWORD status = WaitForSingleObject(process, 0);
    CloseHandle(process);
    return status == WAIT_TIMEOUT;
#else
    if (kill((pid_t)pid, 0) == 0) return 1;
    return (errno == EPERM) ? 1 : 0;
#endif
}

/* File modification fingerprint for cache "outputs already match" checks:
   nanosecond mtime and byte size. Returns 0 on success, -1 if the path
   cannot be stat'd. Lets the build skip rewriting a large unchanged output
   (e.g. a 14MB statically linked binary) without re-hashing its contents. */
int fo_c_stat_fingerprint(const char *path, long long *mtime_ns,
                          long long *size) {
    struct stat st;
    if (!fo_has(path) || stat(path, &st) != 0) return -1;
#if defined(__APPLE__)
    *mtime_ns = (long long)st.st_mtimespec.tv_sec * 1000000000LL +
                (long long)st.st_mtimespec.tv_nsec;
#else
    *mtime_ns = (long long)st.st_mtim.tv_sec * 1000000000LL +
                (long long)st.st_mtim.tv_nsec;
#endif
    *size = (long long)st.st_size;
    return 0;
}

/* Filesystem identity used by behavioral tests that prove a protected path was
   not replaced. */
int fo_c_stat_identity(const char *path, long long *device, long long *inode) {
    struct stat st;
    if (!fo_has(path) || device == NULL || inode == NULL || stat(path, &st) != 0)
        return -1;
    *device = (long long)st.st_dev;
    *inode = (long long)st.st_ino;
    return 0;
}

/* Complete stat key for file-content memoization. ctime changes when a file is
   rewritten even if its original mtime is restored with utimensat/touch. */
int fo_c_stat_change_fingerprint(const char *path, long long *mtime_ns,
                                 long long *ctime_ns, long long *size) {
    struct stat st;
    if (!fo_has(path) || mtime_ns == NULL || ctime_ns == NULL || size == NULL ||
        stat(path, &st) != 0)
        return -1;
#if defined(__APPLE__)
    *mtime_ns = (long long)st.st_mtimespec.tv_sec * 1000000000LL +
                (long long)st.st_mtimespec.tv_nsec;
    *ctime_ns = (long long)st.st_ctimespec.tv_sec * 1000000000LL +
                (long long)st.st_ctimespec.tv_nsec;
#else
    *mtime_ns = (long long)st.st_mtim.tv_sec * 1000000000LL +
                (long long)st.st_mtim.tv_nsec;
    *ctime_ns = (long long)st.st_ctim.tv_sec * 1000000000LL +
                (long long)st.st_ctim.tv_nsec;
#endif
    *size = (long long)st.st_size;
    return 0;
}

static unsigned long long fo_fnv1a_bytes(unsigned long long hash,
                                         const void *data, size_t len) {
    const unsigned char *bytes = data;
    size_t i;
    for (i = 0; i < len; i++) {
        hash ^= (unsigned long long)bytes[i];
        hash *= 1099511628211ULL;
    }
    return hash;
}

static void fo_fingerprint_file(const char *path, const struct stat *st,
                                unsigned long long *sum,
                                unsigned long long *mixed, long long *count) {
    unsigned long long item = 1469598103934665603ULL;
    item = fo_fnv1a_bytes(item, path, strlen(path));
#if defined(__APPLE__)
    item = fo_fnv1a_bytes(item, &st->st_mtimespec, sizeof(st->st_mtimespec));
    item = fo_fnv1a_bytes(item, &st->st_ctimespec, sizeof(st->st_ctimespec));
#else
    item = fo_fnv1a_bytes(item, &st->st_mtim, sizeof(st->st_mtim));
    item = fo_fnv1a_bytes(item, &st->st_ctim, sizeof(st->st_ctim));
#endif
    item = fo_fnv1a_bytes(item, &st->st_size, sizeof(st->st_size));
    *sum += item;
    *mixed ^= (item << (item & 31)) | (item >> ((64 - (item & 31)) & 63));
    (*count)++;
}

static int fo_tree_fingerprint_rec(const char *root, int input_mode, int depth,
                                   unsigned long long *sum,
                                   unsigned long long *mixed,
                                   long long *count) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];
    struct stat st;

    dir = opendir(root);
    if (dir == NULL) return -1;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (ent->d_name[0] == '.') continue;
        if (input_mode) {
            if (strcmp(ent->d_name, "node_modules") == 0 ||
                strcmp(ent->d_name, "venv") == 0 ||
                strcmp(ent->d_name, "__pycache__") == 0 ||
                strcmp(ent->d_name, "site-packages") == 0)
                continue;
        }
        if (snprintf(child, sizeof(child), "%s/%s", root, ent->d_name) >=
            (int)sizeof(child))
            continue;
        if (lstat(child, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            if (input_mode && depth == 0 && strcmp(ent->d_name, "build") == 0)
                continue;
            if (fo_tree_fingerprint_rec(child, input_mode, depth + 1, sum,
                                        mixed, count) != 0) {
                closedir(dir);
                return -1;
            }
            continue;
        }
        if (!S_ISREG(st.st_mode)) continue;
        fo_fingerprint_file(child, &st, sum, mixed, count);
    }
    closedir(dir);
    return 0;
}

/* Order-independent fingerprint of every regular file below root. Input mode
   ignores hidden/generated trees; output mode includes generated files but
   still ignores transient hidden directories such as build/fo/.lock. */
int fo_c_tree_fingerprint(const char *root, int input_mode,
                          long long *sum, long long *mixed, long long *count) {
    unsigned long long usum = 0, umixed = 0;
    long long n = 0;
    int rc;
    struct stat st;
    if (!fo_has(root)) return -1;
    if (stat(root, &st) != 0) return -1;
    if (S_ISREG(st.st_mode)) {
        fo_fingerprint_file(root, &st, &usum, &umixed, &n);
        rc = 0;
    } else {
        rc = fo_tree_fingerprint_rec(root, input_mode, 0, &usum, &umixed, &n);
    }
    *sum = (long long)usum;
    *mixed = (long long)umixed;
    *count = n;
    return rc;
}

static int fo_executable_exists(const char *path) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    wchar_t *wide = fx_win32_wide(path);
    DWORD attributes, binary_type;
    if (!wide) return 0;
    attributes = GetFileAttributesW(wide);
    int executable = attributes != INVALID_FILE_ATTRIBUTES &&
                     !(attributes & FILE_ATTRIBUTE_DIRECTORY) &&
                     GetBinaryTypeW(wide, &binary_type) &&
                     (binary_type == SCS_32BIT_BINARY || binary_type == SCS_64BIT_BINARY);
    free(wide);
    return executable;
#else
    return access(path, X_OK) == 0;
#endif
}

static int fo_resolve_executable(char *candidate, size_t capacity) {
    if (fo_executable_exists(candidate)) return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    size_t length = strlen(candidate);
    if (length + 5 > capacity) return -1;
    memcpy(candidate + length, ".exe", 5);
    if (fo_executable_exists(candidate)) return 0;
#else
    (void)capacity;
#endif
    return -1;
}

int fo_c_find_executable(const char *command, char *out, int cap) {
    const char *path_env, *start, *end;
    char candidate[PATH_MAX];
    size_t dir_len;

    if (!fo_has(command) || out == NULL || cap <= 0) return -1;
    if (strpbrk(command, " \t\r\n") != NULL) return -1;
    if (strchr(command, '/') != NULL
#if defined(_WIN32) && !defined(__CYGWIN__)
        || strchr(command, '\\') != NULL || strchr(command, ':') != NULL
#endif
        ) {
        if (strlen(command) >= sizeof(candidate)) return -1;
        strcpy(candidate, command);
        if (fo_resolve_executable(candidate, sizeof(candidate)) != 0) return -1;
        if (realpath(candidate, candidate) == NULL) return -1;
        if ((int)strlen(candidate) + 1 > cap) return -1;
        strcpy(out, candidate);
        return 0;
    }
    path_env = getenv("PATH");
    if (path_env == NULL) return -1;
    start = path_env;
    while (1) {
        end = strchr(start,
#if defined(_WIN32) && !defined(__CYGWIN__)
                     ';'
#else
                     ':'
#endif
                    );
        dir_len = (end != NULL) ? (size_t)(end - start) : strlen(start);
        if (dir_len == 0) {
            if (snprintf(candidate, sizeof(candidate), "./%s", command) >=
                (int)sizeof(candidate))
                return -1;
        } else {
            if (snprintf(candidate, sizeof(candidate), "%.*s/%s", (int)dir_len,
                         start, command) >= (int)sizeof(candidate))
                return -1;
        }
        if (fo_resolve_executable(candidate, sizeof(candidate)) == 0) {
            char resolved[PATH_MAX];
            if (realpath(candidate, resolved) == NULL) return -1;
            if ((int)strlen(resolved) + 1 > cap) return -1;
            strcpy(out, resolved);
            return 0;
        }
        if (end == NULL) break;
        start = end + 1;
    }
    return -1;
}

#include <time.h>
/* Sleep for the given milliseconds (no shell `sleep`). */
void fo_c_sleep_ms(int ms) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (ms > 0) Sleep((DWORD)ms);
#else
    struct timespec ts;
    if (ms <= 0) return;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (long)(ms % 1000) * 1000000L;
    nanosleep(&ts, NULL);
#endif
}

/* Recursively collect the unique parent directories of every *.mod file under
   root, NUL-separated in out. Returns the count, or -1 on overflow. Replaces
   `find -name '*.mod' -printf '%h\n' | sort -u`. */
static int fo_already_listed(const char *out, int used, const char *dirpath) {
    int i = 0;
    while (i < used) {
        if (strcmp(out + i, dirpath) == 0) return 1;
        i += (int)strlen(out + i) + 1;
    }
    return 0;
}

static int fo_collect_mod_dirs_rec(const char *root, char *out, int cap,
                                   int *used) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];
    struct stat st;
    size_t nlen, rlen;
    int count = 0, sub, have_mod = 0;

    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : 0;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", root, ent->d_name) >=
            (int)sizeof(child)) {
            continue;
        }
        if (lstat(child, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            sub = fo_collect_mod_dirs_rec(child, out, cap, used);
            if (sub < 0) { closedir(dir); return -1; }
            count += sub;
            continue;
        }
        nlen = strlen(ent->d_name);
        if (nlen >= 4 && strcmp(ent->d_name + (nlen - 4), ".mod") == 0)
            have_mod = 1;
    }
    closedir(dir);
    if (have_mod && !fo_already_listed(out, *used, root)) {
        rlen = strlen(root);
        if (*used + (int)rlen + 1 > cap) return -1;
        memcpy(out + *used, root, rlen);
        out[*used + (int)rlen] = '\0';
        *used += (int)rlen + 1;
        count++;
    }
    return count;
}

int fo_c_collect_mod_dirs(const char *root, char *out, int cap) {
    int used = 0;
    if (!fo_has(root)) return 0;
    return fo_collect_mod_dirs_rec(root, out, cap, &used);
}

/* Copy src to dst (truncating dst), setting dst's mode to 0755 so an installed
   binary stays executable. Replaces cp -f for the install path. */
int fo_c_copy_exec(const char *src, const char *dst) {
    FILE *in, *out;
    char buf[65536];
    size_t n;

    if (!fo_has(src) || !fo_has(dst)) return -1;
    in = fopen(src, "rb");
    if (in == NULL) return -1;
    out = fopen(dst, "wb");
    if (out == NULL) { fclose(in); return -1; }
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) { fclose(in); fclose(out); return -1; }
    }
    fclose(in);
    fclose(out);
    if (chmod(dst, 0755) != 0) return -1;
    return 0;
}

/* Rename src to dst (atomic within a filesystem). Replaces mv -f. */
int fo_c_rename_path(const char *src, const char *dst) {
    if (!fo_has(src) || !fo_has(dst)) return -1;
    if (rename(src, dst) != 0) return -1;
    return 0;
}

/* Append the bytes of src onto dst (mirrors cat src >> dst). */
int fo_c_append_file(const char *src, const char *dst) {
    FILE *in, *out;
    char buf[65536];
    size_t n;

    if (!fo_has(src) || !fo_has(dst)) return -1;
    in = fopen(src, "rb");
    if (in == NULL) return -1;
    out = fopen(dst, "ab");
    if (out == NULL) { fclose(in); return -1; }
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) { fclose(in); fclose(out); return -1; }
    }
    fclose(in);
    fclose(out);
    return 0;
}
