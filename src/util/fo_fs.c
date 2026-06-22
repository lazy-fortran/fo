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

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int fo_has(const char *s) { return s != NULL && s[0] != '\0'; }

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
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, ent->d_name) >=
            (int)sizeof(child)) {
            closedir(dir);
            return -1;
        }
        if (fo_c_rm_rf(child) != 0) {
            closedir(dir);
            return -1;
        }
    }
    closedir(dir);
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

/* Recursively collect regular files under root whose basename contains infix
   and ends with suffix, and whose full path contains path_needle (when set).
   Matches are written to out as NUL-separated paths; returns the count, or -1
   if the buffer overflows or a hard error occurs. Replaces a find pipeline. */
static int fo_collect_rec(const char *root, const char *infix,
                          const char *suffix, const char *path_needle,
                          int recursive, char *out, int cap, int *used) {
    DIR *dir;
    struct dirent *ent;
    char child[PATH_MAX];
    struct stat st;
    size_t nlen, slen, plen;
    int count = 0, sub;

    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : 0;
    slen = (suffix != NULL) ? strlen(suffix) : 0;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", root, ent->d_name) >=
            (int)sizeof(child)) {
            continue;
        }
        if (lstat(child, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            if (!recursive) continue;
            sub = fo_collect_rec(child, infix, suffix, path_needle, recursive,
                                 out, cap, used);
            if (sub < 0) { closedir(dir); return -1; }
            count += sub;
            continue;
        }
        if (!S_ISREG(st.st_mode)) continue;
        nlen = strlen(ent->d_name);
        if (slen > 0 && (nlen < slen ||
            strcmp(ent->d_name + (nlen - slen), suffix) != 0)) continue;
        if (!fo_str_contains(ent->d_name, infix)) continue;
        if (!fo_str_contains(child, path_needle)) continue;
        plen = strlen(child);
        if (*used + (int)plen + 1 > cap) { closedir(dir); return -1; }
        memcpy(out + *used, child, plen);
        out[*used + (int)plen] = '\0';
        *used += (int)plen + 1;
        count++;
    }
    closedir(dir);
    return count;
}

int fo_c_collect_files(const char *root, const char *infix, const char *suffix,
                       const char *path_needle, int recursive, char *out,
                       int cap) {
    int used = 0;
    if (!fo_has(root)) return 0;
    return fo_collect_rec(root, infix, suffix, path_needle, recursive, out, cap,
                          &used);
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
    if (kill((pid_t)pid, 0) == 0) return 1;
    return (errno == EPERM) ? 1 : 0;
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

#include <time.h>
/* Sleep for the given milliseconds (no shell `sleep`). */
void fo_c_sleep_ms(int ms) {
    struct timespec ts;
    if (ms <= 0) return;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (long)(ms % 1000) * 1000000L;
    nanosleep(&ts, NULL);
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
