#if defined(__APPLE__)
/* Darwin hides O_NOFOLLOW and mkdtemp behind strict POSIX feature macros. */
#define _DARWIN_C_SOURCE
#else
#define _XOPEN_SOURCE 700
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif

/* The private reference and matching scratch marker bind both directory
   identities. A published view rename preserves this ownership relationship. */
#define SCRATCH_REF ".fo-tmp-ref"
#define SCRATCH_OWNER ".fo-view-owner"
#define SCRATCH_PREFIX "fo-execution-tmp-"
struct scratch_reference {
    uint64_t magic, view_device, view_inode, scratch_device, scratch_inode;
    char path[4096];
};
#define SCRATCH_MAGIC UINT64_C(0x464f544d50524546)
int fo_c_rm_rf(const char *path);

static int private_fd(int fd) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    return fx_win_private_owned(fd) == 1;
#else
    struct stat st;
    return fstat(fd, &st) == 0 && st.st_uid == geteuid() &&
        (st.st_mode & 0077) == 0;
#endif
}

static int owned_directory(const char *path, struct stat *st) {
    struct stat held;
    int fd, e = 0;
    if (lstat(path, st) != 0) return errno;
    if (!S_ISDIR(st->st_mode)) return EPERM;
    fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW | O_DIRECTORY);
    if (fd < 0) return errno;
    if (fstat(fd, &held) != 0) e = errno;
    else if (!S_ISDIR(held.st_mode) || st->st_dev != held.st_dev ||
             st->st_ino != held.st_ino) e = EPERM;
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (fx_win_current_owned(fd) != 1) e = EPERM;
#else
    if (st->st_uid != geteuid()) e = EPERM;
#endif
    close(fd);
    return e;
}

static int reference_path(const char *dir, const char *name, char *path) {
    return snprintf(path, PATH_MAX, "%s/%s", dir, name) >= PATH_MAX ?
        ENAMETOOLONG : 0;
}

static int write_reference(const char *path, const struct scratch_reference *ref) {
    int fd = open(path, O_CREAT | O_EXCL | O_WRONLY | O_NOFOLLOW | O_CLOEXEC, 0600);
    int e = 0;
    if (fd < 0) return errno;
    if (!private_fd(fd)) e = EPERM;
    if (!e && pwrite(fd, ref, sizeof(*ref), 0) != (ssize_t)sizeof(*ref))
        e = errno ? errno : EIO;
    if (!e && fsync(fd) != 0) e = errno;
    if (close(fd) != 0 && !e) e = errno;
    if (e) unlink(path);
    return e;
}

static int read_reference(const char *path, struct scratch_reference *ref) {
    struct stat st;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW), e = 0;
    if (fd < 0) return errno;
    if (fstat(fd, &st) != 0) e = errno;
    else if (!S_ISREG(st.st_mode) || st.st_size != (int64_t)sizeof(*ref) ||
             !private_fd(fd)) e = EPERM;
    if (!e && pread(fd, ref, sizeof(*ref), 0) != (ssize_t)sizeof(*ref))
        e = errno ? errno : EIO;
    close(fd);
    return e;
}

int fo_c_execution_scratch_create(const char *view, const char *parent,
                                   char *out, int capacity) {
    struct stat vst, sst;
    struct scratch_reference ref = {0};
    char root[PATH_MAX], path[PATH_MAX], marker[PATH_MAX], reference[PATH_MAX];
    int e = owned_directory(view, &vst);
    if (capacity <= 0) return EINVAL;
    out[0] = '\0';
    if (e) return e;
    if (!realpath(parent, root)) return errno;
    if (snprintf(path, sizeof(path), "%s/%sXXXXXX", root, SCRATCH_PREFIX) >=
        (int)sizeof(path)) return ENAMETOOLONG;
    if (strlen(path) + 1 > (size_t)capacity) return ENAMETOOLONG;
    e = reference_path(view, SCRATCH_REF, reference);
    if (e) return e;
    if (!mkdtemp(path)) return errno;
    e = owned_directory(path, &sst);
    if (e) goto fail;
    ref.magic = SCRATCH_MAGIC;
    ref.view_device = vst.st_dev;
    ref.view_inode = vst.st_ino;
    ref.scratch_device = sst.st_dev;
    ref.scratch_inode = sst.st_ino;
    strcpy(ref.path, path);
    e = reference_path(path, SCRATCH_OWNER, marker);
    if (!e) e = write_reference(marker, &ref);
    if (!e) e = write_reference(reference, &ref);
    if (e) goto fail;
    strcpy(out, path);
    return 0;
fail:
    if (fo_c_rm_rf(path) != 0) return errno ? errno : EIO;
    return e;
}

/* Return 1 for a validated live scratch, 0 for no scratch, negative errno for
   an invalid reference. Never turn a malformed reference into a deletion. */
int fo_c_execution_scratch_active(const char *view) {
    struct scratch_reference ref, marker;
    struct stat vst, sst;
    char reference[PATH_MAX], owner[PATH_MAX], resolved[PATH_MAX];
    const char *base;
    int e = owned_directory(view, &vst);
    if (e) return -e;
    e = reference_path(view, SCRATCH_REF, reference);
    if (e) return -e;
    e = read_reference(reference, &ref);
    if (e) return e == ENOENT ? 0 : -e;
    if (ref.magic != SCRATCH_MAGIC || ref.view_device != (uint64_t)vst.st_dev ||
        ref.view_inode != (uint64_t)vst.st_ino ||
        !memchr(ref.path, '\0', sizeof(ref.path))) return -EPERM;
    base = strrchr(ref.path, '/');
    if (!base || strncmp(base + 1, SCRATCH_PREFIX, strlen(SCRATCH_PREFIX)) ||
        strlen(base + 1) != strlen(SCRATCH_PREFIX) + 6) return -EPERM;
    e = owned_directory(ref.path, &sst);
    if (e) return e == ENOENT ? 0 : -e;
    if (sst.st_dev != ref.scratch_device || sst.st_ino != ref.scratch_inode)
        return -EPERM;
    if (!realpath(ref.path, resolved)) return -errno;
    if (strcmp(resolved, ref.path)) return -EPERM;
    e = reference_path(ref.path, SCRATCH_OWNER, owner);
    if (!e) e = read_reference(owner, &marker);
    if (e) return -e;
    if (memcmp(&ref, &marker, sizeof(ref))) return -EPERM;
    return 1;
}

int fo_c_execution_scratch_release(const char *view) {
    struct scratch_reference ref;
    char reference[PATH_MAX];
    int active = fo_c_execution_scratch_active(view), e;
    if (active < 0) return -active;
    e = reference_path(view, SCRATCH_REF, reference);
    if (e) return e;
    if (active) {
        e = read_reference(reference, &ref);
        if (e) return e;
        if (fo_c_rm_rf(ref.path) != 0) return errno ? errno : EIO;
    }
    if (unlink(reference) != 0 && errno != ENOENT) return errno;
    return 0;
}

static int fixture_bytes(const char *path, uint64_t *bytes, int depth) {
    struct stat st;
    struct dirent *entry;
    char child[PATH_MAX];
    DIR *directory;
    int e = 0;
    if (depth > 64) return EOVERFLOW;
    if (lstat(path, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode)) {
        if (st.st_size < 0 || INT64_MAX - *bytes < (uint64_t)st.st_size)
            return EOVERFLOW;
        *bytes += (uint64_t)st.st_size;
        return 0;
    }
    directory = opendir(path);
    if (!directory) return errno;
    for (;;) {
        errno = 0;
        entry = readdir(directory);
        if (!entry) { e = errno; break; }
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        e = reference_path(path, entry->d_name, child);
        if (!e) e = fixture_bytes(child, bytes, depth + 1);
        if (e) break;
    }
    if (closedir(directory) != 0 && !e) e = errno;
    return e;
}

/* Explicitly retained fixture directories must survive every enclosing Fo TMP
   release. Only matching private ownership markers authorize leaving a TMP
   ancestor; a similarly named directory alone confers no authority. */
int fo_c_execution_scratch_preserve(const char *fixture, char *out, int capacity,
                                    uint64_t *bytes) {
    struct scratch_reference marker;
    struct stat source, held, ancestor;
    char path[PATH_MAX], scan[PATH_MAX], owner[PATH_MAX], parent[PATH_MAX] = "";
    char evidence[PATH_MAX], target[PATH_MAX];
    char *slash;
#if defined(_WIN32) && !defined(__CYGWIN__)
    char *root_end;
#endif
    size_t root_length = 1;
    int e;
    if (capacity <= 0 || !bytes) return EINVAL;
    out[0] = '\0';
    *bytes = 0;
    e = owned_directory(fixture, &source);
    if (e) return e;
    if (!realpath(fixture, path)) return errno;
    if (strlen(path) + 1 > (size_t)capacity) return ENAMETOOLONG;
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (path[1] == ':' && path[2] == '/') root_length = 3;
    else if (path[0] == '/' && path[1] == '/') {
        root_end = strchr(path + 2, '/');
        if (!root_end) return EINVAL;
        root_end = strchr(root_end + 1, '/');
        root_length = root_end ? (size_t)(root_end - path) : strlen(path);
    }
#endif
    strcpy(scan, path);
    while ((slash = strrchr(scan, '/')) != NULL &&
           (size_t)(slash - scan) >= root_length) {
        *slash = '\0';
        e = reference_path(scan, SCRATCH_OWNER, owner);
        if (e) return e;
        e = read_reference(owner, &marker);
        if (e == ENOENT) continue;
        if (e) return e;
        e = owned_directory(scan, &ancestor);
        if (e) return e;
        slash = strrchr(scan, '/');
        if (!slash || strncmp(slash + 1, SCRATCH_PREFIX, strlen(SCRATCH_PREFIX)) ||
            strlen(slash + 1) != strlen(SCRATCH_PREFIX) + 6) return EPERM;
        if (marker.magic != SCRATCH_MAGIC ||
            !memchr(marker.path, '\0', sizeof(marker.path)) ||
            strcmp(marker.path, scan) ||
            marker.scratch_device != (uint64_t)ancestor.st_dev ||
            marker.scratch_inode != (uint64_t)ancestor.st_ino) return EPERM;
        strcpy(parent, scan);
        slash = strrchr(parent, '/');
        if (!slash) return EPERM;
        if (slash == parent) slash[1] = '\0';
        else *slash = '\0';
    }
    e = fixture_bytes(path, bytes, 0);
    if (e) return e;
    if (!parent[0]) { strcpy(out, path); return 0; }
    if (snprintf(evidence, sizeof(evidence), "%s/fo-failed-evidence-XXXXXX", parent)
            >= (int)sizeof(evidence)) return ENAMETOOLONG;
    e = reference_path(evidence, "fixture", target);
    if (e || strlen(target) + 1 > (size_t)capacity) return ENAMETOOLONG;
    if (!mkdtemp(evidence)) return errno;
    e = reference_path(evidence, "fixture", target);
    if (!e) e = owned_directory(evidence, &held);
    if (!e) e = owned_directory(path, &held);
    if (!e && (source.st_dev != held.st_dev || source.st_ino != held.st_ino)) e = EPERM;
    if (!e && rename(path, target) != 0) e = errno;
    if (e) {
        if (rmdir(evidence) != 0) return errno;
        return e;
    }
    strcpy(out, target);
    return 0;
}
