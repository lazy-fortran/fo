#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif

static int write_all(int fd, const char *data, size_t length) {
    size_t offset = 0;
    while (offset < length) {
        ssize_t written = write(fd, data + offset, length - offset);
        if (written < 0) {
            if (errno == EINTR) continue;
            return errno;
        }
        if (written == 0) return EIO;
        offset += (size_t)written;
    }
    return 0;
}

/* The caller holds this lock from the first read through durable publication. */
int fo_c_gremlin_coverage_lock(const char *path) {
    size_t length;
    char *lock_path;
    int fd, result;
    if (path == NULL || path[0] == '\0') return -EINVAL;
    length = strlen(path) + sizeof(".lock");
    lock_path = malloc(length);
    if (lock_path == NULL) return -ENOMEM;
    snprintf(lock_path, length, "%s.lock", path);
    fd = open(lock_path, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    free(lock_path);
    if (fd < 0) return -errno;
    do { result = flock(fd, LOCK_EX); } while (result != 0 && errno == EINTR);
    if (result != 0) { result = errno; close(fd); return -result; }
    return fd;
}

void fo_c_gremlin_coverage_unlock(int fd) {
    if (fd >= 0) { flock(fd, LOCK_UN); close(fd); }
}

int fo_c_gremlin_coverage_publish(const char *path, const char *data,
                                  size_t data_length) {
    char *temp_path = NULL, *directory = NULL, *slash;
    int temp_fd = -1, dir_fd = -1, result = 0;
    size_t path_length;

    if (path == NULL || path[0] == '\0' || data == NULL) return EINVAL;
    path_length = strlen(path);
    temp_path = malloc(path_length + sizeof(".tmp.XXXXXX"));
    directory = strdup(path);
    if (temp_path == NULL || directory == NULL) {
        result = ENOMEM;
        goto done;
    }
    snprintf(temp_path, path_length + sizeof(".tmp.XXXXXX"), "%s.tmp.XXXXXX", path);
    temp_fd = mkstemp(temp_path);
    if (temp_fd < 0) { result = errno; goto done; }
    if (fchmod(temp_fd, 0600) != 0) { result = errno; goto done; }
    result = write_all(temp_fd, data, data_length);
    if (result != 0) goto done;
    if (fsync(temp_fd) != 0) { result = errno; goto done; }
    if (close(temp_fd) != 0) { temp_fd = -1; result = errno; goto done; }
    temp_fd = -1;
    if (rename(temp_path, path) != 0) { result = errno; goto done; }
    slash = strrchr(directory, '/');
    if (slash == NULL) {
        free(directory);
        directory = strdup(".");
        if (directory == NULL) { result = ENOMEM; goto done; }
    } else if (slash == directory) {
        slash[1] = '\0';
    } else {
        *slash = '\0';
    }
    dir_fd = open(directory, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (dir_fd < 0) { result = errno; goto done; }
    if (fsync(dir_fd) != 0) result = errno;

done:
    if (temp_fd >= 0) close(temp_fd);
    if (result != 0 && temp_path != NULL) unlink(temp_path);
    if (dir_fd >= 0) close(dir_fd);
    free(temp_path);
    free(directory);
    return result;
}
