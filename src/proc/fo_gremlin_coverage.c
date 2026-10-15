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

#include "../util/fo_sys.h"

/* The caller holds this lock from the first read through durable publication. */
int fo_c_gremlin_coverage_lock(const char *path) {
    size_t length;
    char *lock_path;
    int fd;
    if (path == NULL || path[0] == '\0') return -EINVAL;
    length = strlen(path) + sizeof(".lock");
    lock_path = malloc(length);
    if (lock_path == NULL) return -ENOMEM;
    snprintf(lock_path, length, "%s.lock", path);
    fd = fo_sys_lock_file(lock_path, O_CLOEXEC, 0600, LOCK_EX);
    free(lock_path);
    return fd;
}

void fo_c_gremlin_coverage_unlock(int fd) {
    fo_sys_unlock(fd);
}

int fo_c_gremlin_coverage_publish(const char *path, const char *data,
                                  size_t data_length) {
    if (path == NULL || path[0] == '\0' || data == NULL) return EINVAL;
    return fo_sys_publish_file(path, data, data_length);
}
