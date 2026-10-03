#define _POSIX_C_SOURCE 200809L
/* Test-only POSIX wrappers; process supervision and assertions live in Fortran. */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

int fo_test_mkdtemp(char *pattern) {
    return mkdtemp(pattern) == NULL ? -1 : 0;
}

int fo_test_mkdirs(const char *path) {
    char *copy = strdup(path);
    if (copy == NULL || copy[0] == '\0') { free(copy); return -1; }
    size_t length = strlen(copy);
    while (length > 1 && copy[length - 1] == '/') copy[--length] = '\0';
    for (size_t i = 1; i <= length; ++i) {
        if (copy[i] != '/' && copy[i] != '\0') continue;
        char saved = copy[i];
        copy[i] = '\0';
        if (mkdir(copy, 0777) != 0 && errno != EEXIST) { free(copy); return -1; }
        copy[i] = saved;
    }
    free(copy);
    return 0;
}

static int remove_one(const char *path) {
    struct stat info;
    if (lstat(path, &info) != 0) return errno == ENOENT ? 0 : -1;
    if (!S_ISDIR(info.st_mode) || S_ISLNK(info.st_mode)) return unlink(path);
    DIR *directory = opendir(path);
    if (directory == NULL) return -1;
    struct dirent *entry;
    int result = 0;
    while ((entry = readdir(directory)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
        size_t size = strlen(path) + strlen(entry->d_name) + 2;
        char *child = malloc(size);
        if (child == NULL) { result = -1; break; }
        snprintf(child, size, "%s/%s", path, entry->d_name);
        result = remove_one(child);
        free(child);
        if (result != 0) break;
    }
    if (closedir(directory) != 0) result = -1;
    if (result == 0 && rmdir(path) != 0) result = -1;
    return result;
}

int fo_test_remove_tree(const char *path) { return remove_one(path); }
int fo_test_unlink(const char *path) { return unlink(path) == 0 || errno == ENOENT ? 0 : -1; }
int fo_test_rename(const char *source, const char *target) { return rename(source, target); }
int fo_test_symlink(const char *target, const char *link_path) { return symlink(target, link_path); }
int fo_test_getcwd(char *buffer, size_t size) { return getcwd(buffer, size) == NULL ? -1 : 0; }

int64_t fo_test_monotonic_ms(void) {
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    return (int64_t)now.tv_sec * 1000 + (int64_t)now.tv_nsec / 1000000;
}

int fo_test_set_nonblocking(int descriptor) {
    int flags = fcntl(descriptor, F_GETFL, 0);
    return flags < 0 ? -1 : fcntl(descriptor, F_SETFL, flags | O_NONBLOCK);
}

int fo_test_ignore_sigpipe(void) { return signal(SIGPIPE, SIG_IGN) == SIG_ERR ? -1 : 0; }
int fo_test_default_sigpipe(void) { return signal(SIGPIPE, SIG_DFL) == SIG_ERR ? -1 : 0; }

int64_t fo_test_read(int descriptor, char *buffer, size_t capacity) {
    ssize_t count = read(descriptor, buffer, capacity);
    if (count >= 0) return (int64_t)count;
    return errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? -2 : -1;
}

int64_t fo_test_write(int descriptor, const char *buffer, size_t length) {
    ssize_t count = write(descriptor, buffer, length);
    if (count >= 0) return (int64_t)count;
    if (errno == EPIPE) return -3;
    return errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? -2 : -1;
}

int fo_test_poll(struct pollfd *descriptors, size_t count, int timeout_ms) {
    int ready = poll(descriptors, (nfds_t)count, timeout_ms);
    return ready < 0 && errno == EINTR ? 0 : ready;
}
