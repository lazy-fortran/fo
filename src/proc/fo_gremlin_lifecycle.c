#ifdef __APPLE__
#define _DARWIN_C_SOURCE 1
#else
#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif

#define LIFECYCLE_MAX (256 * 1024)

static int sync_parent(const char *path) {
    char *copy = strdup(path);
    char *slash;
    int fd, result;
    if (copy == NULL) return -1;
    slash = strrchr(copy, '/');
    if (slash == NULL) strcpy(copy, ".");
    else if (slash == copy) slash[1] = '\0';
    else *slash = '\0';
    fd = open(copy, O_RDONLY | O_DIRECTORY);
    free(copy);
    if (fd < 0) return -1;
    result = fsync(fd);
    close(fd);
    return result;
}

static int event_id_matches(const char *record, size_t length,
                            const char *event_id) {
    static const char marker[] = "\"event_id\":\"";
    const char *field, *end;
    size_t id_length = strlen(event_id);
    if (length < 2 || record[0] != '{' || record[length - 1] != '}') return 0;
    field = strstr(record, marker);
    if (field == NULL || strstr(field + sizeof(marker) - 1, marker) != NULL) return 0;
    field += sizeof(marker) - 1;
    end = memchr(field, '"', record + length - field);
    if (end == NULL || (size_t)(end - field) != id_length) return 0;
    return memcmp(field, event_id, id_length) == 0;
}

int fo_c_gremlin_lifecycle_append(const char *path, const char *event_id,
                                  const char *record) {
    int fd = -1, result = 2, found = 0, identical = 0;
    size_t record_length, line_length = 0, written = 0;
    off_t scan = 0, complete_end = 0, file_size;
    struct stat st;
    char line[LIFECYCLE_MAX + 1], chunk[8192];

    if (path == NULL || event_id == NULL || record == NULL || path[0] == '\0' ||
        event_id[0] == '\0') return 1;
    record_length = strlen(record);
    if (record_length > LIFECYCLE_MAX ||
        !event_id_matches(record, record_length, event_id)) return 1;

    fd = open(path, O_CREAT | O_RDWR | O_APPEND | O_CLOEXEC, 0600);
    if (fd < 0) return 2;
    if (flock(fd, LOCK_EX) != 0 || fstat(fd, &st) != 0) goto done;
    file_size = st.st_size;
    while (scan < file_size) {
        size_t want = sizeof(chunk), i;
        off_t remaining = file_size - scan;
        ssize_t count;
        if ((off_t)want > remaining) want = (size_t)remaining;
        count = pread(fd, chunk, want, scan);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) goto done;
        for (i = 0; i < (size_t)count; ++i) {
            if (chunk[i] == '\n') {
                line[line_length] = '\0';
                if (line_length < 2 || line[0] != '{' || line[line_length - 1] != '}') {
                    result = 1;
                    goto done;
                }
                {
                    static const char marker[] = "\"event_id\":\"";
                    const char *field = strstr(line, marker);
                    const char *end = field == NULL ? NULL : strchr(field + sizeof(marker) - 1, '"');
                    if (field == NULL || end == NULL) { result = 1; goto done; }
                    field += sizeof(marker) - 1;
                    if ((size_t)(end - field) == strlen(event_id) &&
                        memcmp(field, event_id, strlen(event_id)) == 0) {
                        if (found) { result = 1; goto done; }
                        found = 1;
                        identical = line_length == record_length &&
                            memcmp(line, record, record_length) == 0;
                    }
                }
                complete_end = scan + (off_t)i + 1;
                line_length = 0;
            } else {
                if (line_length == LIFECYCLE_MAX) { result = 4; goto done; }
                line[line_length++] = chunk[i];
            }
        }
        scan += count;
    }
    if (complete_end < file_size && ftruncate(fd, complete_end) != 0) goto done;
    if (found) {
        result = identical && fsync(fd) == 0 && sync_parent(path) == 0 ? 0 :
                 (identical ? 2 : 3);
        goto done;
    }
    if (lseek(fd, 0, SEEK_END) < 0) goto done;
    while (written < record_length) {
        ssize_t count = write(fd, record + written, record_length - written);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) goto done;
        written += (size_t)count;
    }
    for (;;) {
        ssize_t count = write(fd, "\n", 1);
        if (count < 0 && errno == EINTR) continue;
        if (count != 1) goto done;
        break;
    }
    if (fsync(fd) != 0 || sync_parent(path) != 0) goto done;
    result = 0;
done:
    flock(fd, LOCK_UN);
    close(fd);
    return result;
}

/* Session-private freshness barrier. The owner snapshots requests before polling
 * its change provider and acknowledges only that snapshot after publication. */
int fo_c_gremlin_freshness_update(const char *path, int operation, int64_t *ticket) {
    uint64_t counters[2] = {0, 0};
    struct stat st;
    int fd, result = 0;
    if (!path || !*path || !ticket || operation < 0 || operation > 3) return EINVAL;
    fd = open(path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return errno;
    if (flock(fd, LOCK_EX | LOCK_NB) != 0) { result = errno; goto done; }
    if (fstat(fd, &st) != 0) { result = errno; goto done; }
    if (!S_ISREG(st.st_mode) || (st.st_size != 0 && st.st_size != sizeof(counters))) {
        result = EINVAL; goto done;
    }
    if (st.st_size && pread(fd, counters, sizeof(counters), 0) != sizeof(counters)) {
        result = EIO; goto done;
    }
    if (counters[0] > INT64_MAX || counters[1] > counters[0]) {
        result = EINVAL; goto done;
    }
    if (operation == 1) {
        if (counters[0] == INT64_MAX) { result = EOVERFLOW; goto done; }
        *ticket = (int64_t)++counters[0];
    } else if (operation == 2) {
        if (*ticket < 0 || (uint64_t)*ticket > counters[0]) {
            result = EINVAL; goto done;
        }
        if ((uint64_t)*ticket > counters[1]) counters[1] = (uint64_t)*ticket;
    } else {
        *ticket = (int64_t)counters[operation == 3 ? 1 : 0];
    }
    if ((operation == 1 || operation == 2) &&
        pwrite(fd, counters, sizeof(counters), 0) != sizeof(counters)) result = EIO;
done:
    close(fd);
    return result;
}
