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

#include "../util/fo_sys.h"

#define LIFECYCLE_MAX (256 * 1024)

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

static int lifecycle_line_id(const char *line, size_t length, const char *event_id) {
    static const char marker[] = "\"event_id\":\"";
    const char *field, *end;
    if (length < 2 || line[0] != '{' || line[length - 1] != '}') return -1;
    field = strstr(line, marker);
    end = field == NULL ? NULL : strchr(field + sizeof(marker) - 1, '"');
    if (end == NULL) return -1;
    field += sizeof(marker) - 1;
    return (size_t)(end - field) == strlen(event_id) &&
           memcmp(field, event_id, strlen(event_id)) == 0;
}

int fo_c_gremlin_lifecycle_append(const char *path, const char *event_id,
                                  const char *record) {
    size_t record_length;

    if (path == NULL || event_id == NULL || record == NULL || path[0] == '\0' ||
        event_id[0] == '\0') return 1;
    record_length = strlen(record);
    if (record_length > LIFECYCLE_MAX ||
        !event_id_matches(record, record_length, event_id)) return 1;
    return fo_sys_append_once(path, event_id, record, record_length,
                              LIFECYCLE_MAX, 1, lifecycle_line_id);
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
