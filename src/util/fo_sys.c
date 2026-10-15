#ifdef __APPLE__
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#else
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
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
#include <sys/types.h>
#include <unistd.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif
#ifdef __APPLE__
#include <libproc.h>
#include <sys/proc.h>
#endif
#include "fo_sys.h"

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

uint64_t fo_fnv1a(uint64_t hash, const void *data, size_t length) {
    const unsigned char *bytes = data;
    for (size_t i = 0; i < length; ++i) {
        hash ^= bytes[i];
        hash *= UINT64_C(1099511628211);
    }
    return hash;
}

uint64_t fo_fnv1a_text(uint64_t hash, const char *text) {
    return fo_fnv1a(hash, text, strlen(text));
}

int fo_sys_write_all(int fd, const void *data, size_t length) {
    const char *bytes = data;
    while (length > 0) {
        ssize_t written = write(fd, bytes, length);
        if (written < 0) {
            if (errno == EINTR) continue;
            return errno;
        }
        if (written == 0) return EIO;
        bytes += written;
        length -= (size_t)written;
    }
    return 0;
}

int fo_sys_sync_dir(const char *dir) {
    int flags = O_RDONLY | O_CLOEXEC;
#ifdef O_DIRECTORY
    flags |= O_DIRECTORY;
#endif
    int fd = open(dir, flags), e;
    if (fd < 0) return errno;
    e = fsync(fd) == 0 ? 0 : errno;
    close(fd);
    return e;
}

int fo_sys_sync_parent(const char *path) {
    char parent[PATH_MAX], *slash;
    size_t length = strlen(path);
    if (length >= sizeof(parent)) return ENAMETOOLONG;
    memcpy(parent, path, length + 1);
    slash = strrchr(parent, '/');
    if (slash == NULL) strcpy(parent, ".");
    else if (slash == parent) slash[1] = '\0';
    else *slash = '\0';
    return fo_sys_sync_dir(parent);
}

int fo_sys_mkdirs(const char *path, int mode) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (path == NULL || path[0] == '\0') return EINVAL;
    return fx_win_mkdirs_mode(path, mode, 0) == 0 ? 0 : (errno ? errno : EIO);
#else
    char tmp[PATH_MAX];
    struct stat st;
    size_t length;
    if (path == NULL || path[0] == '\0') return EINVAL;
    length = strlen(path);
    while (length > 1 && path[length - 1] == '/') --length;
    if (length >= sizeof(tmp)) return ENAMETOOLONG;
    memcpy(tmp, path, length);
    tmp[length] = '\0';
    if (stat(tmp, &st) == 0) return S_ISDIR(st.st_mode) ? 0 : ENOTDIR;
    for (char *p = tmp + 1; *p; ++p) {
        if (*p != '/') continue;
        *p = '\0';
        if (mkdir(tmp, (mode_t)mode) != 0 && errno != EEXIST) return errno;
        *p = '/';
    }
    if (mkdir(tmp, (mode_t)mode) != 0 && errno != EEXIST) return errno;
    if (stat(tmp, &st) != 0) return errno;
    return S_ISDIR(st.st_mode) ? 0 : ENOTDIR;
#endif
}

int fo_sys_publish_file(const char *path, const void *data, size_t length) {
    char temporary[PATH_MAX];
    int fd, e;
    if (snprintf(temporary, sizeof(temporary), "%s.tmp.XXXXXX", path) >=
        (int)sizeof(temporary)) return ENAMETOOLONG;
    fd = mkstemp(temporary);
    if (fd < 0) return errno;
    e = fchmod(fd, 0600) == 0 ? 0 : errno;
    if (e == 0) e = fo_sys_write_all(fd, data, length);
    if (e == 0 && fsync(fd) != 0) e = errno;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e == 0 && rename(temporary, path) != 0) e = errno;
    if (e == 0) e = fo_sys_sync_parent(path);
    if (e != 0) unlink(temporary);
    return e;
}

int fo_sys_lock_file(const char *path, int extra_flags, int mode, int operation) {
    int fd = open(path, O_CREAT | O_RDWR | extra_flags, mode), result;
    if (fd < 0) return -errno;
    do { result = flock(fd, operation); } while (result != 0 && errno == EINTR);
    if (result != 0) {
        result = errno;
        close(fd);
        return -result;
    }
    return fd;
}

void fo_sys_unlock(int fd) {
    if (fd >= 0) { flock(fd, LOCK_UN); close(fd); }
}

int fo_sys_append_once(const char *path, const char *id, const char *record,
                       size_t length, size_t line_max, int duplicate_result,
                       fo_sys_line_id_fn line_id) {
    int fd, result = 2, found = 0, identical = 0, oversized = 0;
    size_t line_length = 0;
    off_t scan = 0, complete_end = 0, size;
    struct stat st;
    char chunk[8192], *line = malloc(line_max + 1);
    if (line == NULL) return 2;
    fd = open(path, O_CREAT | O_RDWR | O_APPEND | O_CLOEXEC, 0600);
    if (fd < 0) { free(line); return 2; }
    if (flock(fd, LOCK_EX) != 0 || fstat(fd, &st) != 0) goto done;
    size = st.st_size;
    while (scan < size) {
        size_t want = sizeof(chunk);
        ssize_t count;
        if ((off_t)want > size - scan) want = (size_t)(size - scan);
        count = pread(fd, chunk, want, scan);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) goto done;
        for (ssize_t i = 0; i < count; ++i) {
            if (chunk[i] != '\n') {
                if (oversized) continue;
                if (line_length == line_max) oversized = 1;
                else line[line_length++] = chunk[i];
                continue;
            }
            if (oversized) { result = 4; goto done; }
            line[line_length] = '\0';
            switch (line_id(line, line_length, id)) {
            case -1: result = 1; goto done;
            case 1:
                if (found) { result = duplicate_result; goto done; }
                found = 1;
                identical = line_length == length && memcmp(line, record, length) == 0;
                break;
            default: break;
            }
            complete_end = scan + (off_t)i + 1;
            line_length = 0;
        }
        scan += count;
    }
    if (complete_end < size && ftruncate(fd, complete_end) != 0) goto done;
    if (found) {
        result = !identical ? 3 :
                 (fsync(fd) == 0 && fo_sys_sync_parent(path) == 0 ? 0 : 2);
        goto done;
    }
    if (lseek(fd, 0, SEEK_END) < 0 ||
        fo_sys_write_all(fd, record, length) != 0 ||
        fo_sys_write_all(fd, "\n", 1) != 0 ||
        fsync(fd) != 0 || fo_sys_sync_parent(path) != 0) goto done;
    result = 0;
done:
    flock(fd, LOCK_UN);
    close(fd);
    free(line);
    return result;
}

#if !defined(_WIN32) || defined(__CYGWIN__)
int fo_sys_process_info(pid_t pid, pid_t *parent, pid_t *group, pid_t *session,
                        uint64_t *start, int *alive) {
#ifdef __APPLE__
    struct proc_bsdinfo info;
    pid_t sid = 0;
    int bytes;

    if (pid <= 0) return ESRCH;
    bytes = proc_pidinfo((int)pid, PROC_PIDTBSDINFO, 0, &info, (int)sizeof(info));
    if (bytes != (int)sizeof(info) || info.pbi_pid != (uint32_t)pid) return ESRCH;
    if (session != NULL) {
        sid = getsid(pid);
        if (sid <= 0 && info.pbi_status != SZOMB)
            return errno != 0 ? errno : ESRCH;
        *session = sid;
    }
    if (parent != NULL) *parent = (pid_t)info.pbi_ppid;
    if (group != NULL) *group = (pid_t)info.pbi_pgid;
    if (alive != NULL) *alive = info.pbi_status != SZOMB;
    if (start != NULL) {
        *start = (uint64_t)info.pbi_start_tvsec * 1000000ULL +
                 (uint64_t)info.pbi_start_tvusec;
        if (*start == 0) return ESRCH;
    }
    return 0;
#else
    char path[64], line[4096], *end, *cursor;
    char state;
    long ppid, pgid, sid;
    uint64_t identity = 0;
    FILE *file;
    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    file = fopen(path, "r");
    if (file == NULL) return errno;
    if (fgets(line, sizeof(line), file) == NULL) {
        fclose(file);
        return ESRCH;
    }
    fclose(file);
    end = strrchr(line, ')');
    if (end == NULL || sscanf(end + 1, " %c %ld %ld %ld",
                              &state, &ppid, &pgid, &sid) != 4) return EIO;
    cursor = end + 1;
    for (int field = 3; field <= 22; field++) {
        while (*cursor == ' ') cursor++;
        if (*cursor == '\0') return EIO;
        end = cursor;
        while (*end != '\0' && *end != ' ') end++;
        if (field == 22) {
            identity = strtoull(cursor, NULL, 10);
            break;
        }
        cursor = end;
    }
    if (identity == 0) return ESRCH;
    if (parent != NULL) *parent = (pid_t)ppid;
    if (group != NULL) *group = (pid_t)pgid;
    if (session != NULL) *session = (pid_t)sid;
    if (start != NULL) *start = identity;
    if (alive != NULL) *alive = state != 'Z' && state != 'X';
    return 0;
#endif
}

uint64_t fo_sys_process_start(pid_t pid) {
    uint64_t start = 0;
    return fo_sys_process_info(pid, NULL, NULL, NULL, &start, NULL) == 0 ? start : 0;
}
#endif

int fo_sys_identity_text(uint64_t identity, char *out, size_t cap) {
    int n;
    if (identity == 0) return ESRCH;
#ifdef __APPLE__
    n = snprintf(out, cap, "%llu.%06llu",
                 (unsigned long long)(identity / 1000000ULL),
                 (unsigned long long)(identity % 1000000ULL));
#else
    n = snprintf(out, cap, "%llu", (unsigned long long)identity);
#endif
    return n < 0 || (size_t)n >= cap ? ENAMETOOLONG : 0;
}

int fo_sys_process_start_text(pid_t pid, char *out, size_t cap) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    FILETIME creation, exit_time, kernel, user;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE,
                                  FALSE, (DWORD)pid);
    ULARGE_INTEGER born;
    DWORD error;
    if (!process) return GetLastError() == ERROR_ACCESS_DENIED ? EACCES : ESRCH;
    if (WaitForSingleObject(process, 0) != WAIT_TIMEOUT) {
        CloseHandle(process);
        return ESRCH;
    }
    if (!GetProcessTimes(process, &creation, &exit_time, &kernel, &user)) {
        error = GetLastError();
        CloseHandle(process);
        return error == ERROR_ACCESS_DENIED ? EACCES : EIO;
    }
    CloseHandle(process);
    born.LowPart = creation.dwLowDateTime;
    born.HighPart = creation.dwHighDateTime;
    if (snprintf(out, cap, "%llu", (unsigned long long)born.QuadPart) >= (int)cap)
        return ENAMETOOLONG;
    return 0;
#else
    uint64_t start = 0;
    int alive = 0, e = fo_sys_process_info(pid, NULL, NULL, NULL, &start, &alive);
    if (e != 0) return e;
    if (!alive) return ESRCH;
    return fo_sys_identity_text(start, out, cap);
#endif
}
