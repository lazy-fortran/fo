#ifdef __APPLE__
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#else
#define _XOPEN_SOURCE 700
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
#include <sys/wait.h>
#endif
#include <time.h>
#include <unistd.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fx_win_store.h"
#endif
#include "../util/fo_private_path.h"
#include "../util/fo_windows_env.h"
#include "../util/fo_path.h"
#include "../util/fo_sys.h"

int fo_gremlin_process_matches(int pid, const char *start);
int fo_c_recover_async_scope(const char *state_dir, int owner_pid,
                             const char *owner_start);
int fo_c_execution_scratch_release(const char *view);
int fo_c_execution_scratch_active(const char *view);

/* A replacement owner has already recovered the previous owner's children.
   Preserve retained execution views but discard their disposable scratch. */
static int cleanup_view_scratch(const char *state_dir) {
    char views[PATH_MAX], view[PATH_MAX];
    struct stat st;
    struct dirent *entry;
    DIR *directory;
    int e = 0;

    if (snprintf(views, sizeof(views), "%s/views", state_dir) >= (int)sizeof(views))
        return ENAMETOOLONG;
    if (lstat(views, &st) != 0) return errno == ENOENT ? 0 : errno;
    if (!S_ISDIR(st.st_mode) || !fo_private_path(views)) return EPERM;
    directory = opendir(views);
    if (directory == NULL) return errno;
    for (;;) {
        errno = 0;
        entry = readdir(directory);
        if (entry == NULL) { e = errno; break; }
        if (strncmp(entry->d_name, "execution-", 10) != 0 &&
            strncmp(entry->d_name, ".building-execution-", 20) != 0) continue;
        if (snprintf(view, sizeof(view), "%s/%s", views, entry->d_name) >=
            (int)sizeof(view)) { e = ENAMETOOLONG; break; }
        if (lstat(view, &st) != 0) { e = errno; break; }
        if (!S_ISDIR(st.st_mode) || !fo_private_path(view)) { e = EPERM; break; }
        e = fo_c_execution_scratch_release(view);
        if (e != 0) break;
    }
    if (closedir(directory) != 0 && e == 0) e = errno;
    return e;
}

static int state_path(const char *project, const char *lane, char *out,
                      size_t cap, char *canonical, size_t canonical_cap,
                      int create_dirs) {
    char resolved[PATH_MAX];
    const char *base = getenv("FO_GREMLIN_STATE_DIR");
    if (!base || !*base) base = getenv("XDG_CACHE_HOME");
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (!base || !*base) base = getenv("LOCALAPPDATA");
#endif
    char fallback[PATH_MAX];
    if (!base || !*base) {
        const char *home = getenv("HOME");
        if (!home || !*home) return ENOENT;
        if (snprintf(fallback, sizeof(fallback), "%s/.cache", home) >= (int)sizeof(fallback))
            return ENAMETOOLONG;
        base = fallback;
    }
    if (!realpath(project, resolved)) return errno;
    if (strlen(resolved) + 1 > canonical_cap) return ENAMETOOLONG;
    strcpy(canonical, resolved);
    uint64_t hp = fo_fnv1a_text(FO_FNV1A_SEED, resolved);
    uint64_t hl = fo_fnv1a_text(FO_FNV1A_SEED, lane);
    if (snprintf(out, cap, "%s/fo/gremlin/projects/%016llx/%016llx",
                 base, (unsigned long long)hp, (unsigned long long)hl) >= (int)cap)
        return ENAMETOOLONG;
    return create_dirs ? fo_sys_mkdirs(out, 0700) : 0;
}

int fo_gremlin_session_state_dir(const char *project, const char *lane,
                                char *dir, size_t dir_capacity) {
    char canonical[PATH_MAX];
    return state_path(project, lane, dir, dir_capacity, canonical,
                      sizeof(canonical), 0);
}

static int atomic_write_file(const char *dir, const char *name,
                             const char *text, size_t length) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return ENAMETOOLONG;
    return fo_sys_publish_file(path, text, length);
}

static int read_owner(const char *dir, char *id, size_t idcap, int *pid,
                      char *start, size_t startcap) {
    char path[PATH_MAX], buf[4096];
    if (snprintf(path, sizeof(path), "%s/owner", dir) >= (int)sizeof(path)) return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    ssize_t n = read(fd, buf, sizeof(buf) - 1);
    int saved = errno; close(fd); errno = saved;
    if (n < 0) return errno;
    if ((size_t)n >= sizeof(buf) - 1) return EOVERFLOW;
    buf[n] = 0;
    char *line = strtok(buf, "\n");
    if (!line || strlen(line) + 1 > idcap) return EINVAL;
    strcpy(id, line);
    line = strtok(NULL, "\n");
    if (!line) return EINVAL;
    *pid = atoi(line);
    line = strtok(NULL, "\n");
    if (!line || strlen(line) + 1 > startcap) return EINVAL;
    strcpy(start, line);
    return 0;
}

static int read_recovery_source(const char *dir, char *id, size_t cap) {
    char path[PATH_MAX];
    if (cap < 2) return EINVAL;
    if (snprintf(path, sizeof(path), "%s/recovery.source", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    ssize_t n;
    do { n = read(fd, id, cap - 1); } while (n < 0 && errno == EINTR);
    int e = n < 0 ? errno : 0;
    char extra;
    if (e == 0 && (size_t)n == cap - 1) {
        ssize_t more;
        do { more = read(fd, &extra, 1); } while (more < 0 && errno == EINTR);
        if (more != 0) e = more < 0 ? errno : EOVERFLOW;
    }
    if (close(fd) != 0 && e == 0) e = errno;
    if (e) return e;
    if (n < 2 || n > 121 || id[n - 1] != '\n') return EINVAL;
    id[n - 1] = 0;
    for (ssize_t i = 0; i < n - 1; ++i) {
        unsigned char c = (unsigned char)id[i];
        if (!(c == '-' || c == '_' || (c >= '0' && c <= '9') ||
              (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))) return EINVAL;
    }
    return 0;
}

static int stop_path(const char *dir, char *path, size_t cap) {
    return snprintf(path, cap, "%s/stop.request", dir) >= (int)cap ? ENAMETOOLONG : 0;
}

int fo_gremlin_process_matches(int pid, const char *start);

static int task_path(const char *dir, char *path, size_t cap) {
    return snprintf(path, cap, "%s/task.owner", dir) >= (int)cap ? ENAMETOOLONG : 0;
}

/* Drops the stop request and the task binding of the previous owner. */
static void clear_stop(const char *dir) {
    char path[PATH_MAX];
    if (stop_path(dir, path, sizeof(path)) == 0) unlink(path);
    if (task_path(dir, path, sizeof(path)) == 0) unlink(path);
}

static int verify_owner_fd(const char *dir, int fd) {
    char path[PATH_MAX];
    struct stat held, expected;
    int check, e = 0;

    if (fd < 0) return EPERM;
    if (snprintf(path, sizeof(path), "%s/owner.lock", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    check = open(path, O_RDWR | O_CLOEXEC);
    if (check < 0) return errno;
    if (fstat(fd, &held) != 0 || fstat(check, &expected) != 0) e = errno;
    close(check);
    if (e != 0) return e;
    if (held.st_dev != expected.st_dev || held.st_ino != expected.st_ino)
        return EPERM;
    if (flock(fd, LOCK_EX | LOCK_NB) != 0) return EPERM;
    return 0;
}

int fo_gremlin_session_acquire(const char *project, const char *lane,
        char *dir, int dircap, char *session, int sessioncap,
        char *recovered, int recoveredcap, int *lockfd, int *owner, int *pid,
        char *start, int startcap) {
    char canonical[PATH_MAX], lockpath[PATH_MAX];
    char prior_id[128], prior_start[64];
    int prior_pid = 0;
    if (recoveredcap <= 0) return EINVAL;
    recovered[0] = '\0';
    int e = state_path(project, lane, dir, (size_t)dircap, canonical,
                       sizeof(canonical), 1);
    if (e) return e;
    if (snprintf(lockpath, sizeof(lockpath), "%s/owner.lock", dir) >= (int)sizeof(lockpath))
        return ENAMETOOLONG;
    int fd = open(lockpath, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    *lockfd = -1;
    *owner = 0;
    *pid = 0;
    int locked = 0;
    for (int attempt = 0; attempt < 100; ++attempt) {
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) {
            /* An evictor renames a dead lane away while holding its lock
               (fo_gremlin_lane_evict); then start over on a fresh directory. */
            if (verify_owner_fd(dir, fd) == 0) { locked = 1; break; }
            close(fd);
            e = fo_sys_mkdirs(dir, 0700);
            if (e) return e;
            fd = open(lockpath, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
            if (fd < 0) return errno;
            continue;
        }
        if (errno != EWOULDBLOCK && errno != EAGAIN) {
            e = errno;
            close(fd);
            return e;
        }
        e = read_owner(dir, session, (size_t)sessioncap, pid, start,
                       (size_t)startcap);
        if (e == 0 && fo_gremlin_process_matches(*pid, start)) {
            *owner = 0;
            close(fd);
            return 0;
        }
        struct timespec pause = {0, 10000000};
        nanosleep(&pause, NULL);
        if (attempt == 99) {
            close(fd);
            return EAGAIN;
        }
    }
    if (!locked) {
        close(fd);
        return EAGAIN;
    }
    e = read_owner(dir, prior_id, sizeof(prior_id), &prior_pid,
                   prior_start, sizeof(prior_start));
    if (e == 0) {
        if (fo_gremlin_process_matches(prior_pid, prior_start)) {
            e = EBUSY;
            goto fail;
        }
        e = fo_c_recover_async_scope(dir, prior_pid, prior_start);
        if (e != 0) goto fail;
    } else if (e != ENOENT) {
        goto fail;
    }
    e = cleanup_view_scratch(dir);
    if (e != 0) goto fail;
    e = read_recovery_source(dir, recovered, (size_t)recoveredcap);
    if (e != 0 && e != ENOENT) goto fail;
    if (e == ENOENT) {
        recovered[0] = 0;
        if (prior_pid > 0) {
            if (strlen(prior_id) + 1 > (size_t)recoveredcap) {
                e = ENAMETOOLONG;
                goto fail;
            }
            char record[129];
            int n = snprintf(record, sizeof(record), "%s\n", prior_id);
            if (n < 0 || n >= (int)sizeof(record)) { e = EOVERFLOW; goto fail; }
            /* Keep this pointer across replacement-owner and import crashes.
               The owner clears it only after the destination journal commits. */
            e = atomic_write_file(dir, "recovery.source", record, (size_t)n);
            if (e) goto fail;
            strcpy(recovered, prior_id);
        }
    }
    char sid[128], stamp[64];
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) { e = errno; goto fail; }
    e = fo_sys_process_start_text(getpid(), stamp, sizeof(stamp));
    if (e != 0) goto fail;
    snprintf(sid, sizeof(sid), "%ld-%lld-%09ld", (long)getpid(), (long long)ts.tv_sec, ts.tv_nsec);
    if (strlen(sid) + 1 > (size_t)sessioncap || strlen(stamp) + 1 > (size_t)startcap) { e = ENAMETOOLONG; goto fail; }
    char statuspath[PATH_MAX];
    if (snprintf(statuspath, sizeof(statuspath), "%s/status", dir) >=
        (int)sizeof(statuspath)) { e = ENAMETOOLONG; goto fail; }
    if (unlink(statuspath) != 0 && errno != ENOENT) { e = errno; goto fail; }
    clear_stop(dir);
    char metadata[512];
    int n = snprintf(metadata, sizeof(metadata), "%s\n%ld\n%s\n%s\n%s\n", sid,
                     (long)getpid(), stamp, canonical, lane);
    e = n < 0 || n >= (int)sizeof(metadata) ? EOVERFLOW :
        atomic_write_file(dir, "owner", metadata, (size_t)n);
    if (e) goto fail;
    strcpy(session, sid); strcpy(start, stamp); *pid = (int)getpid(); *owner = 1; *lockfd = fd;
    return 0;
fail:
    fo_sys_unlock(fd); return e;
}

int fo_gremlin_session_recovery_complete(const char *dir, const char *session,
                                        const char *recovered, int fd) {
    int e = verify_owner_fd(dir, fd);
    if (e) return e;
    char id[128], start[64], pending[128], current[64];
    int pid = 0;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e) return e;
    if (strcmp(id, session) != 0 || pid != (int)getpid()) return EPERM;
    e = fo_sys_process_start_text(getpid(), current, sizeof(current));
    if (e || strcmp(current, start) != 0) return EPERM;
    e = read_recovery_source(dir, pending, sizeof(pending));
    if (e == ENOENT) return 0;
    if (e) return e;
    if (strcmp(pending, recovered) != 0) return EPERM;
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/recovery.source", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    if (unlink(path) != 0) return errno;
    return fo_sys_sync_dir(dir);
}

int fo_gremlin_session_release(const char *dir, const char *session, int fd) {
    int e = verify_owner_fd(dir, fd);
    if (e != 0) return e;
    char id[128], start[64]; int pid = 0;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e) return e;
    if (strcmp(id, session) != 0 || pid != (int)getpid()) return EPERM;
    char current[64];
    e = fo_sys_process_start_text(getpid(), current, sizeof(current));
    if (e != 0 || strcmp(current, start) != 0) return EPERM;
    char path[PATH_MAX], statuspath[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/owner", dir) >= (int)sizeof(path) ||
        snprintf(statuspath, sizeof(statuspath), "%s/status", dir) >=
        (int)sizeof(statuspath)) return ENAMETOOLONG;
    clear_stop(dir);
    if (unlink(statuspath) != 0 && errno != ENOENT) return errno;
    if (unlink(path) != 0) return errno;
    (void)fo_sys_sync_dir(dir);
    if (fd >= 0) { fo_sys_unlock(fd); }
    return 0;
}

/* Stop retires inactive views only; validated scratch marks an active view. */
int fo_gremlin_session_retire_views(const char *dir, const char *session, int fd) {
    char id[128], start[64], current[64], path[PATH_MAX], views[PATH_MAX];
    int pid = 0, e;
    struct stat st;
    DIR *directory;
    struct dirent *entry;

    e = verify_owner_fd(dir, fd);
    if (e != 0) return e;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e != 0) return e;
    if (strcmp(id, session) != 0 || pid != (int)getpid()) return EPERM;
    e = fo_sys_process_start_text(getpid(), current, sizeof(current));
    if (e != 0 || strcmp(current, start) != 0) return EPERM;
    if (lstat(dir, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode) || !fo_private_path(dir)) return EPERM;
    if (snprintf(views, sizeof(views), "%s/views", dir) >= (int)sizeof(views))
        return ENAMETOOLONG;
    if (lstat(views, &st) != 0) return errno == ENOENT ? 0 : errno;
    if (!S_ISDIR(st.st_mode) || !fo_private_path(views)) return EPERM;
    directory = opendir(views);
    if (directory == NULL) return errno;
    for (;;) {
        errno = 0;
        entry = readdir(directory);
        if (entry == NULL) {
            e = errno;
            break;
        }
        if (strncmp(entry->d_name, "execution-", 10) != 0) continue;
        if (snprintf(path, sizeof(path), "%s/%s", views, entry->d_name) >=
            (int)sizeof(path)) {
            e = ENAMETOOLONG;
            break;
        }
        if (lstat(path, &st) != 0) {
            e = errno;
            break;
        }
        if (!S_ISDIR(st.st_mode) || !fo_private_path(path)) {
            e = EPERM;
            break;
        }
        e = fo_c_execution_scratch_active(path);
        if (e < 0) { e = -e; break; }
        if (e > 0) continue;
        e = fo_c_execution_scratch_release(path);
        if (e != 0) break;
        if (fo_c_rm_rf(path) != 0) {
            e = errno ? errno : EIO;
            break;
        }
    }
    if (closedir(directory) != 0 && e == 0) e = errno;
    if (e == 0) {
        int view_fd = open(views, O_RDONLY | O_CLOEXEC);
        if (view_fd < 0) return errno;
        if (fsync(view_fd) != 0) e = errno;
        close(view_fd);
    }
    return e;
}

int fo_gremlin_session_read(const char *project, const char *lane,
        char *dir, int dircap, char *session, int sessioncap,
        int *pid, char *start, int startcap, char *status, int statuscap) {
    char canonical[PATH_MAX], owner[128], ownerstart[64];
    int e = state_path(project, lane, dir, (size_t)dircap, canonical,
                       sizeof(canonical), 0);
    if (e) return e;
    e = read_owner(dir, owner, sizeof(owner), pid, ownerstart, sizeof(ownerstart));
    if (e) return e;
    if (!fo_gremlin_process_matches(*pid, ownerstart)) return ESRCH;
    if (strlen(owner) + 1 > (size_t)sessioncap || strlen(ownerstart) + 1 > (size_t)startcap) return ENAMETOOLONG;
    strcpy(session, owner); strcpy(start, ownerstart);
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/status", dir) >= (int)sizeof(path)) return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) { if (errno == ENOENT) { status[0] = 0; return 0; } return errno; }
    ssize_t n = read(fd, status, (size_t)statuscap - 1); close(fd);
    if (n < 0) return errno;
    status[n] = 0;
    return 0;
}

int fo_gremlin_session_publish(const char *dir, const char *session,
        const char *text, int textlen, int fd) {
    int e = verify_owner_fd(dir, fd);
    if (e != 0) return e;
    char id[128], start[64]; int pid = 0;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e) return e;
    if (strcmp(id, session) != 0 || pid != (int)getpid()) return EPERM;
    char current[64];
    e = fo_sys_process_start_text(getpid(), current, sizeof(current));
    if (e != 0 || strcmp(current, start) != 0) return EPERM;
    return atomic_write_file(dir, "status", text, (size_t)textlen);
}

int fo_gremlin_session_request_stop(const char *project, const char *lane,
        const char *session) {
    char dir[PATH_MAX], canonical[PATH_MAX], id[128], start[64];
    int pid = 0;
    int e = state_path(project, lane, dir, sizeof(dir), canonical,
                       sizeof(canonical), 0);
    if (e) return e;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e) return e;
    if (strcmp(id, session) != 0) return EPERM;
    if (!fo_gremlin_process_matches(pid, start)) return ESRCH;
    return atomic_write_file(dir, "stop.request", session, strlen(session));
}

int fo_gremlin_session_stop_requested(const char *dir, const char *session) {
    char path[PATH_MAX], value[128];
    struct stat gone;
    /* A lane whose state directory vanished has nothing left to own: its
       owner is asked to stop exactly as if a stop request had been written. */
    if (stat(dir, &gone) != 0 && errno == ENOENT) return 1;
    /* A task-owned lane stops when the client that created it is gone. */
    if (task_path(dir, path, sizeof(path)) == 0) {
        int tfd = open(path, O_RDONLY | O_CLOEXEC);
        if (tfd >= 0) {
            char text[128];
            ssize_t m = read(tfd, text, sizeof(text) - 1);
            close(tfd);
            int pid = 0;
            char start[64];
            if (m > 0) {
                text[m] = 0;
                if (sscanf(text, "%d %63s", &pid, start) == 2 && pid > 0 &&
                    !fo_gremlin_process_matches(pid, start)) return 1;
            }
        }
    }
    int e = stop_path(dir, path, sizeof(path));
    if (e) return -e;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno == ENOENT ? 0 : -errno;
    ssize_t n = read(fd, value, sizeof(value) - 1);
    int saved = errno; close(fd); errno = saved;
    if (n < 0) return -errno;
    value[n] = 0;
    return strcmp(value, session) == 0 ? 1 : 0;
}

/* Bind the live owner's lane to the client process that created it; the
   owner then stops once that process identity is gone. */
int fo_gremlin_session_bind_task(const char *dir, int pid, const char *start) {
    char text[128];
    int n = snprintf(text, sizeof(text), "%d %s\n", pid, start);
    if (pid <= 0 || n < 0 || n >= (int)sizeof(text)) return EINVAL;
    return atomic_write_file(dir, "task.owner", text, (size_t)n);
}

/* Identity (pid and start stamp) of this process, or of its parent. */
int fo_gremlin_client_identity(int parent, int *pid, char *start, int cap) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (parent) return ENOTSUP;
    *pid = (int)GetCurrentProcessId();
#else
    *pid = parent ? (int)getppid() : (int)getpid();
#endif
    if (*pid <= 1) return ESRCH;
    return fo_sys_process_start_text((pid_t)*pid, start, (size_t)cap);
}

int fo_gremlin_process_matches(int pid, const char *start) {
    char have[64];
    if (pid <= 0 || !start || !*start) return 0;
#if !defined(_WIN32) || defined(__CYGWIN__)
    if (kill((pid_t)pid, 0) != 0 && errno != EPERM) return 0;
    /* An exited process an unreaping subreaper still holds is not running. */
    int alive = 1;
    if (fo_sys_process_info((pid_t)pid, NULL, NULL, NULL, NULL, &alive) == 0 &&
        !alive) return 0;
#endif
    if (fo_sys_process_start_text((pid_t)pid, have, sizeof(have)) != 0) return 0;
    return strcmp(have, start) == 0;
}

static int lease_dir(const char *kind, char *dir, size_t cap) {
    const char *base = getenv("FO_GREMLIN_STATE_DIR");
    if (!base || !*base) base = getenv("XDG_CACHE_HOME");
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (!base || !*base) base = getenv("LOCALAPPDATA");
#endif
    char fallback[PATH_MAX];
    if (!base || !*base) {
        const char *home = getenv("HOME"); if (!home || !*home) return ENOENT;
        if (snprintf(fallback, sizeof(fallback), "%s/.cache", home) >= (int)sizeof(fallback)) return ENAMETOOLONG;
        base = fallback;
    }
    uint64_t h = fo_fnv1a_text(FO_FNV1A_SEED, kind);
    if (snprintf(dir, cap, "%s/fo/gremlin/leases/%016llx", base, (unsigned long long)h) >= (int)cap) return ENAMETOOLONG;
    return fo_sys_mkdirs(dir, 0700);
}

static int lease_acquire_in_dir_weighted(const char *dir, int capacity, int weight,
                                         int *fds, int *slots) {
    if (capacity < 1 || capacity > 1024 || weight < 1 || weight > capacity)
        return EINVAL;
    for (int i = 0; i < weight; ++i) { fds[i] = -1; slots[i] = -1; }
    int acquired = 0;
    char config[PATH_MAX];
    int e;
    if (snprintf(config, sizeof(config), "%s/config.lock", dir) >= (int)sizeof(config)) return ENAMETOOLONG;
    int cfd = open(config, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (cfd < 0) return errno;
    if (flock(cfd, LOCK_EX) != 0) { e = errno; close(cfd); return e; }
    char capfile[PATH_MAX], captext[32] = "";
    snprintf(capfile, sizeof(capfile), "%s/capacity", dir);
    int rfd = open(capfile, O_RDONLY | O_CLOEXEC);
    if (rfd >= 0) { (void)read(rfd, captext, sizeof(captext)-1); close(rfd); }
    int oldcap = atoi(captext), active = 0;
    if (oldcap < 0 || oldcap > 1024) { e = EINVAL; goto done; }
    for (int i = 0; i < (oldcap > capacity ? oldcap : capacity); ++i) {
        char path[PATH_MAX]; snprintf(path, sizeof(path), "%s/slot-%04d.lock", dir, i);
        int sfd = open(path, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
        if (sfd < 0) { e = errno; goto undo; }
        if (flock(sfd, LOCK_EX | LOCK_NB) == 0) {
            if (i < capacity && acquired < weight) {
                fds[acquired] = sfd;
                slots[acquired] = i;
                acquired++;
            }
            else { fo_sys_unlock(sfd); }
        } else if (errno == EWOULDBLOCK || errno == EAGAIN) {
            active++;
            close(sfd);
        }
        else { e = errno; close(sfd); goto undo; }
    }
    if (oldcap != 0 && oldcap != capacity && active > 0) { e = EBUSY; goto no_slot; }
    if (acquired < weight) { e = EAGAIN; goto no_slot; }
    char value[32]; int n = snprintf(value, sizeof(value), "%d\n", capacity);
    e = atomic_write_file(dir, "capacity", value, (size_t)n);
    if (e) goto undo;
    goto done;
no_slot:
    for (int i = 0; i < acquired; ++i) {
        flock(fds[i], LOCK_UN);
        close(fds[i]);
        fds[i] = -1;
        slots[i] = -1;
    }
    goto done;
undo:
    for (int i = 0; i < acquired; ++i) {
        flock(fds[i], LOCK_UN);
        close(fds[i]);
        fds[i] = -1;
        slots[i] = -1;
    }
done:
    fo_sys_unlock(cfd); return e;
}

static int lease_acquire_in_dir(const char *dir, int capacity, int *fdout, int *slot) {
    return lease_acquire_in_dir_weighted(dir, capacity, 1, fdout, slot);
}

int fo_gremlin_lease_acquire(const char *kind, int capacity, int *fdout, int *slot) {
    if (capacity < 1 || capacity > 1024) return EINVAL;
    *fdout = -1;
    *slot = -1;
    char dir[PATH_MAX];
    int e = lease_dir(kind, dir, sizeof(dir));
    if (e) return e;
    return lease_acquire_in_dir(dir, capacity, fdout, slot);
}

static int host_lease_base(char *base, size_t cap) {
    struct stat st;
    /* An explicit admission namespace confines lanes to one private domain,
       e.g. one test fixture, instead of every lane of this user on the host. */
    const char *domain = getenv("FO_GREMLIN_ADMISSION_DIR");
    if (domain && *domain) {
        if (!fo_path_is_absolute(domain)) return EINVAL;
        if (snprintf(base, cap, "%s", domain) >= (int)cap) return ENAMETOOLONG;
        int error = fo_sys_mkdirs(base, 0700);
        if (error) return error;
    } else {
#if defined(_WIN32) && !defined(__CYGWIN__)
        const char *local = getenv("LOCALAPPDATA");
        if (!local || !*local) return ENOENT;
        int n = snprintf(base, cap, "%s/fo/gremlin-host-admission", local);
        if (n < 0 || n >= (int)cap) return ENAMETOOLONG;
        int error = fo_sys_mkdirs(base, 0700);
        if (error) return error;
#else
        int n = snprintf(base, cap, "/var/tmp/fo-gremlin-admission-%lu",
                         (unsigned long)geteuid());
        if (n < 0 || n >= (int)cap) return ENAMETOOLONG;
        if (mkdir(base, 0700) == 0) {
            if (chmod(base, 0700) != 0) return errno;
        } else if (errno != EEXIST) {
            return errno;
        }
#endif
    }
    if (lstat(base, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode) || !fo_private_path(base)
#if !defined(_WIN32) || defined(__CYGWIN__)
        || (st.st_mode & 0777) != 0700
#endif
        ) return EPERM;
    return 0;
}

/* Ephemeral admission lives outside source/action identity. Each held group
   reserves an upstream budget and offers weighted child slots under a fresh
   incarnation. Descendants cannot spend sibling or retired scope capacity. */
struct admission_owner {
    int pid, weight;
    unsigned long long kind;
    char start[64], state[PATH_MAX];
};
int fo_c_process_owned_by_scope(const char *, int, const char *);

static int admission_gate(const char *scope, int create) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/gate", scope) >= (int)sizeof(path)) {
        errno = ENAMETOOLONG; return -1;
    }
    int fd = open(path, O_RDWR | O_CLOEXEC | (create ? O_CREAT : 0), 0600);
    if (fd < 0) return -1;
    if (flock(fd, LOCK_EX) != 0) { int e = errno; close(fd); errno = e; return -1; }
    return fd;
}

static int admission_read(int fd, struct admission_owner *owner) {
    char record[PATH_MAX + 160], *state;
    ssize_t n = pread(fd, record, sizeof(record)-1, 0);
    if (n <= 0 || n >= (ssize_t)sizeof(record)-1) return EINVAL;
    record[n] = '\0';
    if (sscanf(record, "%d %63s %d %llx", &owner->pid, owner->start,
               &owner->weight, &owner->kind) != 4 || owner->pid <= 0 ||
        owner->weight < 1 || owner->weight > 1024) return EINVAL;
    state = strchr(record, '\n');
    if (!state) return EINVAL;
    ++state;
    n = strcspn(state, "\n");
    if (n <= 0 || n >= PATH_MAX || state[n] != '\n' || state[n+1] != '\0')
        return EINVAL;
    memcpy(owner->state, state, (size_t)n); owner->state[n] = '\0';
    return 0;
}

#if defined(_WIN32) && !defined(__CYGWIN__)
#include "fo_admission_windows.h"
#endif

static int admission_children_busy(const char *scope, int capacity) {
    char path[PATH_MAX];
    for (int i = 0; i < capacity; ++i) {
        if (snprintf(path, sizeof(path), "%s/children/slot-%04d.lock", scope, i)
            >= (int)sizeof(path)) return ENAMETOOLONG;
        int fd = open(path, O_RDWR | O_CLOEXEC);
        if (fd < 0) { if (errno == ENOENT) continue; return errno; }
        int e = flock(fd, LOCK_EX | LOCK_NB) == 0 ? 0 : errno;
        if (e == 0) flock(fd, LOCK_UN);
        close(fd);
        if (e) return e;
    }
    return 0;
}

/* A guardian retains the original flock descriptions, not newly acquired
   pathname locks. Wrappers may close inherited FDs; owner death cannot release
   capacity while admitted descendants still hold their family reservations. */
#if !defined(_WIN32) || defined(__CYGWIN__)
static int admission_guard_path(int pid, const char *start, char *path,
                                size_t cap) {
    char base[PATH_MAX], directory[PATH_MAX];
    int e = host_lease_base(base, sizeof(base));
    if (e) return e;
    if (snprintf(directory, sizeof(directory), "%s/guardians", base)
        >= (int)sizeof(directory)) return ENAMETOOLONG;
    e = fo_sys_mkdirs(directory, 0700);
    if (e) return e;
    if (snprintf(path, cap, "%s/%d-%s", directory, pid, start) >= (int)cap)
        return ENAMETOOLONG;
    return 0;
}
#endif

int fo_gremlin_admission_is_guardian(int pid, const char *start) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    (void)pid; (void)start;
    return 0; /* Native reservation records replace the Unix admission guardian. */
#else
    char path[PATH_MAX];
    struct stat st;
    if (admission_guard_path(pid, start, path, sizeof(path)) != 0) return 0;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return 0;
    int active = flock(fd, LOCK_EX | LOCK_NB) != 0 &&
                 (errno == EAGAIN || errno == EWOULDBLOCK);
    active = active && fstat(fd, &st) == 0 && S_ISREG(st.st_mode) &&
             fo_private_fd(fd) &&
             fo_gremlin_process_matches(pid, start);
    close(fd);
    return active;
#endif
}

#if !defined(_WIN32) || defined(__CYGWIN__)
static int admission_guard_close_fds(const int *fds, int weight, int ack, int marker) {
#ifdef __linux__
    DIR *directory = opendir("/proc/self/fd");
#else
    DIR *directory = opendir("/dev/fd");
#endif
    if (!directory) return errno;
    struct dirent *entry;
    while ((entry = readdir(directory)) != NULL) {
        char *end;
        long value = strtol(entry->d_name, &end, 10);
        if (*end || value < 0 || value > INT_MAX) continue;
        int keep = value == dirfd(directory) || value == ack || value == marker;
        for (int i = 0; i < weight; ++i) if (value == fds[i]) keep = 1;
        if (!keep) close((int)value);
    }
    closedir(directory);
    return 0;
}

#endif
static int admission_guard_start(const char *scope, const int *fds, int weight,
                                  int *guardian) {
#if defined(_WIN32) && !defined(__CYGWIN__)
    (void)scope; (void)fds; (void)weight;
    *guardian = 0;
    return 0; /* Exact job drainage under the machine gate precedes every reuse. */
#else
    int handshake[2];
    char owner_start[64], authority[PATH_MAX], state[PATH_MAX];
    pid_t owner = getpid();
    int e = fo_sys_process_start_text(owner, owner_start, sizeof(owner_start));
    if (e) return e;
    const char *state_hint = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
    const char *pid_hint = getenv("FO_GREMLIN_PROCESS_SCOPE_PID");
    const char *start_hint = getenv("FO_GREMLIN_PROCESS_SCOPE_START");
    state[0] = '\0';
    if (state_hint && pid_hint && start_hint && atoi(pid_hint) == (int)owner &&
        strcmp(start_hint, owner_start) == 0 && strlen(state_hint) < sizeof(state))
        strcpy(state, state_hint);
    if (snprintf(authority, sizeof(authority), "%s/authority", scope)
        >= (int)sizeof(authority)) return ENAMETOOLONG;
    if (pipe(handshake) != 0) return errno;
    pid_t pid = fork();
    if (pid < 0) { e = errno; close(handshake[0]); close(handshake[1]); return e; }
    if (pid == 0) {
        /* Ordinary owner cancellation must drain payloads before dropping their
           reservation. The guardian is capacity bookkeeping, not payload. */
        signal(SIGTERM, SIG_IGN); signal(SIGINT, SIG_IGN); signal(SIGHUP, SIG_IGN);
        signal(SIGPIPE, SIG_IGN);
        int detached = setsid() >= 0;
        int detach_error = detached ? 0 : errno;
        char stamp[64], marker_path[PATH_MAX];
        int marker = -1;
        e = detach_error ? detach_error : fo_sys_process_start_text(getpid(), stamp, sizeof(stamp));
        if (e == 0) e = admission_guard_path((int)getpid(), stamp, marker_path,
                                           sizeof(marker_path));
        if (e == 0) {
            marker = open(marker_path, O_CREAT | O_EXCL | O_RDWR | O_CLOEXEC, 0600);
            if (marker < 0) e = errno;
            else if (flock(marker, LOCK_EX | LOCK_NB) != 0) e = errno;
        }
        if (e == 0) e = admission_guard_close_fds(fds, weight, handshake[1], marker);
        (void)write(handshake[1], &e, sizeof(e)); close(handshake[1]);
        if (e) {
            if (marker >= 0) { close(marker); unlink(marker_path); }
            _exit(1);
        }
        struct timespec pause = {0, 20000000};
        for (;;) {
            int alive = fo_gremlin_process_matches((int)owner, owner_start);
            struct stat st;
            int observed = lstat(authority, &st);
            int observation_error = observed < 0 ? errno : 0;
            if (observation_error && observation_error != ENOENT) {
                nanosleep(&pause, NULL);
                continue;
            }
            int active = alive && observed == 0;
            /* A deleted lane state directory holds no payload to drain. */
            struct stat state_stat;
            int state_gone = *state && lstat(state, &state_stat) != 0 &&
                             errno == ENOENT;
            int payload_drained = alive || !*state || state_gone ||
                fo_c_recover_async_scope(state, (int)owner, owner_start) == 0;
            if (!active && payload_drained) {
                int gate = -1;
                e = 0;
                /* A child may have validated the owner before its death but
                   not yet locked its slot. Revoke under the same gate before
                   checking drainage, keeping upstream capacity throughout. */
                if (!alive) {
                    /* A scope already collected needs no revocation. */
                    gate = admission_gate(scope, 0);
                    if (gate < 0) e = errno == ENOENT ? 0 : errno;
                    else if (unlink(authority) != 0 && errno != ENOENT) e = errno;
                }
                if (!e) e = admission_children_busy(scope, weight);
                if (gate >= 0) { fo_sys_unlock(gate); }
                if (!e) break;
            }
            nanosleep(&pause, NULL);
        }
        /* Closing copies preserves any other live holder's flock. */
        for (int i = 0; i < weight; ++i) close(fds[i]);
        unlink(marker_path); close(marker);
        if (!fo_gremlin_process_matches((int)owner, owner_start))
            (void)fo_c_rm_rf(scope);
        _exit(0);
    }
    close(handshake[1]);
    ssize_t n;
    do { n = read(handshake[0], &e, sizeof(e)); } while (n < 0 && errno == EINTR);
    close(handshake[0]);
    if (n != sizeof(e)) e = EIO;
    if (e) { (void)waitpid(pid, NULL, 0); return e; }
    *guardian = (int)pid;
    return 0;
#endif
}

int fo_gremlin_host_lease_close(int fd) {
    if (fd < 0) return EINVAL;
    return close(fd) == 0 ? 0 : errno;
}

static int admission_parent(const char *scope, uint64_t kind,
                             struct admission_owner *owner) {
    char path[PATH_MAX];
    struct stat st;
    if (snprintf(path, sizeof(path), "%s/authority", scope) >= (int)sizeof(path))
        return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    char retired[PATH_MAX];
    if (snprintf(retired, sizeof(retired), "%s/retired", scope) >= (int)sizeof(retired))
        return 0;
    if (lstat(retired, &st) == 0 || errno != ENOENT) return 0;
#endif
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return 0;
    int active = flock(fd, LOCK_EX | LOCK_NB) != 0 &&
                 (errno == EAGAIN || errno == EWOULDBLOCK);
    int valid = active && fstat(fd, &st) == 0 && S_ISREG(st.st_mode) &&
                fo_private_fd(fd) &&
                admission_read(fd, owner) == 0 && owner->kind == kind &&
                fo_c_process_owned_by_scope(owner->state, owner->pid, owner->start);
    close(fd);
    return valid;
}

static int admission_publish(const char *base, uint64_t kind, int weight,
                              char *scope, int cap, int *authority,
                              const char *parent_scope,
                              const struct admission_owner *parent) {
    char directory[PATH_MAX], path[PATH_MAX], start[64], record[PATH_MAX+160];
    const char *state = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
    int n = snprintf(directory, sizeof(directory), "%s/scopes", base);
    if (n < 0 || n >= (int)sizeof(directory)) return ENAMETOOLONG;
    int e = fo_sys_mkdirs(directory, 0700);
    if (e) return e;
    n = snprintf(scope, (size_t)cap, "%s/%ld-XXXXXX", directory, (long)getpid());
    if (n < 0 || n >= cap) return ENAMETOOLONG;
    if (!mkdtemp(scope)) return errno;
    int gate = admission_gate(scope, 1);
    if (gate < 0) { e = errno; goto failed; }
    close(gate);
#if defined(_WIN32) && !defined(__CYGWIN__)
    /* Persist verified outer membership before arming the nested guardian. */
    if (parent) {
        e = admission_write_ancestor(scope, parent_scope, parent);
        if (e) goto failed;
    }
#else
    (void)parent_scope; (void)parent;
#endif
    e = fo_sys_process_start_text(getpid(), start, sizeof(start));
    if (e) goto failed;
    n = snprintf(path, sizeof(path), "%s/authority", scope);
    if (n < 0 || n >= (int)sizeof(path)) { e = ENAMETOOLONG; goto failed; }
    *authority = open(path, O_CREAT | O_EXCL | O_RDWR | O_CLOEXEC, 0600);
    if (*authority < 0) { e = errno; goto failed; }
    if (flock(*authority, LOCK_EX | LOCK_NB) != 0) { e = errno; goto failed; }
#if defined(_WIN32) && !defined(__CYGWIN__)
    e = admission_bind_native_scope(scope, *authority);
    if (e) goto failed;
    state = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
    if (!state || !*state) { e = EIO; goto failed; }
#endif
    n = snprintf(record, sizeof(record), "%ld %s %d %llx\n%s\n",
                 (long)getpid(), start, weight, (unsigned long long)kind,
                 state && *state ? state : "-");
    if (n < 0 || n >= (int)sizeof(record)) { e = ENAMETOOLONG; goto failed; }
    e = fo_sys_write_all(*authority, record, (size_t)n);
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (!e && fsync(*authority) != 0) e = errno;
    if (!e) e = fo_sys_sync_dir(scope);
#endif
    if (e == 0 && setenv("FO_GREMLIN_ADMISSION_SCOPE", scope, 1) != 0) e = errno;
    if (e == 0) return 0;
failed:
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (*authority >= 0) (void)admission_unbind_native_scope(*authority);
#endif
    if (*authority >= 0) { close(*authority); *authority = -1; }
    fo_c_rm_rf(scope); scope[0] = '\0'; return e;
}

int fo_gremlin_host_scope_retire(int *authority, int *guardian, const char *scope,
                                 const char *previous) {
    if (*authority < 0) return 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    HANDLE machine_gate = admission_machine_gate();
    if (!machine_gate) return errno;
    char base[PATH_MAX];
    int recovered = host_lease_base(base, sizeof(base));
    if (!recovered) recovered = admission_recover_records(base);
    if (recovered) { admission_machine_unlock(machine_gate); return recovered; }
#endif
    int gate = admission_gate(scope, 0);
    if (gate < 0) {
#if defined(_WIN32) && !defined(__CYGWIN__)
        admission_machine_unlock(machine_gate);
#endif
        return errno;
    }
    struct admission_owner owner;
    int e = admission_read(*authority, &owner);
    char path[PATH_MAX];
    if (e == 0 && snprintf(path, sizeof(path), "%s/authority", scope)
        >= (int)sizeof(path)) e = ENAMETOOLONG;
    /* Revocation and child admission share this gate. Existing children retain
       their upstream reservation; no new sibling can enter a closing scope. */
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (e == 0) e = atomic_write_file(scope, "retired", "1\n", 2);
#else
    if (e == 0 && unlink(path) != 0 && errno != ENOENT) e = errno;
#endif
    if (e == 0) e = admission_children_busy(scope, owner.weight);
#if defined(_WIN32) && !defined(__CYGWIN__)
    if (e == 0) e = admission_unbind_native_scope(*authority);
    if (e == 0) e = admission_recover_records(base);
    *guardian = 0;
#else
    if (e == 0 && *guardian > 0) {
        pid_t done;
        do { done = waitpid((pid_t)*guardian, NULL, 0); }
        while (done < 0 && errno == EINTR);
        if (done < 0 && errno != ECHILD) e = errno;
        else *guardian = 0;
    }
#endif
    if (e == 0) {
        const char *current = getenv("FO_GREMLIN_ADMISSION_SCOPE");
        if (current && strcmp(current, scope) == 0) {
            if (previous && *previous) {
                if (setenv("FO_GREMLIN_ADMISSION_SCOPE", previous, 1) != 0) e = errno;
            } else if (unsetenv("FO_GREMLIN_ADMISSION_SCOPE") != 0) e = errno;
        }
    }
    if (e == 0) {
        close(*authority); *authority = -1;
        if (fo_c_rm_rf(scope) != 0) e = errno ? errno : EIO;
    }
    fo_sys_unlock(gate);
#if defined(_WIN32) && !defined(__CYGWIN__)
    admission_machine_unlock(machine_gate);
#endif
    return e;
}

#if !defined(_WIN32) || defined(__CYGWIN__)
/* Remove admission entries whose owner is provably gone: a guardian marker
   and a scope authority are held under flock while their owner lives, so an
   entry is stale only when its recorded process identity no longer matches
   and its lock can be taken (scopes also need drained children). Anything
   unreadable, foreign or contended is left alone. */
static void admission_collect_garbage(const char *base) {
    char directory[PATH_MAX], path[PATH_MAX], name[PATH_MAX];
    struct dirent *entry;
    struct stat st;
    DIR *dir;

    if (snprintf(directory, sizeof(directory), "%s/guardians", base)
        < (int)sizeof(directory) && (dir = opendir(directory)) != NULL) {
        while ((entry = readdir(dir)) != NULL) {
            int pid;
            char start[64];
            if (sscanf(entry->d_name, "%d-%63s", &pid, start) != 2 || pid <= 0)
                continue;
            if (snprintf(name, sizeof(name), "%d-%s", pid, start) >= (int)sizeof(name) ||
                strcmp(name, entry->d_name) != 0 ||
                fo_gremlin_process_matches(pid, start) ||
                snprintf(path, sizeof(path), "%s/%s", directory, name) >= (int)sizeof(path))
                continue;
            int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
            if (fd < 0) continue;
            if (fstat(fd, &st) == 0 && S_ISREG(st.st_mode) && st.st_uid == geteuid() &&
                flock(fd, LOCK_EX | LOCK_NB) == 0)
                unlink(path);
            close(fd);
        }
        closedir(dir);
    }
    if (snprintf(directory, sizeof(directory), "%s/scopes", base)
        >= (int)sizeof(directory) || (dir = opendir(directory)) == NULL) return;
    while ((entry = readdir(dir)) != NULL) {
        struct admission_owner owner;
        if (entry->d_name[0] == '.' ||
            snprintf(path, sizeof(path), "%s/%s", directory, entry->d_name)
                >= (int)sizeof(path) ||
            lstat(path, &st) != 0 || !S_ISDIR(st.st_mode) || st.st_uid != geteuid())
            continue;
        if (snprintf(name, sizeof(name), "%s/authority", path) >= (int)sizeof(name))
            continue;
        int fd = open(name, O_RDWR | O_CLOEXEC | O_NOFOLLOW);
        if (fd < 0) continue;
        if (admission_read(fd, &owner) == 0 &&
            !fo_gremlin_process_matches(owner.pid, owner.start) &&
            flock(fd, LOCK_EX | LOCK_NB) == 0 &&
            admission_children_busy(path, owner.weight) == 0)
            (void)fo_c_rm_rf(path);
        close(fd);
    }
    closedir(dir);
}
#endif

int fo_gremlin_host_lease_acquire_weighted(const char *kind, int capacity,
                                           int weight, int *fds, int *slots,
                                           int *authority, int *guardian, char *scope,
                                           char *previous, int text_capacity) {
    char base[PATH_MAX], dir[PATH_MAX];
    const char *hint = getenv("FO_GREMLIN_ADMISSION_SCOPE");
    struct admission_owner parent;
    int e = host_lease_base(base, sizeof(base)), gate = -1, delegated = 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
    HANDLE machine_gate = NULL;
#endif
    *authority = -1; *guardian = 0; scope[0] = '\0'; previous[0] = '\0';
    if (e) return e;
#if !defined(_WIN32) || defined(__CYGWIN__)
    admission_collect_garbage(base);
#endif
#if defined(_WIN32) && !defined(__CYGWIN__)
    machine_gate = admission_machine_gate();
    if (!machine_gate) return errno;
    e = admission_recover_records(base);
    if (e) goto done;
#endif
    if (hint && *hint) {
        if (strlen(hint) >= (size_t)text_capacity) { e = ENAMETOOLONG; goto done; }
        strcpy(previous, hint);
    }
    uint64_t h = fo_fnv1a_text(FO_FNV1A_SEED, kind);
    if (hint && *hint) {
        gate = admission_gate(hint, 0);
        if (gate >= 0 && admission_parent(hint, h, &parent)) {
            if (weight > parent.weight) { e = E2BIG; goto done; }
            if (snprintf(dir, sizeof(dir), "%s/children", hint) >= (int)sizeof(dir)) {
                e = ENAMETOOLONG; goto done;
            }
            capacity = parent.weight;
            delegated = 1;
        }
    }
    if (!delegated && snprintf(dir, sizeof(dir), "%s/fo/gremlin/leases/%016llx",
                 base, (unsigned long long)h) >= (int)sizeof(dir)) {
        e = ENAMETOOLONG; goto done;
    }
    e = fo_sys_mkdirs(dir, 0700);
    if (e == 0) e = lease_acquire_in_dir_weighted(dir, capacity, weight, fds, slots);
    if (e == 0) {
        e = admission_publish(base, h, weight, scope, text_capacity, authority,
                              previous, delegated ? &parent : NULL);
        if (e == 0) e = admission_guard_start(scope, fds, weight, guardian);
        if (e != 0) {
            if (*authority >= 0) {
#if defined(_WIN32) && !defined(__CYGWIN__)
                (void)admission_unbind_native_scope(*authority);
#endif
                close(*authority); *authority = -1;
                (void)fo_c_rm_rf(scope); scope[0] = '\0';
                if (*previous) (void)setenv("FO_GREMLIN_ADMISSION_SCOPE", previous, 1);
                else (void)unsetenv("FO_GREMLIN_ADMISSION_SCOPE");
            }
            for (int i = 0; i < weight; ++i) {
                flock(fds[i], LOCK_UN); close(fds[i]); fds[i] = -1; slots[i] = -1;
            }
        }
    }
done:
    if (gate >= 0) { fo_sys_unlock(gate); }
#if defined(_WIN32) && !defined(__CYGWIN__)
    admission_machine_unlock(machine_gate);
#endif
    return e;
}

int fo_gremlin_lease_is_busy(int error) {
    return error == EAGAIN || error == EWOULDBLOCK;
}

int fo_gremlin_lease_release(int fd) {
    if (fd < 0) return EINVAL;
    int e = flock(fd, LOCK_UN) == 0 ? 0 : errno;
    if (close(fd) != 0 && e == 0) e = errno;
    return e;
}

static int valid_generation_id(const char *id) {
    size_t n = strlen(id);
    if (n == 0 || n > 120) return 0;
    for (size_t i = 0; i < n; ++i)
        if (!(id[i] == '-' || id[i] == '_' ||
              (id[i] >= '0' && id[i] <= '9') ||
              (id[i] >= 'a' && id[i] <= 'z') ||
              (id[i] >= 'A' && id[i] <= 'Z'))) return 0;
    return 1;
}

static int read_generation_root(const char *dir, char *root, size_t cap);

static int generation_state_path(const char *root, char *dir, size_t cap,
                                 char *canonical, size_t canonical_cap) {
    struct stat st;
    char resolved[PATH_MAX], parent[PATH_MAX], parent_real[PATH_MAX];
    const char *id, *slash;
    int e;
    if (!fo_path_is_absolute(root)) return EINVAL;
    slash = strrchr(root, '/');
    if (!slash || !valid_generation_id(slash + 1)) return EINVAL;
    if (realpath(root, resolved)) {
        if (lstat(resolved, &st) != 0) return errno;
        if (!S_ISDIR(st.st_mode) || (st.st_mode & 0222) != 0) return EPERM;
    } else {
        if (errno != ENOENT) return errno;
        if ((size_t)(slash - root) >= sizeof(parent)) return ENAMETOOLONG;
        memcpy(parent, root, (size_t)(slash - root));
        parent[slash - root] = '\0';
        if (!realpath(parent, parent_real)) return errno;
        if (snprintf(resolved, sizeof(resolved), "%s/%s", parent_real,
                     slash + 1) >= (int)sizeof(resolved)) return ENAMETOOLONG;
    }
    if (strlen(resolved) + 1 > canonical_cap) return ENAMETOOLONG;
    id = strrchr(resolved, '/');
    id = id ? id + 1 : resolved;
    if (!valid_generation_id(id)) return EINVAL;
    strcpy(canonical, resolved);
    slash = strrchr(resolved, '/');
    if (snprintf(dir, cap, "%.*s/.state/%s", (int)(slash - resolved),
                 resolved, id) >= (int)cap) return ENAMETOOLONG;
    e = fo_sys_mkdirs(dir, 0700);
    return e;
}

/* Return at most limit inactive candidates from this materialization's store.
   Each path occupies one PATH_MAX-sized slot in roots.  The pruning operation
   rechecks the guard, pin, and lease after this advisory scan. */
int fo_gremlin_generation_prune_candidates(const char *anchor, char *roots,
                                           int slot_size, int limit,
                                           int min_age_seconds) {
    char dir[PATH_MAX], canonical[PATH_MAX], base[PATH_MAX];
    int e = generation_state_path(anchor, dir, sizeof(dir), canonical,
                                  sizeof(canonical));
    if (e) return -e;
    if (!roots || slot_size < PATH_MAX || limit < 1 || limit > 32 ||
        min_age_seconds < 0) return -EINVAL;
    char *slash = strrchr(dir, '/');
    if (!slash) return -EINVAL;
    size_t nbase = (size_t)(slash - dir);
    memcpy(base, dir, nbase);
    base[nbase] = '\0';
    DIR *stream = opendir(base);
    if (!stream) return -errno;
    int count = 0, scan_error = 0;
    time_t now = time(NULL);
    struct dirent *entry;
    while (count < limit) {
        errno = 0;
        entry = readdir(stream);
        if (!entry) { scan_error = errno; break; }
        if (!valid_generation_id(entry->d_name)) continue;
        char sidecar[PATH_MAX], record[PATH_MAX], pinned[PATH_MAX];
        char lease[PATH_MAX], pending[PATH_MAX], stored[PATH_MAX + 1];
        struct stat st;
        if (snprintf(sidecar, sizeof(sidecar), "%s/%s", base, entry->d_name) >=
                (int)sizeof(sidecar) ||
            snprintf(record, sizeof(record), "%s/registered", sidecar) >=
                (int)sizeof(record) ||
            snprintf(pinned, sizeof(pinned), "%s/pinned", sidecar) >=
                (int)sizeof(pinned) ||
            snprintf(lease, sizeof(lease), "%s/lease.lock", sidecar) >=
                (int)sizeof(lease) ||
            snprintf(pending, sizeof(pending), "%s/pending-root-release", sidecar) >=
                (int)sizeof(pending)) continue;
        if (lstat(sidecar, &st) != 0 || !S_ISDIR(st.st_mode)) continue;
        int has_pending = lstat(pending, &st) == 0 && S_ISREG(st.st_mode);
        if (lstat(record, &st) == 0 && S_ISREG(st.st_mode)) {
            if (now < st.st_mtime || now - st.st_mtime < min_age_seconds) continue;
        } else if (!has_pending) continue;
        if (lstat(pinned, &st) == 0) continue;
        if (read_generation_root(sidecar, stored, sizeof(stored)) != 0) continue;
        char expected[PATH_MAX];
        if (snprintf(expected, sizeof(expected), "%.*s/%s",
                     (int)(nbase - strlen("/.state")), base, entry->d_name) >=
            (int)sizeof(expected)) continue;
        if (strcmp(stored, expected) != 0) continue;
        int fd = fo_sys_lock_file(lease, O_CLOEXEC, 0600, LOCK_EX | LOCK_NB);
        if (fd < 0) continue;
        fo_sys_unlock(fd);
        memcpy(roots + (size_t)count * (size_t)slot_size,
               stored, strlen(stored) + 1);
        ++count;
    }
    if (closedir(stream) != 0 && scan_error == 0) scan_error = errno;
    if (scan_error != 0) return -scan_error;
    return count;
}

static int read_generation_root(const char *dir, char *root, size_t cap) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/root", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    ssize_t n = read(fd, root, cap - 1);
    int e = n < 0 ? errno : 0;
    char extra;
    if (e == 0 && (size_t)n == cap - 1) {
        ssize_t more = read(fd, &extra, 1);
        if (more != 0) e = EOVERFLOW;
    }
    if (close(fd) != 0 && e == 0) e = errno;
    if (e) return e;
    if (n < 2 || root[n - 1] != '\n') return EINVAL;
    root[n - 1] = '\0';
    return 0;
}

static int generation_valid_at(const char *dir, const char *canonical) {
    char marker[PATH_MAX], stored[PATH_MAX + 1], resolved[PATH_MAX];
    struct stat st;
    if (read_generation_root(dir, stored, sizeof(stored)) != 0 ||
        strcmp(stored, canonical) != 0) return 0;
    if (!realpath(stored, resolved) || strcmp(resolved, canonical) != 0) return 0;
    if (lstat(resolved, &st) != 0 || !S_ISDIR(st.st_mode) ||
        (st.st_mode & 0222) != 0) return 0;
    if (snprintf(marker, sizeof(marker), "%s/registered", dir) >= (int)sizeof(marker))
        return 0;
    return lstat(marker, &st) == 0 && S_ISREG(st.st_mode);
}

static int lock_in(const char *dir, const char *name, int operation) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return -ENAMETOOLONG;
    return fo_sys_lock_file(path, O_CLOEXEC, 0600, operation);
}

static int rm_tree(const char *path) {
    return fo_c_rm_rf(path) == 0 ? 0 : (errno ? errno : EIO);
}

static int generation_register_path(const char *root, int *lease_fd) {
    char dir[PATH_MAX], canonical[PATH_MAX], record[PATH_MAX + 1];
    struct stat st;
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    int fd = lock_in(dir, "guard.lock", LOCK_EX);
    if (fd < 0) return -fd;
    if (lstat(canonical, &st) != 0) e = errno;
    else if (!S_ISDIR(st.st_mode) || (st.st_mode & 0222) != 0) e = EPERM;
    char old_root[PATH_MAX + 1];
    if (e == 0) {
        e = read_generation_root(dir, old_root, sizeof(old_root));
        if (e == 0 && strcmp(old_root, canonical) != 0) e = EEXIST;
        else if (e == ENOENT) {
            int n = snprintf(record, sizeof(record), "%s\n", canonical);
            if (n < 0 || n >= (int)sizeof(record)) e = ENAMETOOLONG;
            else e = atomic_write_file(dir, "root", record, (size_t)n);
        }
    }
    if (e == 0 && !generation_valid_at(dir, canonical))
        e = atomic_write_file(dir, "registered", "generation-v1\n", 14);
    if (e == 0) e = fo_sys_sync_dir(dir);
    if (lstat(canonical, &st) != 0 && e == 0) e = errno;
    if (e == 0) {
        int lfd = lock_in(dir, "lease.lock", LOCK_SH | LOCK_NB);
        if (lfd < 0) e = -lfd;
        else {
#if !defined(_WIN32) || defined(__CYGWIN__)
            (void)futimens(lfd, NULL); /* last use, for storage retention */
#endif
            *lease_fd = lfd;
        }
    }
    fo_sys_unlock(fd);
    return e;
}

static int generation_lease_path(const char *root, int *fdout) {
    char dir[PATH_MAX], canonical[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return ENOENT;
    int gfd = lock_in(dir, "guard.lock", LOCK_SH);
    if (gfd < 0) return -gfd;
    if (!generation_valid_at(dir, canonical)) {
        fo_sys_unlock(gfd);
        return ENOENT;
    }
    int lfd = lock_in(dir, "lease.lock", LOCK_SH | LOCK_NB);
    fo_sys_unlock(gfd);
    if (lfd < 0) return -lfd;
#if !defined(_WIN32) || defined(__CYGWIN__)
    (void)futimens(lfd, NULL); /* last use, for storage retention */
#endif
    *fdout = lfd;
    return 0;
}

static int generation_pin_path(const char *root, int pinned) {
    char dir[PATH_MAX], canonical[PATH_MAX], pin[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return ENOENT;
    if (snprintf(pin, sizeof(pin), "%s/pinned", dir) >= (int)sizeof(pin))
        return ENAMETOOLONG;
    int fd = lock_in(dir, "guard.lock", LOCK_EX);
    if (fd < 0) return -fd;
    if (!generation_valid_at(dir, canonical)) {
        fo_sys_unlock(fd);
        return ENOENT;
    }
    if (pinned) e = atomic_write_file(dir, "pinned", "pinned\n", 7);
    else if (unlink(pin) != 0 && errno != ENOENT) e = errno;
    else e = fo_sys_sync_dir(dir);
    fo_sys_unlock(fd);
    return e;
}

static int generation_prune_path(const char *root) {
    char dir[PATH_MAX], canonical[PATH_MAX], pin[PATH_MAX], registered[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return EPERM;
    if (snprintf(pin, sizeof(pin), "%s/pinned", dir) >= (int)sizeof(pin) ||
        snprintf(registered, sizeof(registered), "%s/registered", dir) >= (int)sizeof(registered))
        return ENAMETOOLONG;
    int gfd = lock_in(dir, "guard.lock", LOCK_EX | LOCK_NB);
    if (gfd < 0) return -gfd == EAGAIN || -gfd == EWOULDBLOCK ? EBUSY : -gfd;
    if (!generation_valid_at(dir, canonical)) {
        fo_sys_unlock(gfd);
        return ENOENT;
    }
    struct stat pin_stat;
    if (lstat(pin, &pin_stat) == 0) { fo_sys_unlock(gfd); return EBUSY; }
    if (errno != ENOENT) { e = errno; fo_sys_unlock(gfd); return e; }
    int lfd = lock_in(dir, "lease.lock", LOCK_EX | LOCK_NB);
    if (lfd < 0) {
        e = -lfd;
        fo_sys_unlock(gfd);
        return e == EAGAIN || e == EWOULDBLOCK ? EBUSY : e;
    }
    e = rm_tree(canonical);
    if (e == 0 && unlink(registered) != 0 && errno != ENOENT) e = errno;
    if (e == 0) e = fo_sys_sync_dir(dir);
    fo_sys_unlock(lfd);
    fo_sys_unlock(gfd);
    return e;
}

int fo_gremlin_generation_mark_release(const char *root, const char *store,
                                       const char *owner) {
    char dir[PATH_MAX], canonical[PATH_MAX], record[PATH_MAX + 128];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!fo_path_is_absolute(store) || !owner || strlen(owner) != 64) return EINVAL;
    for (const char *p = store; *p; ++p)
        if (*p == '\n' || *p == '\r') return EINVAL;
    for (const char *p = owner; *p; ++p)
        if (!((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f'))) return EINVAL;
    int fd = lock_in(dir, "guard.lock", LOCK_EX);
    if (fd < 0) return -fd;
    if (!generation_valid_at(dir, canonical)) e = ENOENT;
    if (e == 0) {
        int n = snprintf(record, sizeof(record), "%s\n%s\n", store, owner);
        if (n < 0 || n >= (int)sizeof(record)) e = ENAMETOOLONG;
        else e = atomic_write_file(dir, "pending-root-release", record, (size_t)n);
    }
    fo_sys_unlock(fd);
    return e;
}

int fo_gremlin_generation_pending_release(const char *root, char *store,
                                          int store_cap, char *owner, int owner_cap) {
    char dir[PATH_MAX], canonical[PATH_MAX], path[PATH_MAX];
    char record[PATH_MAX + 128];
    char *separator, *end;
    int e = generation_state_path(root, dir, sizeof(dir), canonical,
                                  sizeof(canonical));
    if (e) return e;
    if (store_cap < 2 || owner_cap < 65 ||
        snprintf(path, sizeof(path), "%s/pending-root-release", dir) >=
            (int)sizeof(path)) return EINVAL;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    ssize_t n = read(fd, record, sizeof(record) - 1);
    e = n < 0 ? errno : 0;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e) return e;
    record[n] = '\0';
    separator = strchr(record, '\n');
    if (!separator) return EINVAL;
    *separator++ = '\0';
    end = strchr(separator, '\n');
    if (!end || end[1] != '\0') return EINVAL;
    *end = '\0';
    if (strlen(separator) != 64 || !fo_path_is_absolute(record) ||
        strlen(record) >= (size_t)store_cap ||
        strlen(separator) >= (size_t)owner_cap) return EINVAL;
    strcpy(store, record);
    strcpy(owner, separator);
    return 0;
}

int fo_gremlin_generation_forget(const char *root) {
    char dir[PATH_MAX], canonical[PATH_MAX], path[PATH_MAX];
    char recorded[PATH_MAX + 1];
    struct stat st;
    int e = generation_state_path(root, dir, sizeof(dir), canonical,
                                  sizeof(canonical));
    if (e) return e;
    int fd = lock_in(dir, "guard.lock", LOCK_EX);
    if (fd < 0) return -fd;
    e = read_generation_root(dir, recorded, sizeof(recorded));
    if (e == 0 && strcmp(recorded, canonical) != 0) e = EEXIST;
    if (e == 0 && lstat(recorded, &st) == 0) e = EBUSY;
    if (e == 0 && errno != ENOENT) e = errno;
    if (e == 0) {
        if (snprintf(path, sizeof(path), "%s/pending-root-release", dir) >=
            (int)sizeof(path)) e = ENAMETOOLONG;
        else if (unlink(path) != 0 && errno != ENOENT) e = errno;
    }
    if (e == 0) {
        if (snprintf(path, sizeof(path), "%s/root", dir) >= (int)sizeof(path))
            e = ENAMETOOLONG;
        else if (unlink(path) != 0 && errno != ENOENT) e = errno;
    }
    if (e == 0) e = fo_sys_sync_dir(dir);
    fo_sys_unlock(fd);
    return e;
}

int fo_gremlin_generation_register_lease_at(const char *root, int *fdout) {
    if (!fdout) return EINVAL;
    *fdout = -1;
    return generation_register_path(root, fdout);
}

int fo_gremlin_generation_lease_acquire_at(const char *root, int *fdout) {
    return generation_lease_path(root, fdout);
}

int fo_gremlin_generation_pin_at(const char *root, int pinned) {
    return generation_pin_path(root, pinned);
}

int fo_gremlin_generation_prune_at(const char *root) {
    return generation_prune_path(root);
}

/* ---- Storage retention: dead lanes and stale generations ----
   The scan is read-only and advisory; every removal rechecks its guard under
   the owning lock.  A lane is reclaimable only when no process holds its
   owner.lock, its recorded owner and every recorded async process are not
   running (fo_gremlin_process_matches), and nothing in it changed within the
   grace period.  A generation is reclaimable only when no lease holds it, its
   registration, pin and last lease are older than the grace period, and no
   live lane status or recent terminal status names it. */

enum { EVICT_LANE = 1, EVICT_GENERATION = 2, EVICT_TOMBSTONE = 3 };
#define GREMLIN_REF_TEXT 65536

struct evict_item {
    char path[PATH_MAX];
    int kind;
    long long used;
    long long bytes;
};

struct evict_scan {
    struct evict_item *items;
    size_t count, cap;
    char (*refs)[65];
    size_t nrefs, refcap;
    long long total;
};

static long long evict_tree_bytes(const char *path, int depth) {
    struct stat st;
    long long total = 0;
    if (depth > 128 || lstat(path, &st) != 0) return 0;
    if (!S_ISDIR(st.st_mode)) return S_ISREG(st.st_mode) ? (long long)st.st_size : 0;
    DIR *dir = opendir(path);
    if (!dir) return 0;
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        char child[PATH_MAX];
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) continue;
        total += evict_tree_bytes(child, depth + 1);
    }
    closedir(dir);
    return total;
}

static void evict_newer(const char *path, long long *newest) {
    struct stat st;
    if (lstat(path, &st) == 0 && (long long)st.st_mtime > *newest)
        *newest = (long long)st.st_mtime;
}

/* Newest mtime of the lane's entries and their direct entries: a status,
   journal, log or view changes there while the lane is in use.  owner.lock is
   never written and the evictor itself may create it, so it does not count. */
static long long lane_last_use(const char *dir) {
    long long newest = 0;
    DIR *top = opendir(dir);
    struct dirent *entry;
    if (!top) return newest;
    while ((entry = readdir(top)) != NULL) {
        char child[PATH_MAX];
        struct stat st;
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..") ||
            !strcmp(entry->d_name, "owner.lock")) continue;
        if (snprintf(child, sizeof(child), "%s/%s", dir, entry->d_name) >=
            (int)sizeof(child)) continue;
        if (lstat(child, &st) != 0) continue;
        if ((long long)st.st_mtime > newest) newest = (long long)st.st_mtime;
        if (!S_ISDIR(st.st_mode)) continue;
        DIR *sub = opendir(child);
        struct dirent *inner;
        if (!sub) continue;
        while ((inner = readdir(sub)) != NULL) {
            char grandchild[PATH_MAX];
            if (!strcmp(inner->d_name, ".") || !strcmp(inner->d_name, "..")) continue;
            if (snprintf(grandchild, sizeof(grandchild), "%s/%s", child,
                         inner->d_name) >= (int)sizeof(grandchild)) continue;
            evict_newer(grandchild, &newest);
        }
        closedir(sub);
    }
    closedir(top);
    return newest;
}

/* 1 when a process recorded in the lane's async registries may still run.
   Record names are <pid>-<start>.<kind>; an unparsable one counts as running. */
static int lane_async_running(const char *dir) {
    char base[PATH_MAX];
    DIR *registries;
    struct dirent *entry;
    int running = 0;
    if (snprintf(base, sizeof(base), "%s/async-processes", dir) >= (int)sizeof(base))
        return 1;
    registries = opendir(base);
    if (!registries) return errno == ENOENT ? 0 : 1;
    while (!running && (entry = readdir(registries)) != NULL) {
        char registry[PATH_MAX];
        DIR *records;
        struct dirent *record;
        if (entry->d_name[0] == '.') continue;
        if (snprintf(registry, sizeof(registry), "%s/%s", base, entry->d_name) >=
            (int)sizeof(registry)) { running = 1; break; }
        records = opendir(registry);
        if (!records) continue;
        while ((record = readdir(records)) != NULL) {
            char name[256], *dash, *dot;
            if (record->d_name[0] == '.') continue;
            if (strlen(record->d_name) >= sizeof(name)) { running = 1; break; }
            strcpy(name, record->d_name);
            dash = strchr(name, '-');
            dot = strrchr(name, '.');
            if (!dash || !dot || dot < dash) { running = 1; break; }
            *dash = '\0';
            *dot = '\0';
            if (fo_gremlin_process_matches(atoi(name), dash + 1)) { running = 1; break; }
        }
        closedir(records);
    }
    closedir(registries);
    return running;
}

/* Probe or take the lane: returns an fd holding owner.lock exclusively when the
   lane has no running owner or async process, -EBUSY when it is in use, or
   another negative errno.  With create == 0 a missing lock file is no owner. */
static int lane_claim(const char *dir, int create) {
    char path[PATH_MAX], id[128], start[64];
    int fd, pid = 0;
    if (snprintf(path, sizeof(path), "%s/owner.lock", dir) >= (int)sizeof(path))
        return -ENAMETOOLONG;
    fd = open(path, O_RDWR | O_CLOEXEC | O_NOFOLLOW | (create ? O_CREAT : 0), 0600);
    if (fd < 0) {
        if (errno != ENOENT || create) return -errno;
        fd = INT_MAX;
    } else if (flock(fd, LOCK_EX | LOCK_NB) != 0) {
        close(fd);
        return -EBUSY;
    }
    if ((read_owner(dir, id, sizeof(id), &pid, start, sizeof(start)) == 0 &&
         fo_gremlin_process_matches(pid, start)) || lane_async_running(dir)) {
        if (fd != INT_MAX) fo_sys_unlock(fd);
        return -EBUSY;
    }
    return fd;
}

static void lane_unclaim(int fd) {
    if (fd >= 0 && fd != INT_MAX) fo_sys_unlock(fd);
}

static int evict_push(struct evict_scan *scan, const char *path, int kind,
                      long long used, long long bytes) {
    if (scan->count == scan->cap) {
        size_t cap = scan->cap ? scan->cap * 2 : 64;
        struct evict_item *next = realloc(scan->items, cap * sizeof(*next));
        if (!next) return ENOMEM;
        scan->items = next;
        scan->cap = cap;
    }
    struct evict_item *item = &scan->items[scan->count++];
    if (strlen(path) >= sizeof(item->path)) { --scan->count; return ENAMETOOLONG; }
    strcpy(item->path, path);
    item->kind = kind;
    item->used = used;
    item->bytes = bytes;
    return 0;
}

/* Record every 64-hex generation identity a status text names. */
static int evict_add_refs(struct evict_scan *scan, const char *path) {
    char text[GREMLIN_REF_TEXT];
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    ssize_t n;
    const char *key = "generation\":\"";
    if (fd < 0) return 0;
    n = read(fd, text, sizeof(text) - 1);
    close(fd);
    if (n <= 0) return 0;
    text[n] = '\0';
    for (char *at = strstr(text, key); at; at = strstr(at + 1, key)) {
        char *id = at + strlen(key);
        size_t i;
        for (i = 0; i < 64; ++i)
            if (!((id[i] >= '0' && id[i] <= '9') || (id[i] >= 'a' && id[i] <= 'f')))
                break;
        if (i != 64 || id[64] != '"') continue;
        if (scan->nrefs == scan->refcap) {
            size_t cap = scan->refcap ? scan->refcap * 2 : 64;
            char (*next)[65] = realloc(scan->refs, cap * sizeof(*next));
            if (!next) return ENOMEM;
            scan->refs = next;
            scan->refcap = cap;
        }
        memcpy(scan->refs[scan->nrefs], id, 64);
        scan->refs[scan->nrefs++][64] = '\0';
    }
    return 0;
}

static int evict_referenced(const struct evict_scan *scan, const char *id) {
    for (size_t i = 0; i < scan->nrefs; ++i)
        if (!strcmp(scan->refs[i], id)) return 1;
    return 0;
}

static int valid_lane_name(const char *name) {
    if (strlen(name) != 16) return 0;
    for (const char *p = name; *p; ++p)
        if (!((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f'))) return 0;
    return 1;
}

static int scan_lanes(struct evict_scan *scan, const char *root, long long now,
                      long long grace) {
    char projects[PATH_MAX];
    DIR *outer;
    struct dirent *project;
    int e = 0;
    if (snprintf(projects, sizeof(projects), "%s/projects", root) >=
        (int)sizeof(projects)) return ENAMETOOLONG;
    outer = opendir(projects);
    if (!outer) return errno == ENOENT ? 0 : errno;
    while (e == 0 && (project = readdir(outer)) != NULL) {
        char pdir[PATH_MAX];
        DIR *inner;
        struct dirent *lane;
        if (!valid_lane_name(project->d_name)) continue;
        if (snprintf(pdir, sizeof(pdir), "%s/%s", projects, project->d_name) >=
            (int)sizeof(pdir)) continue;
        inner = opendir(pdir);
        if (!inner) continue;
        while (e == 0 && (lane = readdir(inner)) != NULL) {
            char dir[PATH_MAX], status[PATH_MAX];
            struct stat st;
            if (snprintf(dir, sizeof(dir), "%s/%s", pdir, lane->d_name) >=
                (int)sizeof(dir)) continue;
            if (!strncmp(lane->d_name, ".evict-", 7)) {
                /* A tombstone outlives only an interrupted eviction. */
                if (lstat(dir, &st) == 0 && S_ISDIR(st.st_mode) &&
                    now - (long long)st.st_ctime >= 3600) {
                    long long bytes = evict_tree_bytes(dir, 0);
                    scan->total += bytes;
                    e = evict_push(scan, dir, EVICT_TOMBSTONE, 0, bytes);
                }
                continue;
            }
            if (!valid_lane_name(lane->d_name)) continue;
            if (lstat(dir, &st) != 0 || !S_ISDIR(st.st_mode)) continue;
            long long bytes = evict_tree_bytes(dir, 0);
            long long used = lane_last_use(dir);
            scan->total += bytes;
            int fd = lane_claim(dir, 0);
            if (fd >= 0) lane_unclaim(fd);
            if (fd >= 0 && now - used >= grace) {
                e = evict_push(scan, dir, EVICT_LANE, used, bytes);
                continue;
            }
            if (fd < 0 && fd != -EBUSY) continue;
            /* A kept lane keeps the generations its status names. */
            if (snprintf(status, sizeof(status), "%s/status", dir) <
                (int)sizeof(status)) e = evict_add_refs(scan, status);
        }
        closedir(inner);
    }
    closedir(outer);
    return e;
}

/* terminal/<project>/<lane>/<session>/status within the grace period. */
static int scan_terminal_refs(struct evict_scan *scan, const char *root,
                              long long now, long long grace) {
    char level[3][PATH_MAX];
    DIR *dirs[3] = {NULL, NULL, NULL};
    struct dirent *entry;
    int depth = 0, e = 0;
    if (snprintf(level[0], sizeof(level[0]), "%s/terminal", root) >=
        (int)sizeof(level[0])) return ENAMETOOLONG;
    dirs[0] = opendir(level[0]);
    if (!dirs[0]) return 0;
    while (depth >= 0 && e == 0) {
        entry = readdir(dirs[depth]);
        if (!entry) {
            closedir(dirs[depth]);
            dirs[depth--] = NULL;
            continue;
        }
        if (entry->d_name[0] == '.') continue;
        char child[PATH_MAX];
        if (snprintf(child, sizeof(child), "%s/%s", level[depth], entry->d_name) >=
            (int)sizeof(child)) continue;
        if (depth == 2) {
            char status[PATH_MAX];
            struct stat st;
            if (snprintf(status, sizeof(status), "%s/status", child) <
                    (int)sizeof(status) && lstat(status, &st) == 0 &&
                now - (long long)st.st_mtime < grace)
                e = evict_add_refs(scan, status);
            continue;
        }
        DIR *next = opendir(child);
        if (!next) continue;
        strcpy(level[depth + 1], child);
        dirs[++depth] = next;
    }
    for (int i = 0; i < 3; ++i) if (dirs[i]) closedir(dirs[i]);
    return e;
}

/* Exclusive probe of an existing lock file without creating it: an fd, INT_MAX
   when the file does not exist, or a negative errno (-EBUSY when held). */
static int probe_lock(const char *dir, const char *name) {
    char path[PATH_MAX];
    int fd;
    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return -ENAMETOOLONG;
    fd = open(path, O_RDWR | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno == ENOENT ? INT_MAX : -errno;
    if (flock(fd, LOCK_EX | LOCK_NB) != 0) {
        close(fd);
        return -EBUSY;
    }
    return fd;
}

static long long generation_last_use(const char *sidecar) {
    static const char *names[] = {"registered", "pinned", "lease.lock",
                                  "pending-root-release"};
    long long newest = 0;
    for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
        char path[PATH_MAX];
        if (snprintf(path, sizeof(path), "%s/%s", sidecar, names[i]) <
            (int)sizeof(path)) evict_newer(path, &newest);
    }
    return newest;
}

static int scan_generations(struct evict_scan *scan, const char *root,
                            long long now, long long grace) {
    char base[PATH_MAX], states[PATH_MAX], resolved[PATH_MAX];
    DIR *dir;
    struct dirent *entry;
    int e = 0;
    if (snprintf(base, sizeof(base), "%s/generations-v2", root) >= (int)sizeof(base))
        return ENAMETOOLONG;
    if (!realpath(base, resolved)) return errno == ENOENT ? 0 : errno;
    if (snprintf(states, sizeof(states), "%s/.state", resolved) >= (int)sizeof(states))
        return ENAMETOOLONG;
    dir = opendir(states);
    if (!dir) return errno == ENOENT ? 0 : errno;
    while (e == 0 && (entry = readdir(dir)) != NULL) {
        char sidecar[PATH_MAX], generation[PATH_MAX], path[PATH_MAX];
        struct stat st;
        if (!valid_generation_id(entry->d_name)) continue;
        if (snprintf(sidecar, sizeof(sidecar), "%s/%s", states, entry->d_name) >=
                (int)sizeof(sidecar) ||
            snprintf(generation, sizeof(generation), "%s/%s", resolved,
                     entry->d_name) >= (int)sizeof(generation)) continue;
        int exists = lstat(generation, &st) == 0 && S_ISDIR(st.st_mode);
        long long bytes = exists ? evict_tree_bytes(generation, 0) : 0;
        scan->total += bytes;
        if (snprintf(path, sizeof(path), "%s/registered", sidecar) >=
            (int)sizeof(path)) continue;
        int registered = lstat(path, &st) == 0 && S_ISREG(st.st_mode);
        if (snprintf(path, sizeof(path), "%s/pending-root-release", sidecar) >=
            (int)sizeof(path)) continue;
        int pending = lstat(path, &st) == 0 && S_ISREG(st.st_mode);
        if (!(exists && registered) && !pending) continue;
        long long used = generation_last_use(sidecar);
        if (now - used < grace) continue;
        if (evict_referenced(scan, entry->d_name)) continue;
        /* Probe the lease under the guard, as fo_c_gremlin_retention does, so
           a concurrent lease acquirer waits on the guard instead of failing. */
        int gfd = probe_lock(sidecar, "guard.lock");
        if (gfd < 0) continue;
        int lfd = probe_lock(sidecar, "lease.lock");
        if (lfd >= 0 && lfd != INT_MAX) fo_sys_unlock(lfd);
        if (gfd != INT_MAX) fo_sys_unlock(gfd);
        if (lfd < 0) continue;
        e = evict_push(scan, generation, EVICT_GENERATION, used, bytes);
    }
    closedir(dir);
    return e;
}

static int cmp_evict_item(const void *l, const void *r) {
    const struct evict_item *a = l, *b = r;
    if (a->used != b->used) return a->used < b->used ? -1 : 1;
    return strcmp(a->path, b->path);
}

/* Reclaimable lanes, tombstones and generations below a Gremlin root, least
   recently used first, at most limit of them in PATH_MAX slots.  total is the
   apparent size of all lane directories and generations.  Read-only. */
int fo_gremlin_evict_scan(const char *root, long long grace, char *paths,
                          int slot_size, int limit, int *kinds, long long *used,
                          long long *bytes, int *count, long long *total) {
    struct evict_scan scan = {0};
    long long now = (long long)time(NULL);
    int e;
    *count = 0;
    *total = 0;
    if (!root || !*root || slot_size < PATH_MAX || limit < 0 || grace < 0)
        return EINVAL;
    e = scan_lanes(&scan, root, now, grace);
    if (e == 0) e = scan_terminal_refs(&scan, root, now, grace);
    if (e == 0) e = scan_generations(&scan, root, now, grace);
    if (e == 0) {
        qsort(scan.items, scan.count, sizeof(*scan.items), cmp_evict_item);
        for (size_t i = 0; i < scan.count && (int)i < limit; ++i) {
            strcpy(paths + i * (size_t)slot_size, scan.items[i].path);
            kinds[i] = scan.items[i].kind;
            used[i] = scan.items[i].used;
            bytes[i] = scan.items[i].bytes;
            ++*count;
        }
        *total = scan.total;
    }
    free(scan.items);
    free(scan.refs);
    return e;
}

/* Remove a dead, unused lane directory.  Holding its owner.lock, recheck the
   owner, async processes and last use, release its private execution scratch
   like a replacement owner, then rename it to a tombstone so a concurrent
   acquirer starts on a fresh directory (see fo_gremlin_session_acquire), and
   delete the tombstone.  EBUSY: the lane is in use and stays. */
int fo_gremlin_lane_evict(const char *dir, long long grace) {
    char tomb[PATH_MAX];
    const char *name;
    struct stat st;
    int fd, e;
    if (!dir || !fo_path_is_absolute(dir) || grace < 0) return EINVAL;
    name = strrchr(dir, '/');
    if (!name || !valid_lane_name(name + 1)) return EINVAL;
    if (lstat(dir, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode) || !fo_private_path(dir)) return EPERM;
    fd = lane_claim(dir, 1);
    if (fd < 0) return -fd;
    if ((long long)time(NULL) - lane_last_use(dir) < grace) {
        lane_unclaim(fd);
        return EBUSY;
    }
    e = cleanup_view_scratch(dir);
    if (e == 0 &&
        snprintf(tomb, sizeof(tomb), "%.*s/.evict-%s-%ld", (int)(name - dir), dir,
                 name + 1, (long)getpid()) >= (int)sizeof(tomb)) e = ENAMETOOLONG;
    if (e == 0 && rename(dir, tomb) != 0) e = errno;
    lane_unclaim(fd);
    if (e == 0 && fo_c_rm_rf(tomb) != 0) e = errno ? errno : EIO;
    return e;
}

int fo_gremlin_tombstone_remove(const char *path) {
    const char *name = path ? strrchr(path, '/') : NULL;
    if (!name || strncmp(name + 1, ".evict-", 7) || !fo_path_is_absolute(path))
        return EINVAL;
    return fo_c_rm_rf(path) == 0 ? 0 : (errno ? errno : EIO);
}

/* Drop the pin of a generation no lease holds and nobody used within the
   grace period.  Under the guard, an owner can only pin after leasing, so a
   pin taken after this check is never lost; pruning follows separately and
   rechecks lease and pin (fo_gremlin_generation_prune_at). */
int fo_gremlin_generation_unpin_stale(const char *root, long long grace) {
    char dir[PATH_MAX], canonical[PATH_MAX], pin[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (snprintf(pin, sizeof(pin), "%s/pinned", dir) >= (int)sizeof(pin))
        return ENAMETOOLONG;
    int gfd = lock_in(dir, "guard.lock", LOCK_EX | LOCK_NB);
    if (gfd < 0) return -gfd == EAGAIN || -gfd == EWOULDBLOCK ? EBUSY : -gfd;
    int lfd = lock_in(dir, "lease.lock", LOCK_EX | LOCK_NB);
    if (lfd < 0) {
        fo_sys_unlock(gfd);
        return -lfd == EAGAIN || -lfd == EWOULDBLOCK ? EBUSY : -lfd;
    }
    if ((long long)time(NULL) - generation_last_use(dir) < grace) e = EBUSY;
    else if (unlink(pin) != 0 && errno != ENOENT) e = errno;
    else e = fo_sys_sync_dir(dir);
    fo_sys_unlock(lfd);
    fo_sys_unlock(gfd);
    return e;
}
