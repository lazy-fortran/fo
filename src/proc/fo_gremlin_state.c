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
#include <time.h>
#include <unistd.h>

#ifdef __APPLE__
#include <libproc.h>
#include <sys/proc.h>
#endif

static int make_dirs(const char *path) {
    char tmp[PATH_MAX];
    size_t n = strlen(path);
    if (n == 0 || n >= sizeof(tmp)) return ENAMETOOLONG;
    memcpy(tmp, path, n + 1);
    for (char *p = tmp + 1; *p; ++p) {
        if (*p != '/') continue;
        *p = '\0';
        if (mkdir(tmp, 0700) != 0 && errno != EEXIST) return errno;
        *p = '/';
    }
    if (mkdir(tmp, 0700) != 0 && errno != EEXIST) return errno;
    return 0;
}

static uint64_t hash_bytes(uint64_t h, const unsigned char *s) {
    while (*s) { h ^= *s++; h *= UINT64_C(1099511628211); }
    return h;
}

int fo_gremlin_process_matches(int pid, const char *start);
int fo_c_recover_async_scope(const char *state_dir, int owner_pid,
                             const char *owner_start);
int fo_c_rm_rf(const char *path);

/* A replacement owner has already recovered the previous owner's children.
   Preserve retained execution views but discard their disposable scratch. */
static int cleanup_view_scratch(const char *state_dir) {
    char views[PATH_MAX], view[PATH_MAX], scratch[PATH_MAX];
    struct stat st;
    struct dirent *entry;
    DIR *directory;
    int e = 0;

    if (snprintf(views, sizeof(views), "%s/views", state_dir) >= (int)sizeof(views))
        return ENAMETOOLONG;
    if (lstat(views, &st) != 0) return errno == ENOENT ? 0 : errno;
    if (!S_ISDIR(st.st_mode) || st.st_uid != geteuid()) return EPERM;
    directory = opendir(views);
    if (directory == NULL) return errno;
    for (;;) {
        errno = 0;
        entry = readdir(directory);
        if (entry == NULL) { e = errno; break; }
        if (strncmp(entry->d_name, "execution-", 10) != 0) continue;
        if (snprintf(view, sizeof(view), "%s/%s", views, entry->d_name) >=
            (int)sizeof(view) ||
            snprintf(scratch, sizeof(scratch), "%s/.fo-tmp", view) >=
            (int)sizeof(scratch)) { e = ENAMETOOLONG; break; }
        if (lstat(view, &st) != 0) { e = errno; break; }
        if (!S_ISDIR(st.st_mode) || st.st_uid != geteuid()) { e = EPERM; break; }
        if (lstat(scratch, &st) != 0) {
            if (errno == ENOENT) continue;
            e = errno;
            break;
        }
        if (!S_ISDIR(st.st_mode) || st.st_uid != geteuid()) { e = EPERM; break; }
        if (fo_c_rm_rf(scratch) != 0) { e = errno ? errno : EIO; break; }
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
    uint64_t hp = hash_bytes(UINT64_C(1469598103934665603), (const unsigned char *)resolved);
    uint64_t hl = hash_bytes(UINT64_C(1469598103934665603), (const unsigned char *)lane);
    if (snprintf(out, cap, "%s/fo/gremlin/projects/%016llx/%016llx",
                 base, (unsigned long long)hp, (unsigned long long)hl) >= (int)cap)
        return ENAMETOOLONG;
    return create_dirs ? make_dirs(out) : 0;
}

int fo_gremlin_session_state_dir(const char *project, const char *lane,
                                char *dir, size_t dir_capacity) {
    char canonical[PATH_MAX];
    return state_path(project, lane, dir, dir_capacity, canonical,
                      sizeof(canonical), 0);
}

static int write_all(int fd, const char *s, size_t n) {
    while (n) {
        ssize_t k = write(fd, s, n);
        if (k < 0) { if (errno == EINTR) continue; return errno; }
        s += k; n -= (size_t)k;
    }
    return 0;
}

static int sync_directory(const char *dir) {
    int fd = open(dir, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    int e = fsync(fd) == 0 ? 0 : errno;
    close(fd);
    return e;
}

static int atomic_write_file(const char *dir, const char *name,
                             const char *text, size_t length) {
    char path[PATH_MAX], tmp[PATH_MAX];
    struct timespec ts;
    int n, fd, e;

    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return ENAMETOOLONG;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) return errno;
    n = snprintf(tmp, sizeof(tmp), "%s/.%s.%ld.%lld.%09ld.tmp", dir, name,
                 (long)getpid(), (long long)ts.tv_sec, ts.tv_nsec);
    if (n < 0 || n >= (int)sizeof(tmp)) return ENAMETOOLONG;
    fd = open(tmp, O_CREAT | O_EXCL | O_WRONLY | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    e = write_all(fd, text, length);
    if (e == 0 && fsync(fd) != 0) e = errno;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e == 0 && rename(tmp, path) != 0) e = errno;
    if (e == 0) e = sync_directory(dir);
    if (e != 0) unlink(tmp);
    return e;
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

static void clear_stop(const char *dir) {
    char path[PATH_MAX];
    if (stop_path(dir, path, sizeof(path)) == 0) unlink(path);
}

static int process_start(pid_t pid, char *out, size_t cap) {
#ifdef __APPLE__
    struct proc_bsdinfo info;
    int n = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, sizeof(info));
    if (n != sizeof(info)) return ESRCH;
    if (info.pbi_status == SZOMB) return ESRCH;
    if (snprintf(out, cap, "%llu.%06llu",
                 (unsigned long long)info.pbi_start_tvsec,
                 (unsigned long long)info.pbi_start_tvusec) >= (int)cap)
        return ENAMETOOLONG;
    return 0;
#else
    char path[64], buf[8192];
    snprintf(path, sizeof(path), "/proc/%ld/stat", (long)pid);
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    ssize_t n = read(fd, buf, sizeof(buf) - 1); close(fd);
    if (n < 0) return errno;
    buf[n] = 0;
    char *p = strrchr(buf, ')');
    if (!p || p[1] != ' ') return EINVAL;
    p += 2;
    if (p[0] == 'Z' || p[0] == 'X') return ESRCH;
    for (int field = 3; field < 22; ++field) {
        p = strchr(p, ' ');
        if (!p) return EINVAL;
        while (*p == ' ') ++p;
    }
    char *end = strchr(p, ' ');
    size_t len = end ? (size_t)(end - p) : strlen(p);
    if (len + 1 > cap) return ENAMETOOLONG;
    memcpy(out, p, len); out[len] = 0;
    return 0;
#endif
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
    for (int attempt = 0; attempt < 100; ++attempt) {
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) break;
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
    e = process_start(getpid(), stamp, sizeof(stamp));
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
    flock(fd, LOCK_UN); close(fd); return e;
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
    e = process_start(getpid(), current, sizeof(current));
    if (e || strcmp(current, start) != 0) return EPERM;
    e = read_recovery_source(dir, pending, sizeof(pending));
    if (e == ENOENT) return 0;
    if (e) return e;
    if (strcmp(pending, recovered) != 0) return EPERM;
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/recovery.source", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    if (unlink(path) != 0) return errno;
    return sync_directory(dir);
}

int fo_gremlin_session_release(const char *dir, const char *session, int fd) {
    int e = verify_owner_fd(dir, fd);
    if (e != 0) return e;
    char id[128], start[64]; int pid = 0;
    e = read_owner(dir, id, sizeof(id), &pid, start, sizeof(start));
    if (e) return e;
    if (strcmp(id, session) != 0 || pid != (int)getpid()) return EPERM;
    char current[64];
    e = process_start(getpid(), current, sizeof(current));
    if (e != 0 || strcmp(current, start) != 0) return EPERM;
    char path[PATH_MAX], statuspath[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/owner", dir) >= (int)sizeof(path) ||
        snprintf(statuspath, sizeof(statuspath), "%s/status", dir) >=
        (int)sizeof(statuspath)) return ENAMETOOLONG;
    clear_stop(dir);
    if (unlink(statuspath) != 0 && errno != ENOENT) return errno;
    if (unlink(path) != 0) return errno;
    (void)sync_directory(dir);
    if (fd >= 0) { flock(fd, LOCK_UN); close(fd); }
    return 0;
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
    e = process_start(getpid(), current, sizeof(current));
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

int fo_gremlin_process_matches(int pid, const char *start) {
    char have[64];
    if (pid <= 0 || !start || !*start) return 0;
    if (kill((pid_t)pid, 0) != 0 && errno != EPERM) return 0;
    if (process_start((pid_t)pid, have, sizeof(have)) != 0) return 0;
    return strcmp(have, start) == 0;
}

static int lease_dir(const char *kind, char *dir, size_t cap) {
    const char *base = getenv("FO_GREMLIN_STATE_DIR");
    if (!base || !*base) base = getenv("XDG_CACHE_HOME");
    char fallback[PATH_MAX];
    if (!base || !*base) {
        const char *home = getenv("HOME"); if (!home || !*home) return ENOENT;
        if (snprintf(fallback, sizeof(fallback), "%s/.cache", home) >= (int)sizeof(fallback)) return ENAMETOOLONG;
        base = fallback;
    }
    uint64_t h = hash_bytes(UINT64_C(1469598103934665603), (const unsigned char *)kind);
    if (snprintf(dir, cap, "%s/fo/gremlin/leases/%016llx", base, (unsigned long long)h) >= (int)cap) return ENAMETOOLONG;
    return make_dirs(dir);
}

int fo_gremlin_lease_acquire(const char *kind, int capacity, int *fdout, int *slot) {
    if (capacity < 1 || capacity > 1024) return EINVAL;
    *fdout = -1;
    *slot = -1;
    char dir[PATH_MAX], config[PATH_MAX];
    int e = lease_dir(kind, dir, sizeof(dir)); if (e) return e;
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
        if (sfd < 0) { e = errno; goto done; }
        if (flock(sfd, LOCK_EX | LOCK_NB) == 0) {
            if (i < capacity && *fdout < 0) { *fdout = sfd; *slot = i; }
            else { flock(sfd, LOCK_UN); close(sfd); }
        } else if (errno == EWOULDBLOCK || errno == EAGAIN) active++;
        else { e = errno; close(sfd); goto done; }
    }
    if (oldcap != 0 && oldcap != capacity && active > 0) { e = EBUSY; goto no_slot; }
    if (*fdout < 0) { e = EAGAIN; goto done; }
    char value[32]; int n = snprintf(value, sizeof(value), "%d\n", capacity);
    e = atomic_write_file(dir, "capacity", value, (size_t)n);
    if (e) goto undo;
    goto done;
no_slot:
    if (*fdout >= 0) { flock(*fdout, LOCK_UN); close(*fdout); *fdout = -1; }
    goto done;
undo:
    if (*fdout >= 0) { flock(*fdout, LOCK_UN); close(*fdout); *fdout = -1; }
done:
    flock(cfd, LOCK_UN); close(cfd); return e;
}

int fo_gremlin_lease_release(int fd) {
    if (fd < 0) return EINVAL;
    int e = flock(fd, LOCK_UN) == 0 ? 0 : errno;
    if (close(fd) != 0 && e == 0) e = errno;
    return e;
}

static int remove_generated_tree(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return errno == ENOENT ? 0 : errno;
    if (!S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode))
        return unlink(path) == 0 ? 0 : errno;

    /* Frozen generations have read-only directories.  Change only the
       directory being removed; files remain immutable until unlink. */
    if (chmod(path, 0700) != 0) return errno;

    DIR *dir = opendir(path);
    if (!dir) return errno;
    int e = 0;
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
        char child[PATH_MAX];
        int n = snprintf(child, sizeof(child), "%s/%s", path, entry->d_name);
        if (n < 0 || n >= (int)sizeof(child)) { e = ENAMETOOLONG; break; }
        e = remove_generated_tree(child);
        if (e != 0) break;
    }
    if (closedir(dir) != 0 && e == 0) e = errno;
    if (e != 0) return e;
    return rmdir(path) == 0 ? 0 : errno;
}

static int generation_base(char *base, size_t cap) {
    const char *value = getenv("FO_GREMLIN_STATE_DIR");
    if (!value || !*value) value = getenv("XDG_CACHE_HOME");
    char fallback[PATH_MAX];
    if (!value || !*value) {
        const char *home = getenv("HOME");
        if (!home || !*home) return ENOENT;
        if (snprintf(fallback, sizeof(fallback), "%s/.cache", home) >= (int)sizeof(fallback))
            return ENAMETOOLONG;
        value = fallback;
    }
    if (snprintf(base, cap, "%s/fo/gremlin/generation-state", value) >= (int)cap)
        return ENAMETOOLONG;
    return make_dirs(base);
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

static int generation_state_path_id(const char *id, char *dir, size_t cap) {
    char base[PATH_MAX];
    int e;
    if (!valid_generation_id(id)) return EINVAL;
    e = generation_base(base, sizeof(base));
    if (e) return e;
    if (snprintf(dir, cap, "%s/%s", base, id) >= (int)cap) return ENAMETOOLONG;
    return make_dirs(dir);
}

static int generation_state_path(const char *root, char *dir, size_t cap,
                                 char *canonical, size_t canonical_cap) {
    struct stat st;
    char resolved[PATH_MAX];
    const char *id;
    int e;
    if (!realpath(root, resolved)) return errno;
    if (strlen(resolved) + 1 > canonical_cap) return ENAMETOOLONG;
    if (lstat(resolved, &st) != 0) return errno;
    if (!S_ISDIR(st.st_mode) || (st.st_mode & 0222) != 0) return EPERM;
    id = strrchr(resolved, '/');
    id = id ? id + 1 : resolved;
    if (!valid_generation_id(id)) return EINVAL;
    strcpy(canonical, resolved);
    e = generation_state_path_id(id, dir, cap);
    return e;
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

static int generation_resolve(const char *id, char *root, size_t cap) {
    char dir[PATH_MAX], stored[PATH_MAX + 1], resolved[PATH_MAX];
    struct stat st;
    int e = generation_state_path_id(id, dir, sizeof(dir));
    if (e) return e;
    e = read_generation_root(dir, stored, sizeof(stored));
    if (e) return e;
    if (strlen(stored) + 1 > cap) return ENAMETOOLONG;
    if (!realpath(stored, resolved)) return errno;
    if (strcmp(stored, resolved) != 0 || !generation_valid_at(dir, resolved)) return ENOENT;
    if (lstat(resolved, &st) != 0) return errno;
    strcpy(root, resolved);
    return 0;
}

static int generation_register_path(const char *root) {
    char dir[PATH_MAX], canonical[PATH_MAX], guard[PATH_MAX], record[PATH_MAX + 1];
    struct stat st;
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard))
        return ENAMETOOLONG;
    int fd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    if (flock(fd, LOCK_EX) != 0) { e = errno; close(fd); return e; }
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
    if (e == 0) e = atomic_write_file(dir, "registered", "generation-v1\n", 14);
    if (e == 0) e = sync_directory(dir);
    if (lstat(canonical, &st) != 0 && e == 0) e = errno;
    flock(fd, LOCK_UN);
    close(fd);
    return e;
}

static int generation_lease_path(const char *root, int *fdout) {
    char dir[PATH_MAX], canonical[PATH_MAX], guard[PATH_MAX], lease[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return ENOENT;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard) ||
        snprintf(lease, sizeof(lease), "%s/lease.lock", dir) >= (int)sizeof(lease))
        return ENAMETOOLONG;
    int gfd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (gfd < 0) return errno;
    if (flock(gfd, LOCK_SH) != 0) { e = errno; close(gfd); return e; }
    if (!generation_valid_at(dir, canonical)) {
        flock(gfd, LOCK_UN);
        close(gfd);
        return ENOENT;
    }
    int lfd = open(lease, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (lfd < 0) { e = errno; flock(gfd, LOCK_UN); close(gfd); return e; }
    if (flock(lfd, LOCK_SH | LOCK_NB) != 0) {
        e = errno; close(lfd); flock(gfd, LOCK_UN); close(gfd); return e;
    }
    flock(gfd, LOCK_UN);
    close(gfd);
    *fdout = lfd;
    return 0;
}

static int generation_pin_path(const char *root, int pinned) {
    char dir[PATH_MAX], canonical[PATH_MAX], guard[PATH_MAX], pin[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return ENOENT;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard) ||
        snprintf(pin, sizeof(pin), "%s/pinned", dir) >= (int)sizeof(pin))
        return ENAMETOOLONG;
    int fd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    if (flock(fd, LOCK_EX) != 0) { e = errno; close(fd); return e; }
    if (!generation_valid_at(dir, canonical)) {
        flock(fd, LOCK_UN);
        close(fd);
        return ENOENT;
    }
    if (pinned) e = atomic_write_file(dir, "pinned", "pinned\n", 7);
    else if (unlink(pin) != 0 && errno != ENOENT) e = errno;
    else e = sync_directory(dir);
    flock(fd, LOCK_UN);
    close(fd);
    return e;
}

static int generation_prune_path(const char *root) {
    char dir[PATH_MAX], canonical[PATH_MAX], guard[PATH_MAX];
    char lease[PATH_MAX], pin[PATH_MAX], registered[PATH_MAX], root_record[PATH_MAX];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!generation_valid_at(dir, canonical)) return EPERM;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard) ||
        snprintf(lease, sizeof(lease), "%s/lease.lock", dir) >= (int)sizeof(lease) ||
        snprintf(pin, sizeof(pin), "%s/pinned", dir) >= (int)sizeof(pin) ||
        snprintf(registered, sizeof(registered), "%s/registered", dir) >= (int)sizeof(registered) ||
        snprintf(root_record, sizeof(root_record), "%s/root", dir) >= (int)sizeof(root_record))
        return ENAMETOOLONG;
    int gfd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (gfd < 0) return errno;
    if (flock(gfd, LOCK_EX | LOCK_NB) != 0) {
        e = errno;
        close(gfd);
        return e == EAGAIN || e == EWOULDBLOCK ? EBUSY : e;
    }
    if (!generation_valid_at(dir, canonical)) {
        flock(gfd, LOCK_UN);
        close(gfd);
        return ENOENT;
    }
    struct stat pin_stat;
    if (lstat(pin, &pin_stat) == 0) { flock(gfd, LOCK_UN); close(gfd); return EBUSY; }
    if (errno != ENOENT) { e = errno; flock(gfd, LOCK_UN); close(gfd); return e; }
    int lfd = open(lease, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (lfd < 0) { e = errno; flock(gfd, LOCK_UN); close(gfd); return e; }
    if (flock(lfd, LOCK_EX | LOCK_NB) != 0) {
        e = errno;
        close(lfd);
        flock(gfd, LOCK_UN);
        close(gfd);
        return e == EAGAIN || e == EWOULDBLOCK ? EBUSY : e;
    }
    e = remove_generated_tree(canonical);
    if (e == 0 && unlink(registered) != 0 && errno != ENOENT) e = errno;
    if (e == 0) e = sync_directory(dir);
    flock(lfd, LOCK_UN);
    close(lfd);
    flock(gfd, LOCK_UN);
    close(gfd);
    return e;
}

/* Keep the root locator after prune until Fo has released its durable Fx roots.
   A repeated prune can then finish a release interrupted by process death. */
int fo_gremlin_generation_recorded_root(const char *id, char *root, int cap) {
    char dir[PATH_MAX];
    int e;
    if (cap < 2) return EINVAL;
    e = generation_state_path_id(id, dir, sizeof(dir));
    return e == 0 ? read_generation_root(dir, root, (size_t)cap) : e;
}

int fo_gremlin_generation_mark_release(const char *root, const char *store,
                                       const char *owner) {
    char dir[PATH_MAX], canonical[PATH_MAX], guard[PATH_MAX], record[PATH_MAX + 128];
    int e = generation_state_path(root, dir, sizeof(dir), canonical, sizeof(canonical));
    if (e) return e;
    if (!store || store[0] != '/' || !owner || strlen(owner) != 64) return EINVAL;
    for (const char *p = store; *p; ++p)
        if (*p == '\n' || *p == '\r') return EINVAL;
    for (const char *p = owner; *p; ++p)
        if (!((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f'))) return EINVAL;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard))
        return ENAMETOOLONG;
    int fd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    if (flock(fd, LOCK_EX) != 0) { e = errno; close(fd); return e; }
    if (!generation_valid_at(dir, canonical)) e = ENOENT;
    if (e == 0) {
        int n = snprintf(record, sizeof(record), "%s\n%s\n", store, owner);
        if (n < 0 || n >= (int)sizeof(record)) e = ENAMETOOLONG;
        else e = atomic_write_file(dir, "pending-root-release", record, (size_t)n);
    }
    flock(fd, LOCK_UN);
    close(fd);
    return e;
}

int fo_gremlin_generation_pending_release(const char *id, char *store,
                                          int store_cap, char *owner, int owner_cap) {
    char dir[PATH_MAX], path[PATH_MAX], record[PATH_MAX + 128];
    char *separator, *end;
    int e = generation_state_path_id(id, dir, sizeof(dir));
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
    if (strlen(separator) != 64 || record[0] != '/' ||
        strlen(record) >= (size_t)store_cap ||
        strlen(separator) >= (size_t)owner_cap) return EINVAL;
    strcpy(store, record);
    strcpy(owner, separator);
    return 0;
}

int fo_gremlin_generation_forget(const char *id) {
    char dir[PATH_MAX], guard[PATH_MAX], root[PATH_MAX], path[PATH_MAX];
    struct stat st;
    int e = generation_state_path_id(id, dir, sizeof(dir));
    if (e) return e;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", dir) >= (int)sizeof(guard))
        return ENAMETOOLONG;
    int fd = open(guard, O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    if (flock(fd, LOCK_EX) != 0) { e = errno; close(fd); return e; }
    e = read_generation_root(dir, root, sizeof(root));
    if (e == 0 && lstat(root, &st) == 0) e = EBUSY;
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
    if (e == 0) e = sync_directory(dir);
    flock(fd, LOCK_UN);
    close(fd);
    return e;
}

int fo_gremlin_generation_register_at(const char *root) {
    return generation_register_path(root);
}

int fo_gremlin_generation_root(const char *id, char *root, int cap) {
    if (cap < 2) return EINVAL;
    return generation_resolve(id, root, (size_t)cap);
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

int fo_gremlin_generation_register(const char *id) {
    char root[PATH_MAX];
    int e = generation_resolve(id, root, sizeof(root));
    return e == 0 ? generation_register_path(root) : e;
}

int fo_gremlin_generation_lease_acquire(const char *id, int *fdout) {
    char root[PATH_MAX];
    int e = generation_resolve(id, root, sizeof(root));
    return e == 0 ? generation_lease_path(root, fdout) : e;
}

int fo_gremlin_generation_pin(const char *id, int pinned) {
    char root[PATH_MAX];
    int e = generation_resolve(id, root, sizeof(root));
    return e == 0 ? generation_pin_path(root, pinned) : e;
}

int fo_gremlin_generation_prune(const char *id) {
    char root[PATH_MAX];
    int e = generation_resolve(id, root, sizeof(root));
    return e == 0 ? generation_prune_path(root) : e;
}
