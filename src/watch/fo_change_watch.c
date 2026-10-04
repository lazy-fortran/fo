#define _XOPEN_SOURCE 700

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void fo_change_watch_error_text(int error, char *buffer, int capacity) {
    if (buffer == NULL || capacity <= 0) return;
    snprintf(buffer, (size_t)capacity, "%s (errno %d)", strerror(error), error);
}
#ifndef __APPLE__
void fo_change_native_diagnostic(void *handle, char *buffer, int capacity) {
    (void)handle;
    if (buffer != NULL && capacity > 0) buffer[0] = 0;
}
#endif

int fo_change_watch_realpath(const char *path, char *resolved, int capacity) {
    char *canonical;
    size_t length;
    if (path == NULL || resolved == NULL || capacity <= 0) return EINVAL;
    canonical = realpath(path, NULL);
    if (canonical == NULL) return errno == 0 ? EIO : errno;
    length = strlen(canonical);
    if (length >= (size_t)capacity) {
        free(canonical);
        return ENAMETOOLONG;
    }
    memcpy(resolved, canonical, length + 1);
    free(canonical);
    return 0;
}

int fo_change_watch_is_dir(const char *path) {
    struct stat info;
    return path != NULL && stat(path, &info) == 0 && S_ISDIR(info.st_mode);
}

/* Linux exposes reconciliation and errors that the pinned fx ABI discards.
 * Every directory is subscribed before enumeration; queued structural events
 * reconcile again, including directories populated during that enumeration. */
#ifdef __linux__
#include <dirent.h>
#include <poll.h>
#include <stdio.h>
#include <sys/inotify.h>
#include <time.h>
#include <unistd.h>

struct change_entry { int wd, seen; char *path; };
struct change_self { char *path; long long until; };
struct change_watch {
    int fd, epoch, reconcile_pending;
    long long reconcile_deadline, reconcile_maximum;
    union { char bytes[65536]; struct inotify_event alignment; } pending;
    size_t pending_pos, pending_len;
    char **roots;
    size_t nroots, nentries;
    struct change_entry *entries;
    struct change_self *self;
    size_t nself;
};
static long long change_now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}
static int change_excluded(const char *rel) {
    const char *part = rel;
    while (*part) {
        size_t n = strcspn(part, "/");
        if ((n == 4 && !strncmp(part, ".git", n)) ||
            (n == 3 && !strncmp(part, ".hg", n)) ||
            (n == 4 && !strncmp(part, ".bzr", n)) ||
            (n == 4 && !strncmp(part, ".svn", n)) ||
            (n == 8 && !strncmp(part, ".gremlin", n)) ||
            (part == rel && n == 5 && !strncmp(part, "build", n))) return 1;
        part += n;
        if (*part) ++part;
    }
    return 0;
}
static int change_relevant(struct change_watch *w, const char *path) {
    size_t i;
    for (i = 0; i < w->nroots; ++i) {
        size_t n = strlen(w->roots[i]);
        if (!strcmp(path, w->roots[i])) return 1;
        if (!strncmp(path, w->roots[i], n) && path[n] == '/' &&
            !change_excluded(path + n + 1)) return 1;
    }
    return 0;
}
static int change_subscribe(struct change_watch *w, const char *path) {
    struct change_entry *next;
    size_t i;
    int wd = inotify_add_watch(w->fd, path, IN_MODIFY | IN_ATTRIB |
        IN_CREATE | IN_DELETE | IN_MOVED_FROM | IN_MOVED_TO |
        IN_DELETE_SELF | IN_MOVE_SELF | IN_ONLYDIR | IN_DONT_FOLLOW);
    if (wd < 0) return errno == ENOENT || errno == ENOTDIR ? 0 : errno;
    for (i = 0; i < w->nentries; ++i) {
        if (w->entries[i].wd == wd) {
            if (strcmp(w->entries[i].path, path)) {
                char *replacement = strdup(path);
                if (!replacement) return ENOMEM;
                free(w->entries[i].path);
                w->entries[i].path = replacement;
            }
            w->entries[i].seen = w->epoch;
            return 0;
        }
    }
    next = realloc(w->entries, (w->nentries + 1) * sizeof(*next));
    if (!next) { inotify_rm_watch(w->fd, wd); return ENOMEM; }
    w->entries = next;
    next[w->nentries].wd = wd;
    next[w->nentries].seen = w->epoch;
    next[w->nentries].path = strdup(path);
    if (!next[w->nentries].path) { inotify_rm_watch(w->fd, wd); return ENOMEM; }
    ++w->nentries;
    return 0;
}
static int change_tree(struct change_watch *w, const char *path) {
    DIR *dir;
    struct dirent *entry;
    char child[PATH_MAX];
    struct stat st;
    int err = change_subscribe(w, path);
    if (err) return err;
    dir = opendir(path);
    if (!dir) return errno == ENOENT || errno == ENOTDIR ? 0 : errno;
    for (;;) {
        errno = 0;
        entry = readdir(dir);
        if (!entry) { err = errno; break; }
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) { err = ENAMETOOLONG; break; }
        if (!change_relevant(w, child)) continue;
        if (lstat(child, &st)) {
            if (errno == ENOENT) continue;
            err = errno; break;
        }
        if (S_ISDIR(st.st_mode)) {
            err = change_tree(w, child);
            if (err) break;
        }
    }
    closedir(dir);
    return err;
}
int fo_change_native_reconcile(void *handle) {
    struct change_watch *w = handle;
    size_t i;
    int err;
    char parent[PATH_MAX], *slash;
    if (!w) return EINVAL;
    ++w->epoch;
    for (i = 0; i < w->nroots; ++i) {
        strcpy(parent, w->roots[i]);
        slash = strrchr(parent, '/');
        if (!slash) return EINVAL;
        if (slash == parent) slash[1] = 0; else *slash = 0;
        err = change_subscribe(w, parent);
        if (err) return err;
        err = change_tree(w, w->roots[i]);
        if (err) return err;
    }
    for (i = w->nentries; i > 0; --i) {
        struct change_entry *entry = &w->entries[i - 1];
        if (entry->seen == w->epoch) continue;
        inotify_rm_watch(w->fd, entry->wd);
        free(entry->path);
        *entry = w->entries[--w->nentries];
    }
    return 0;
}
void *fo_change_native_open(int *error) {
    struct change_watch *w = calloc(1, sizeof(*w));
    *error = 0;
    if (!w) { *error = ENOMEM; return NULL; }
    w->fd = inotify_init1(IN_CLOEXEC | IN_NONBLOCK);
    if (w->fd < 0) { *error = errno; free(w); return NULL; }
    return w;
}
void *fo_change_native_open_diagnostic(int *error, char *diagnostic, int capacity) {
    void *handle = fo_change_native_open(error);
    if (diagnostic != NULL && capacity > 0) {
        if (handle || *error == 0) diagnostic[0] = 0;
        else snprintf(diagnostic, (size_t)capacity,
            "open inotify provider: %s (errno %d)", strerror(*error), *error);
    }
    return handle;
}
void fo_change_native_clear_roots(void *handle) {
    struct change_watch *w = handle;
    size_t i;
    for (i = 0; i < w->nroots; ++i) free(w->roots[i]);
    free(w->roots); w->roots = NULL; w->nroots = 0;
}
int fo_change_native_root(void *handle, const char *path) {
    struct change_watch *w = handle;
    char **next = realloc(w->roots, (w->nroots + 1) * sizeof(*next));
    if (!next) return ENOMEM;
    w->roots = next;
    next[w->nroots] = strdup(path);
    if (!next[w->nroots]) return ENOMEM;
    ++w->nroots;
    return 0;
}
static int change_classify(int mask) {
    if ((unsigned)mask & IN_Q_OVERFLOW) return 4;
    if ((unsigned)mask & (IN_DELETE | IN_DELETE_SELF | IN_MOVED_FROM | IN_MOVE_SELF)) return 3;
    if ((unsigned)mask & (IN_CREATE | IN_MOVED_TO)) return 2;
    if ((unsigned)mask & (IN_MODIFY | IN_ATTRIB)) return 1;
    return 0;
}
static void change_discard_queued(struct change_watch *w) {
    int batch;
    w->pending_pos = w->pending_len;
    for (batch = 0; batch < 8; ++batch) {
        ssize_t count = read(w->fd, w->pending.bytes, sizeof(w->pending.bytes));
        if (count <= 0) break;
    }
    w->pending_pos = w->pending_len = 0;
}
int fo_change_native_poll(void *handle, int timeout, char *path, int capacity, int *kind) {
    struct change_watch *w = handle;
    struct pollfd pfd;
    ssize_t count;
    size_t i;
    int structural = 0, batch, rc;
    if (!w || capacity < PATH_MAX) return EINVAL;
    *path = 0; *kind = 0;
    if (w->reconcile_pending) {
        long long now = change_now();
        long long remaining = w->reconcile_deadline - now;
        if (timeout <= 0) return 0;
        if (now < w->reconcile_deadline && now < w->reconcile_maximum) {
            int wait_ms = (int)(remaining < timeout ? remaining : timeout);
            pfd.fd = w->fd; pfd.events = POLLIN; pfd.revents = 0;
            rc = poll(&pfd, 1, wait_ms);
            if (rc < 0) return errno == EINTR ? 0 : errno;
            if (rc == 0 && wait_ms < timeout) return 0;
            if (rc > 0) {
                change_discard_queued(w);
                now = change_now();
                w->reconcile_deadline = now + 100;
                if (w->reconcile_deadline > w->reconcile_maximum)
                    w->reconcile_deadline = w->reconcile_maximum;
                return 0;
            }
            if (change_now() < w->reconcile_deadline &&
                change_now() < w->reconcile_maximum) return 0;
        }
        change_discard_queued(w);
        w->reconcile_pending = 0;
        rc = fo_change_native_reconcile(w);
        if (rc) return rc;
        *kind = 4;
        return 0;
    }
    pfd.fd = w->fd; pfd.events = POLLIN; pfd.revents = 0;
    rc = w->pending_pos < w->pending_len ? 1 : poll(&pfd, 1, timeout);
    if (rc < 0) return errno == EINTR ? 0 : errno;
    if (rc == 0) return 0;
    if (pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) return EIO;
    /* Bound work even under an unrelated event storm; unread events stay queued. */
    for (batch = 0; batch < 8; ++batch) {
        if (w->pending_pos == w->pending_len) {
            count = read(w->fd, w->pending.bytes, sizeof(w->pending.bytes));
            if (count < 0) {
                if (errno == EAGAIN || errno == EINTR) break;
                return errno;
            }
            if (count == 0) return EIO;
            w->pending_pos = 0; w->pending_len = (size_t)count;
        }
        while (w->pending_pos < w->pending_len) {
            struct inotify_event *event = (void *)(w->pending.bytes + w->pending_pos);
            int type = change_classify((int)event->mask);
            char candidate[PATH_MAX];
            w->pending_pos += sizeof(*event) + event->len;
            if (type == 4) { structural = 1; *kind = 4; *path = 0; continue; }
            if (event->mask & IN_IGNORED) {
                for (i = 0; i < w->nentries; ++i) {
                    if (w->entries[i].wd != event->wd) continue;
                    free(w->entries[i].path);
                    w->entries[i] = w->entries[--w->nentries]; break;
                }
                continue;
            }
            for (i = 0; i < w->nentries; ++i) if (w->entries[i].wd == event->wd) break;
            if (!type || i == w->nentries) continue;
            if (snprintf(candidate, sizeof(candidate), "%s%s%s", w->entries[i].path,
                event->len ? "/" : "", event->len ? event->name : "") >= (int)sizeof(candidate))
                return ENAMETOOLONG;
            if (!change_relevant(w, candidate)) continue;
            if ((event->mask & (IN_ISDIR | IN_DELETE_SELF | IN_MOVE_SELF)) && type != 1)
                structural = 1;
            for (i = 0; i < w->nself; ++i)
                if (type == 1 && !strcmp(w->self[i].path, candidate) &&
                    change_now() <= w->self[i].until) break;
            if (i < w->nself) { w->self[i].until = 0; continue; }
            if (*kind == 4) continue;
            strcpy(path, candidate); *kind = type;
            /* Preserve each file notification for fo watch --fmt. Gremlin's
             * shared debounce coalesces them without dropping formatter work. */
            if (structural) {
                change_discard_queued(w);
                if (timeout == 0) {
                    long long now = change_now();
                    w->reconcile_pending = 1;
                    w->reconcile_deadline = now + 100;
                    w->reconcile_maximum = now + 500;
                    *kind = 0; *path = 0;
                    return 0;
                }
                rc = fo_change_native_reconcile(w);
                if (rc) return rc;
                *kind = 4; *path = 0;
            }
            return 0;
        }
    }
    if (structural) {
        change_discard_queued(w);
        if (timeout == 0) {
            long long now = change_now();
            w->reconcile_pending = 1;
            w->reconcile_deadline = now + 100;
            w->reconcile_maximum = now + 500;
            *kind = 0; *path = 0;
            return 0;
        }
        rc = fo_change_native_reconcile(w);
        if (rc) return rc;
        /* Include files populated before the new directory subscription. */
        *kind = 4; *path = 0;
    }
    return 0;
}
/* A bounded poll may stop behind ignored notifications. Green consumers need
 * proof that neither the buffered notifications nor the kernel queue remains. */
int fo_change_native_pending(void *handle) {
    struct change_watch *w = handle;
    struct pollfd pfd;
    if (!w || w->reconcile_pending || w->pending_pos < w->pending_len) return 1;
    pfd.fd = w->fd; pfd.events = POLLIN; pfd.revents = 0;
    return poll(&pfd, 1, 0) != 0;
}
void fo_change_native_self(void *handle, const char *path) {
    struct change_watch *w = handle;
    struct change_self *next;
    long long now = change_now();
    size_t i;
    for (i = 0; i < w->nself; ++i) {
        if (!strcmp(w->self[i].path, path)) { w->self[i].until = now + 500; return; }
    }
    for (i = 0; i < w->nself; ++i) if (w->self[i].until < now) break;
    if (i == w->nself) {
        next = realloc(w->self, (w->nself + 1) * sizeof(*next));
        if (!next) return;
        w->self = next; w->self[i].path = NULL; ++w->nself;
    }
    next = w->self + i;
    free(next->path);
    next->path = strdup(path);
    if (!next->path) { *next = w->self[--w->nself]; return; }
    next->until = now + 500;
}
void fo_change_native_close(void *handle) {
    struct change_watch *w = handle;
    size_t i;
    if (!w) return;
    close(w->fd);
    fo_change_native_clear_roots(w);
    for (i = 0; i < w->nentries; ++i) free(w->entries[i].path);
    for (i = 0; i < w->nself; ++i) free(w->self[i].path);
    free(w->self); free(w->entries); free(w);
}
#elif !defined(__APPLE__)
void *fo_change_native_open(int *error) { *error = 0; return NULL; }
void *fo_change_native_open_diagnostic(int *error, char *diagnostic, int capacity) {
    void *handle = fo_change_native_open(error);
    if (diagnostic != NULL && capacity > 0) diagnostic[0] = 0;
    return handle;
}
void fo_change_native_close(void *handle) { (void)handle; }
void fo_change_native_clear_roots(void *handle) { (void)handle; }
int fo_change_native_root(void *h, const char *p) { (void)h; (void)p; return ENOSYS; }
int fo_change_native_reconcile(void *h) { (void)h; return ENOSYS; }
int fo_change_native_poll(void *h, int t, char *p, int n, int *k) {
    (void)h; (void)t; (void)p; (void)n; (void)k; return ENOSYS;
}
int fo_change_native_pending(void *h) { (void)h; return 1; }
void fo_change_native_self(void *h, const char *p) { (void)h; (void)p; }
#endif
