#define _XOPEN_SOURCE 700
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
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

static uint64_t hash_text(uint64_t h, const char *text) {
    for (const unsigned char *p = (const unsigned char *)text; *p; ++p) {
        h ^= *p;
        h *= UINT64_C(1099511628211);
    }
    return h;
}

static uint64_t hash_separator(uint64_t h) {
    h ^= 0;
    h *= UINT64_C(1099511628211);
    return h;
}

static int safe_session_id(const char *session) {
    size_t n = strlen(session);
    if (n == 0 || n > 120) return 0;
    for (size_t i = 0; i < n; ++i) {
        unsigned char c = (unsigned char)session[i];
        if (!(c == '-' || c == '_' || (c >= '0' && c <= '9') ||
              (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))) return 0;
    }
    return 1;
}

static int base_path(char *out, size_t cap) {
    const char *base = getenv("FO_GREMLIN_STATE_DIR");
    char fallback[PATH_MAX];
    if (!base || !*base) base = getenv("XDG_CACHE_HOME");
    if (!base || !*base) {
        const char *home = getenv("HOME");
        if (!home || !*home) return ENOENT;
        if (snprintf(fallback, sizeof(fallback), "%s/.cache", home) >=
            (int)sizeof(fallback)) return ENAMETOOLONG;
        base = fallback;
    }
    if (snprintf(out, cap, "%s/fo/gremlin", base) >= (int)cap)
        return ENAMETOOLONG;
    return make_dirs(out);
}

static int paths(const char *project, const char *lane, const char *session,
                 char *canonical, size_t canonical_cap,
                 char *terminal_parent, size_t terminal_parent_cap,
                 char *terminal_dir, size_t terminal_dir_cap,
                 char *journal_path, size_t journal_path_cap) {
    char resolved[PATH_MAX], root[PATH_MAX], journal_root[PATH_MAX];
    uint64_t hp, hl;
    int e;
    if (!project || !*project || !lane || !*lane || !safe_session_id(session))
        return EINVAL;
    if (!realpath(project, resolved)) return errno;
    if (strlen(resolved) + 1 > canonical_cap) return ENAMETOOLONG;
    strcpy(canonical, resolved);
    hp = hash_text(UINT64_C(1469598103934665603), resolved);
    hp = hash_separator(hp);
    hl = hash_text(UINT64_C(1469598103934665603), lane);
    e = base_path(root, sizeof(root));
    if (e) return e;
    if (snprintf(terminal_parent, terminal_parent_cap,
            "%s/terminal/%016llx/%016llx", root,
            (unsigned long long)hp, (unsigned long long)hl) >=
        (int)terminal_parent_cap) return ENAMETOOLONG;
    if (snprintf(terminal_dir, terminal_dir_cap, "%s/%s", terminal_parent,
            session) >= (int)terminal_dir_cap) return ENAMETOOLONG;
    if (snprintf(journal_root, sizeof(journal_root),
            "%s/session-journals/%016llx/%016llx/%s", root,
            (unsigned long long)hp, (unsigned long long)hl, session) >=
        (int)sizeof(journal_root)) return ENAMETOOLONG;
    if (snprintf(journal_path, journal_path_cap, "%s/journal.jsonl",
            journal_root) >= (int)journal_path_cap) return ENAMETOOLONG;
    e = make_dirs(terminal_parent);
    if (e) return e;
    return make_dirs(journal_root);
}

static int write_all(int fd, const char *data, size_t length) {
    while (length) {
        ssize_t n = write(fd, data, length);
        if (n < 0) {
            if (errno == EINTR) continue;
            return errno;
        }
        data += n;
        length -= (size_t)n;
    }
    return 0;
}

static int write_file(const char *dir, const char *name,
                      const char *data, size_t length) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_CREAT | O_EXCL | O_WRONLY | O_CLOEXEC, 0600);
    if (fd < 0) return errno;
    int e = write_all(fd, data, length);
    if (e == 0 && fsync(fd) != 0) e = errno;
    if (close(fd) != 0 && e == 0) e = errno;
    return e;
}

static int sync_dir(const char *path) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    int e = fsync(fd) == 0 ? 0 : errno;
    close(fd);
    return e;
}

static int terminal_identity(const char *dir, char *identity, size_t cap) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/identity", dir) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    ssize_t n = read(fd, identity, cap - 1);
    int e = n < 0 ? errno : 0;
    if (close(fd) != 0 && e == 0) e = errno;
    if (e) return e;
    identity[n] = '\0';
    return 0;
}

static int read_file(const char *dir, const char *name, char *out, size_t cap) {
    char path[PATH_MAX];
    if (snprintf(path, sizeof(path), "%s/%s", dir, name) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return errno;
    size_t used = 0;
    int e = 0;
    while (used < cap - 1) {
        ssize_t n = read(fd, out + used, cap - 1 - used);
        if (n < 0) {
            if (errno == EINTR) continue;
            e = errno;
            break;
        }
        if (n == 0) break;
        used += (size_t)n;
    }
    if (e == 0 && used == cap - 1) {
        char extra;
        ssize_t n = read(fd, &extra, 1);
        if (n != 0) e = n < 0 ? errno : EOVERFLOW;
    }
    if (close(fd) != 0 && e == 0) e = errno;
    if (e == 0) out[used] = '\0';
    return e;
}

int fo_gremlin_terminal_journal_path(const char *project, const char *lane,
        const char *session, char *out, int cap) {
    char canonical[PATH_MAX], parent[PATH_MAX], terminal[PATH_MAX], journal[PATH_MAX];
    if (cap <= 0) return EINVAL;
    int e = paths(project, lane, session, canonical, sizeof(canonical),
                  parent, sizeof(parent), terminal, sizeof(terminal),
                  journal, sizeof(journal));
    if (e) return e;
    if (strlen(journal) + 1 > (size_t)cap) return ENAMETOOLONG;
    strcpy(out, journal);
    return 0;
}

int fo_gremlin_terminal_publish(const char *project, const char *lane,
        const char *session, const char *status, int status_len) {
    char canonical[PATH_MAX], parent[PATH_MAX], final[PATH_MAX], journal[PATH_MAX];
    char identity[PATH_MAX + 512], old_identity[PATH_MAX + 512];
    char stage[PATH_MAX];
    struct timespec ts;
    if (!status || status_len < 1 || memchr(status, '\0', (size_t)status_len))
        return EINVAL;
    int e = paths(project, lane, session, canonical, sizeof(canonical),
                  parent, sizeof(parent), final, sizeof(final),
                  journal, sizeof(journal));
    if (e) return e;
    int n = snprintf(identity, sizeof(identity), "%s\n%s\n%s\n", canonical,
                     lane, session);
    if (n < 0 || n >= (int)sizeof(identity)) return EOVERFLOW;
    if (lstat(final, &(struct stat){0}) == 0) {
        e = terminal_identity(final, old_identity, sizeof(old_identity));
        return e ? e : (strcmp(identity, old_identity) == 0 ? 0 : EEXIST);
    }
    if (errno != ENOENT) return errno;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) return errno;
    n = snprintf(stage, sizeof(stage), "%s/.%s.%ld.%lld.%09ld.tmp", parent,
        session, (long)getpid(), (long long)ts.tv_sec, ts.tv_nsec);
    if (n < 0 || n >= (int)sizeof(stage)) return ENAMETOOLONG;
    if (mkdir(stage, 0700) != 0) return errno;
    e = write_file(stage, "identity", identity, (size_t)strlen(identity));
    if (e == 0) e = write_file(stage, "status", status, (size_t)status_len);
    if (e == 0) e = write_file(stage, "journal-path", journal, strlen(journal));
    if (e == 0) e = sync_dir(stage);
    if (e == 0 && rename(stage, final) != 0) {
        int rename_error = errno;
        if (rename_error == EEXIST || rename_error == ENOTEMPTY) {
            int read_error = terminal_identity(final, old_identity, sizeof(old_identity));
            if (read_error == 0 && strcmp(identity, old_identity) == 0) e = 0;
            else e = read_error ? read_error : EEXIST;
        } else e = rename_error;
    }
    if (e == 0) e = sync_dir(parent);
    if (access(stage, F_OK) == 0) {
        char path[PATH_MAX];
        const char *names[] = {"identity", "status", "journal-path"};
        for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
            if (snprintf(path, sizeof(path), "%s/%s", stage, names[i]) < (int)sizeof(path))
                unlink(path);
        }
        rmdir(stage);
    }
    return e;
}

int fo_gremlin_terminal_read(const char *project, const char *lane,
        const char *session, char *status, int status_cap,
        char *journal, int journal_cap) {
    char canonical[PATH_MAX], parent[PATH_MAX], terminal[PATH_MAX], expected_journal[PATH_MAX];
    char identity[PATH_MAX + 512], stored[PATH_MAX + 512], stored_journal[PATH_MAX];
    if (status_cap <= 0 || journal_cap <= 0) return EINVAL;
    int e = paths(project, lane, session, canonical, sizeof(canonical),
                  parent, sizeof(parent), terminal, sizeof(terminal),
                  expected_journal, sizeof(expected_journal));
    if (e) return e;
    int n = snprintf(identity, sizeof(identity), "%s\n%s\n%s\n", canonical,
                     lane, session);
    if (n < 0 || n >= (int)sizeof(identity)) return EOVERFLOW;
    e = terminal_identity(terminal, stored, sizeof(stored));
    if (e) return e;
    if (strcmp(identity, stored) != 0) return EEXIST;
    e = read_file(terminal, "status", status, (size_t)status_cap);
    if (e) return e;
    e = read_file(terminal, "journal-path", stored_journal, sizeof(stored_journal));
    if (e) return e;
    if (strcmp(expected_journal, stored_journal) != 0) return EEXIST;
    if (strlen(stored_journal) + 1 > (size_t)journal_cap) return ENAMETOOLONG;
    strcpy(journal, stored_journal);
    return 0;
}
