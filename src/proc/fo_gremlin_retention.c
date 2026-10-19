/* Retained Gremlin storage as data: generations, their bytes, pins, live owners,
 * and the per-invocation views. Cheap by construction: a generation is frozen
 * after publication, so its byte count is walked once and remembered; pins and
 * owners are one lstat and one non-blocking lock probe per generation; the
 * transient views directory is walked with a bounded entry budget. */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define RETENTION_MEMO 1024

struct memo_entry {
    char id[128];
    long long bytes;
};

static struct memo_entry memo[RETENTION_MEMO];
static int memo_count;

static int valid_id(const char *id) {
    size_t n = strlen(id), i;
    if (n == 0 || n > 120) return 0;
    for (i = 0; i < n; ++i)
        if (!(id[i] == '-' || id[i] == '_' || (id[i] >= '0' && id[i] <= '9') ||
              (id[i] >= 'a' && id[i] <= 'z') || (id[i] >= 'A' && id[i] <= 'Z')))
            return 0;
    return 1;
}

/* Apparent bytes of regular files below path; budget counts visited entries
 * and goes negative when exhausted. */
static long long walk_bytes(const char *path, long *budget) {
    DIR *dir = opendir(path);
    struct dirent *entry;
    long long total = 0;
    if (dir == NULL) return 0;
    while ((entry = readdir(dir)) != NULL) {
        char child[PATH_MAX];
        struct stat st;
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
        if (*budget <= 0) break;
        --*budget;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) continue;
        if (lstat(child, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) total += walk_bytes(child, budget);
        else if (S_ISREG(st.st_mode)) total += (long long)st.st_size;
    }
    closedir(dir);
    return total;
}

static long long generation_bytes(const char *base, const char *id) {
    char root[PATH_MAX];
    long budget = 10000000;
    long long bytes;
    int i;
    for (i = 0; i < memo_count; ++i)
        if (strcmp(memo[i].id, id) == 0) return memo[i].bytes;
    if (snprintf(root, sizeof(root), "%s/%s", base, id) >= (int)sizeof(root))
        return 0;
    bytes = walk_bytes(root, &budget);
    if (budget > 0 && memo_count < RETENTION_MEMO) {
        strncpy(memo[memo_count].id, id, sizeof(memo[0].id) - 1);
        memo[memo_count].id[sizeof(memo[0].id) - 1] = '\0';
        memo[memo_count].bytes = bytes;
        ++memo_count;
    }
    return bytes;
}

/* A lease holder keeps lease.lock shared. An exclusive probe, taken under the
 * guard so no acquirer can race it, tells whether anyone does. A busy guard
 * counts as owned. */
static int has_owner(const char *sidecar) {
    char guard[PATH_MAX], lease[PATH_MAX];
    int gfd, lfd, owned;
    if (snprintf(guard, sizeof(guard), "%s/guard.lock", sidecar) >=
            (int)sizeof(guard) ||
        snprintf(lease, sizeof(lease), "%s/lease.lock", sidecar) >=
            (int)sizeof(lease)) return 0;
    gfd = open(guard, O_RDWR | O_CLOEXEC);
    if (gfd < 0) return 0;
    if (flock(gfd, LOCK_EX | LOCK_NB) != 0) {
        close(gfd);
        return 1;
    }
    lfd = open(lease, O_RDWR | O_CLOEXEC);
    owned = 0;
    if (lfd >= 0) {
        if (flock(lfd, LOCK_EX | LOCK_NB) != 0) owned = 1;
        else flock(lfd, LOCK_UN);
        close(lfd);
    }
    flock(gfd, LOCK_UN);
    close(gfd);
    return owned;
}

/* out: generations, generation bytes, pinned, owned, views, view bytes,
 * views_exact (1 when the view walk finished inside its budget). */
int fo_c_gremlin_retention(const char *generation_root, const char *views_dir,
                           long long *out) {
    char base[PATH_MAX], state[PATH_MAX];
    const char *slash;
    DIR *dir;
    struct dirent *entry;
    long budget = 200000;
    int i;
    for (i = 0; i < 7; ++i) out[i] = 0;
    out[6] = 1;
    if (generation_root != NULL && generation_root[0] == '/' &&
        (slash = strrchr(generation_root, '/')) != NULL && slash != generation_root &&
        (size_t)(slash - generation_root) < sizeof(base)) {
        memcpy(base, generation_root, (size_t)(slash - generation_root));
        base[slash - generation_root] = '\0';
        if (snprintf(state, sizeof(state), "%s/.state", base) <
            (int)sizeof(state) && (dir = opendir(base)) != NULL) {
            while ((entry = readdir(dir)) != NULL) {
                char sidecar[PATH_MAX], marker[PATH_MAX];
                struct stat st;
                if (!valid_id(entry->d_name) || entry->d_name[0] == '.') continue;
                if (snprintf(sidecar, sizeof(sidecar), "%s/%s", state,
                             entry->d_name) >= (int)sizeof(sidecar)) continue;
                if (snprintf(marker, sizeof(marker), "%s/%s", base,
                             entry->d_name) >= (int)sizeof(marker)) continue;
                if (lstat(marker, &st) != 0 || !S_ISDIR(st.st_mode)) continue;
                ++out[0];
                out[1] += generation_bytes(base, entry->d_name);
                if (snprintf(marker, sizeof(marker), "%s/pinned", sidecar) <
                        (int)sizeof(marker) && lstat(marker, &st) == 0) ++out[2];
                if (has_owner(sidecar)) ++out[3];
            }
            closedir(dir);
        }
    }
    if (views_dir != NULL && views_dir[0] != '\0' &&
        (dir = opendir(views_dir)) != NULL) {
        while ((entry = readdir(dir)) != NULL) {
            char child[PATH_MAX];
            struct stat st;
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;
            if (snprintf(child, sizeof(child), "%s/%s", views_dir, entry->d_name) >=
                (int)sizeof(child)) continue;
            if (lstat(child, &st) != 0 || !S_ISDIR(st.st_mode)) continue;
            ++out[4];
            out[5] += walk_bytes(child, &budget);
        }
        closedir(dir);
    }
    if (budget <= 0) out[6] = 0;
    return 0;
}

/* Lane state is host-local (owner PID/start identities are interpreted by the
 * existing session service). Cache generations may live elsewhere. Never
 * retire a build view of a live lane: completed build views still supply its
 * tests after their disposable scratch has been released. */
extern int fo_gremlin_process_matches(int pid, const char *start);
extern int fo_c_execution_scratch_active(const char *view);
extern int fo_c_execution_scratch_release(const char *view);
extern int fo_c_rm_rf(const char *path);

/* No async record may remain when reclaiming orphan scratch. A record is
 * published before its payload starts and removed only after drainage. Keep
 * every unknown/nonempty registry; do not guess at process records or signal
 * anything. New owners never acquire an earlier owner's execution views. */
static int lane_payload_drained(const char *lane) {
    char path[PATH_MAX], child[PATH_MAX];
    struct dirent *entry, *record;
    struct stat st;
    int drained = 1;
    if (snprintf(path, sizeof(path), "%s/async-processes", lane) >=
        (int)sizeof(path)) return 0;
    DIR *registries = opendir(path);
    if (!registries) return errno == ENOENT;
    for (;;) {
        errno = 0;
        entry = readdir(registries);
        if (!entry) { if (errno) drained = 0; break; }
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) { drained = 0; break; }
        if (lstat(child, &st) != 0) {
            if (errno == ENOENT) continue;
            drained = 0; break;
        }
        if (!S_ISDIR(st.st_mode)) { drained = 0; break; }
        DIR *records = opendir(child);
        if (!records) { drained = 0; break; }
        for (;;) {
            errno = 0;
            record = readdir(records);
            if (!record) { if (errno) drained = 0; break; }
            if (!strcmp(record->d_name, ".") || !strcmp(record->d_name, ".."))
                continue;
            drained = 0; break;
        }
        if (closedir(records) != 0) drained = 0;
        if (!drained) break;
    }
    if (closedir(registries) != 0) drained = 0;
    return drained;
}

static int lane_inactive(const char *lane) {
    char path[PATH_MAX], id[128], start[64];
    int pid;
    FILE *file;
    if (snprintf(path, sizeof(path), "%s/owner", lane) >= (int)sizeof(path))
        return 0;
    file = fopen(path, "r");
    if (!file) return errno == ENOENT && lane_payload_drained(lane);
    int parsed = fscanf(file, "%127s%d%63s", id, &pid, start) == 3;
    int e = ferror(file);
    fclose(file);
    return parsed && !e && !fo_gremlin_process_matches(pid, start) &&
           lane_payload_drained(lane);
}

struct retained_view {
    char path[PATH_MAX], lane[PATH_MAX];
    long long bytes;
    time_t used;
};

static int older_view(const void *left, const void *right) {
    const struct retained_view *a = left, *b = right;
    if (a->used != b->used) return a->used < b->used ? -1 : 1;
    return strcmp(a->path, b->path);
}

static int parent_path(char *path) {
    char *slash = strrchr(path, '/');
    if (!slash || slash == path) return EINVAL;
    *slash = '\0';
    return 0;
}

/* Visit only the existing projects/<project>/<lane>/views layout. Every pending payload record protects all views, including build views an
 * orphan case may still need. Valid scratch of a drained dead lane is retired
 * with its view; malformed references remain protected. A final view is never acquired again by a new owner;
 * each creation uses an exclusive, process/clock/serial-specific name. */
static int collect_views(const char *lane, struct retained_view **items,
                          size_t *count, size_t *capacity, time_t now,
                          int min_age) {
    char views[PATH_MAX];
    DIR *dir;
    struct dirent *entry;
    size_t begin = *count;
    int protected = !lane_inactive(lane), e = 0;
    if (snprintf(views, sizeof(views), "%s/views", lane) >= (int)sizeof(views))
        return ENAMETOOLONG;
    dir = opendir(views);
    if (!dir) return errno == ENOENT ? 0 : errno;
    for (;;) {
        errno = 0;
        entry = readdir(dir);
        if (!entry) { e = errno; break; }
        char path[PATH_MAX];
        struct stat st;
        long budget = 10000000;
        if (strncmp(entry->d_name, "execution-", 10) &&
            strncmp(entry->d_name, ".building-execution-", 20)) continue;
        if (snprintf(path, sizeof(path), "%s/%s", views, entry->d_name) >=
                (int)sizeof(path)) { e = ENAMETOOLONG; break; }
        if (lstat(path, &st)) { if (errno == ENOENT) continue; e = errno; break; }
        if (!S_ISDIR(st.st_mode)) continue;
        if (fo_c_execution_scratch_active(path) < 0) protected = 1;
        if (protected || st.st_mtime > now || now - st.st_mtime < min_age)
            continue;
        if (*count == *capacity) {
            size_t next = *capacity ? *capacity * 2 : 32;
            void *more = realloc(*items, next * sizeof(**items));
            if (!more) { e = ENOMEM; break; }
            *items = more;
            *capacity = next;
        }
        struct retained_view *view = *items + *count;
        strcpy(view->path, path);
        strcpy(view->lane, lane);
        view->bytes = walk_bytes(path, &budget);
        view->used = st.st_mtime;
        if (budget <= 0) { e = EOVERFLOW; break; }
        ++*count;
    }
    if (closedir(dir) && !e) e = errno;
    if (protected) *count = begin;
    return e;
}

/* A soft budget for the whole Gremlin tree, with lock-free eviction of old
 * inactive views. The existing generation pin/lease retirement contract and
 * small durable journals remain authoritative: they may exceed the budget.
 * No session directory, owner lock, journal or generation is removed here. */
int fo_c_gremlin_trim(const char *generation_root, const char *state_dir,
                       long long max_bytes, int period, int min_age) {
    char state[PATH_MAX], cache[PATH_MAX], path[PATH_MAX], projects[PATH_MAX];
    struct stat st;
    struct retained_view *items = NULL;
    size_t count = 0, capacity = 0;
    long budget = 10000000;
    long long total;
    time_t now = time(NULL);
    int e = 0, removed = 0;
    DIR *project_dirs;
    struct dirent *project;
    FILE *stamp;
    if (max_bytes <= 0) return 0;
    if (!state_dir || strlen(state_dir) >= sizeof(state)) return -EINVAL;
    strcpy(state, state_dir);
    for (int i = 0; i < 3; ++i) if (parent_path(state)) return -EINVAL;
    if (snprintf(path, sizeof(path), "%s/.trim-stamp", state) >=
            (int)sizeof(path)) return -ENAMETOOLONG;
    if (!stat(path, &st) && now >= st.st_mtime && now - st.st_mtime < period)
        return 0;
    stamp = fopen(path, "ab");
    if (!stamp) return errno == ENOENT ? 0 : -errno;
    fclose(stamp);
    extern void fo_c_touch_if_older(const char *, int);
    fo_c_touch_if_older(path, 0);
    total = walk_bytes(state, &budget);
    if (budget <= 0) return -EOVERFLOW;
    if (generation_root && *generation_root) {
        if (strlen(generation_root) >= sizeof(cache)) return -ENAMETOOLONG;
        strcpy(cache, generation_root);
        if (parent_path(cache) || parent_path(cache)) return -EINVAL;
        if (strcmp(cache, state)) total += walk_bytes(cache, &budget);
        if (budget <= 0) return -EOVERFLOW;
    }
    if (total <= max_bytes) return 0;
    if (snprintf(projects, sizeof(projects), "%s/projects", state) >=
            (int)sizeof(projects)) return -ENAMETOOLONG;
    project_dirs = opendir(projects);
    if (!project_dirs) return errno == ENOENT ? 0 : -errno;
    while ((project = readdir(project_dirs)) != NULL && !e) {
        char project_path[PATH_MAX];
        DIR *lanes;
        struct dirent *lane;
        if (!valid_id(project->d_name) || project->d_name[0] == '.') continue;
        if (snprintf(project_path, sizeof(project_path), "%s/%s", projects,
                     project->d_name) >= (int)sizeof(project_path)) {
            e = ENAMETOOLONG; break;
        }
        if (lstat(project_path, &st) || !S_ISDIR(st.st_mode)) continue;
        lanes = opendir(project_path);
        if (!lanes) { e = errno; break; }
        while ((lane = readdir(lanes)) != NULL && !e) {
            if (!valid_id(lane->d_name) || lane->d_name[0] == '.') continue;
            if (snprintf(path, sizeof(path), "%s/%s", project_path,
                         lane->d_name) >= (int)sizeof(path)) {
                e = ENAMETOOLONG; break;
            }
            if (lstat(path, &st) || !S_ISDIR(st.st_mode)) continue;
            e = collect_views(path, &items, &count, &capacity, now, min_age);
        }
        if (closedir(lanes) && !e) e = errno;
    }
    if (closedir(project_dirs) && !e) e = errno;
    if (!e && count > 0) qsort(items, count, sizeof(*items), older_view);
    for (size_t i = 0; !e && i < count && total > max_bytes / 5 * 4; ++i) {
        /* Recheck after collection: a replacement owner may have appeared.
         * New owners never reuse an old view. A busy replacement merely
         * postpones eviction, rather than racing its recovery walk. */
        if (!lane_inactive(items[i].lane)) continue;
        int active = fo_c_execution_scratch_active(items[i].path);
        if (active < 0) continue;
        int released = fo_c_execution_scratch_release(items[i].path);
        if (released == ENOENT) continue; /* another evictor won */
        if (released != 0) { e = released; break; }
        if (fo_c_rm_rf(items[i].path)) { if (errno != ENOENT) e = errno ? errno : EIO; }
        else { ++removed; total -= items[i].bytes; }
    }
    free(items);
    return e ? -e : removed;
}
