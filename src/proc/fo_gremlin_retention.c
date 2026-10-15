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
