/* fs_probe: characterise shared-filesystem semantics relevant to a lock-free
 * build cache (publish by temp file + rename, lock-free readers).
 *
 * Usage: fs_probe A DIR | fs_probe B DIR | fs_probe local DIR
 * Roles A and B coordinate only through files named DIR/fsp.* .
 * Output: one "PROPERTY: value (evidence)" line per measured property.
 * Exit: 0 ok, 2 usage, 3 DIR not accessible/writable, 4 peer barrier timeout,
 *       124 global timeout. All fsp.* files are removed on exit and on signals.
 */
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifndef RENAME_NOREPLACE
#define RENAME_NOREPLACE 1
#endif

#define NVIS 20        /* visibility trials */
#define NRACE 200      /* names per exclusivity race */
#define RSZ 4096       /* size of each published version */
#define NSTABLE 100    /* files that exist throughout the readdir test */
#define ULSZ (4 << 20) /* size of the file unlinked while open */
#define BAR_MS 20000   /* barrier timeout */
#define HELLO_MS 60000 /* first barrier: hosts start at about the same time */
#define ROLE_ALARM 100 /* seconds, per role */

static char D[PATH_MAX];
static char R = 'A', O = 'B';
static char pathbuf[8][4096];

static double now_ms(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1e3 + t.tv_nsec / 1e6;
}

static void nap(long us)
{
    struct timespec t = {us / 1000000, (us % 1000000) * 1000};
    nanosleep(&t, NULL);
}

/* Build DIR/fsp.<fmt> into slot k (8 rotating slots). */
static char *P(int k, const char *fmt, ...)
{
    char *b = pathbuf[k & 7];
    int n = snprintf(b, 4096, "%s/fsp.", D);
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(b + n, 4096 - n, fmt, ap);
    va_end(ap);
    return b;
}

static void cleanup_all(void)
{
    char sub[4200], q[4600];
    DIR *d;
    struct dirent *e;
    int tries = 0;
    snprintf(sub, sizeof sub, "%s/fsp.sub", D);
    while (!(d = opendir(sub)) && errno != ENOENT && ++tries < 20) nap(50000);
    if (d) {
        while ((e = readdir(d)))
            if (e->d_name[0] != '.') {
                snprintf(q, sizeof q, "%s/%s", sub, e->d_name);
                unlink(q);
            }
        closedir(d);
        rmdir(sub);
    }
    for (int pass = 0; pass < 5; pass++) {
        int removed = 0;
        tries = 0;
        while (!(d = opendir(D)) && errno != ENOENT && ++tries < 20) nap(50000);
        if (!d) return; /* transient server errors (seen: EREMOTEIO) are retried */
        while ((e = readdir(d)))
            if (!strncmp(e->d_name, "fsp.", 4)) {
                snprintf(q, sizeof q, "%s/%s", D, e->d_name);
                if (unlink(q) == 0) removed++;
            }
        closedir(d);
        if (!removed) break;
    }
}

static void on_sig(int s)
{
    cleanup_all();
    _exit(s == SIGALRM ? 124 : 128 + s);
}

static int mkfile(const char *p, const void *buf, size_t n)
{
    int fd = open(p, O_CREAT | O_TRUNC | O_WRONLY, 0644);
    if (fd < 0) return -errno;
    ssize_t w = write(fd, buf, n);
    int e = (w == (ssize_t)n) ? 0 : (w < 0 ? -errno : -EIO);
    if (close(fd) != 0 && !e) e = -errno;
    return e;
}

/* Atomic publish: write tmp, rename over p. */
static int put(const char *p, const void *buf, size_t n)
{
    char t[4200];
    snprintf(t, sizeof t, "%s.tmp%c", p, R);
    int e = mkfile(t, buf, n);
    if (e) return e;
    return rename(t, p) ? -errno : 0;
}

static long get(const char *p, char *buf, size_t max)
{
    int fd = open(p, O_RDONLY);
    if (fd < 0) return -errno;
    size_t n = 0;
    ssize_t r;
    while (n < max && (r = read(fd, buf + n, max - n)) > 0) n += r;
    long res = (r < 0) ? -errno : (long)n;
    close(fd);
    return res;
}

static int exists(const char *p)
{
    int fd = open(p, O_RDONLY);
    if (fd >= 0) {
        close(fd);
        return 1;
    }
    return errno == ENOENT ? 0 : -errno;
}

/* Returns elapsed ms until p exists (want=1) or is gone (want=0), -1 on timeout. */
static double wait_state(const char *p, int want, double tmo)
{
    double t0 = now_ms();
    for (;;) {
        int s = exists(p);
        if ((want && s == 1) || (!want && s == 0)) return now_ms() - t0;
        if (now_ms() - t0 > tmo) return -1;
        nap(500);
    }
}

static void barrier_t(const char *name, double tmo)
{
    mkfile(P(6, "bar.%s.%c", name, R), "b", 1);
    if (wait_state(P(7, "bar.%s.%c", name, O), 1, tmo) < 0) {
        printf("FSPROBE_ERROR: role %c: barrier '%s' timed out after %.0f ms "
               "(peer not running or its files not visible)\n", R, name, tmo);
        fflush(stdout);
        cleanup_all();
        exit(4);
    }
}
static void barrier(const char *name) { barrier_t(name, BAR_MS); }

static void check_access(void)
{
    struct stat st;
    const char *why = NULL;
    int e = 0;
    if (stat(D, &st) != 0) why = "stat", e = errno;
    else if (!S_ISDIR(st.st_mode)) why = "not a directory", e = ENOTDIR;
    else {
        int fd = open(P(0, "access.%c.%d", R, (int)getpid()),
                      O_CREAT | O_EXCL | O_WRONLY, 0644);
        if (fd < 0) why = "create", e = errno;
        else {
            close(fd);
            unlink(pathbuf[0]);
        }
    }
    if (why) {
        printf("FSPROBE_ERROR: DIR not accessible/writable: %s: %s failed: %s%s\n", D,
               why, strerror(e),
               (e == EACCES || e == EPERM) ? " (missing Kerberos/AFS token?)" : "");
        exit(3);
    }
}

static void print_mount(void)
{
    FILE *f = fopen("/proc/self/mountinfo", "r");
    char line[4096], best[4096] = "", mp[2048], opts[1024], fs[256];
    size_t bestlen = 0;
    if (!f) return;
    while (fgets(line, sizeof line, f)) {
        char *sep = strstr(line, " - ");
        if (!sep || sscanf(line, "%*s %*s %*s %*s %2047s %1023s", mp, opts) != 2)
            continue;
        size_t l = strlen(mp);
        if (!strncmp(D, mp, l) && (D[l] == '/' || D[l] == 0 || l == 1) && l >= bestlen) {
            if (sscanf(sep + 3, "%255s", fs) != 1) continue;
            bestlen = l;
            snprintf(best, sizeof best, "%s type %s opts %s", mp, fs, opts);
        }
    }
    fclose(f);
    char host[256] = "?";
    gethostname(host, sizeof host);
    printf("MOUNT[%c]: %s (host %s, /proc/self/mountinfo)\n", R, best, host);
}

/* ---- single-host checks (role A) ---- */
static void t_single(void)
{
    int e1, e2;
    mkfile(P(0, "l1"), "x", 1);
    e1 = link(P(0, "l1"), P(1, "l2")) ? errno : 0;
    mkdir(P(2, "sub"), 0755);
    e2 = link(P(0, "l1"), P(3, "sub/l3")) ? errno : 0;
    printf("LINK_SAME_DIR: %s (link(2) returned %s)\n", e1 ? "no" : "yes",
           e1 ? strerror(e1) : "0");
    printf("LINK_CROSS_DIR: %s (link(2) into subdirectory returned %s)\n",
           e2 ? "no" : "yes", e2 ? strerror(e2) : "0");
#ifdef SYS_renameat2
    mkfile(P(0, "rn0a"), "a", 1);
    mkfile(P(1, "rn0b"), "b", 1);
    int r1 = syscall(SYS_renameat2, AT_FDCWD, P(0, "rn0a"), AT_FDCWD, P(1, "rn0b"),
                     RENAME_NOREPLACE) ? errno : 0;
    int r2 = syscall(SYS_renameat2, AT_FDCWD, P(0, "rn0a"), AT_FDCWD, P(2, "rn0c"),
                     RENAME_NOREPLACE) ? errno : 0;
    int ok = (r1 == EEXIST && r2 == 0);
    printf("RENAME_NOREPLACE_SUPPORT: %s (existing target -> %s, new target -> %s)\n",
           ok ? "yes" : "no", r1 ? strerror(r1) : "0 (REPLACED!)",
           r2 ? strerror(r2) : "0");
#else
    printf("RENAME_NOREPLACE_SUPPORT: unknown (SYS_renameat2 not in build headers)\n");
#endif
}

/* ---- visibility round trips ---- */
static void t_vis(void)
{
    double mx[3] = {0}, sum[3] = {0};
    int fail[3] = {0};
    const char *ack[3] = {"ack.c", "ack.r", "ack.d"};
    barrier("vis");
    for (int i = 0; i < NVIS; i++) {
        if (R == 'A') {
            for (int k = 0; k < 3; k++) {
                double t0 = now_ms();
                if (k == 0) mkfile(P(0, "vis.%d", i), "v", 1);
                if (k == 1) {
                    mkfile(P(0, "rtmp.%d", i), "r", 1);
                    rename(P(0, "rtmp.%d", i), P(1, "ren.%d", i));
                }
                if (k == 2) unlink(P(0, "vis.%d", i));
                if (wait_state(P(2, "%s.%d", ack[k], i), 1, 10000) < 0) {
                    fail[k]++;
                    continue;
                }
                double r = now_ms() - t0;
                sum[k] += r;
                if (r > mx[k]) mx[k] = r;
            }
        } else {
            if (wait_state(P(0, "vis.%d", i), 1, 15000) >= 0)
                mkfile(P(1, "ack.c.%d", i), "a", 1);
            if (wait_state(P(0, "ren.%d", i), 1, 15000) >= 0)
                mkfile(P(1, "ack.r.%d", i), "a", 1);
            if (wait_state(P(0, "vis.%d", i), 0, 15000) >= 0)
                mkfile(P(1, "ack.d.%d", i), "a", 1);
        }
    }
    const char *nm[3] = {"VISIBILITY_CREATE_RTT_MS", "VISIBILITY_RENAME_RTT_MS",
                         "VISIBILITY_UNLINK_RTT_MS"};
    if (R == 'A')
        for (int k = 0; k < 3; k++)
            printf("%s: max %.1f mean %.1f timeouts %d (A acts, B polls open() every "
                   "0.5 ms and acks; RTT over %d trials upper-bounds one-way delay)\n",
                   nm[k], mx[k], (NVIS - fail[k]) ? sum[k] / (NVIS - fail[k]) : -1.0,
                   fail[k], NVIS);
}

/* ---- atomic rename replace under concurrent readers ---- */
static void fill_version(char *b, int seq)
{
    memset(b, 'a' + seq % 26, RSZ);
    char h[16];
    snprintf(h, sizeof h, "seq=%08d\n", seq % 100000000);
    memcpy(b, h, 13);
}

static void t_ren(void)
{
    static char b[RSZ + 16], c[RSZ + 16];
    if (R == 'A') {
        fill_version(b, 0);
        put(P(0, "target"), b, RSZ);
    }
    barrier("ren");
    if (R == 'A') {
        int k = 0, errs = 0;
        double t0 = now_ms();
        while (now_ms() - t0 < 3000) {
            fill_version(b, ++k);
            if (put(P(0, "target"), b, RSZ)) errs++;
            nap(200);
        }
        mkfile(P(1, "ren.done"), "d", 1);
        printf("RENAME_WRITER: %d versions published in 3 s, %d errors (A)\n", k, errs);
    } else {
        long reads = 0, enoent = 0, partial = 0, bad = 0, other = 0, changes = 0;
        int lasterr = 0, lastseq = -1;
        double t0 = now_ms();
        while (now_ms() - t0 < 20000) {
            if ((reads & 31) == 0 && exists(P(1, "ren.done")) == 1) break;
            reads++;
            long n = get(P(0, "target"), c, RSZ + 8);
            if (n < 0) {
                if (n == -ENOENT) enoent++;
                else other++, lasterr = (int)-n;
                continue;
            }
            if (n != RSZ) {
                partial++;
                continue;
            }
            int seq = -1;
            if (sscanf(c, "seq=%8d", &seq) != 1 || seq < 0) {
                bad++;
                continue;
            }
            fill_version(b, seq);
            if (memcmp(b, c, RSZ)) {
                bad++;
                continue;
            }
            if (seq != lastseq) changes++, lastseq = seq;
        }
        long v = enoent + partial + bad + other;
        printf("RENAME_ATOMIC_REPLACE: %s (%ld reads by B, %ld distinct versions seen, "
               "ENOENT %ld, partial/empty %ld, mixed content %ld, other errors %ld%s%s)\n",
               v ? "VIOLATED" : "yes", reads, changes, enoent, partial, bad, other,
               other ? " last " : "", other ? strerror(lasterr) : "");
    }
    barrier("ren2");
}

/* ---- exclusivity races: 0 O_EXCL, 1 link, 2 renameat2 NOREPLACE ---- */
static void t_race(int mode)
{
    static const char *nm[3] = {"OEXCL_RACE", "LINK_RACE", "RENAME_NOREPLACE_RACE"};
    static const char *tag[3] = {"ex", "lk", "rx"};
    char claim[NRACE + 1], other[NRACE + 8], bar[16];
    int errs = 0, lasterr = 0;
    if (mode == 1) mkfile(P(0, "lsrc.%c", R), &R, 1);
    if (mode == 2)
        for (int i = 0; i < NRACE; i++) mkfile(P(0, "rs.%c.%d", R, i), &R, 1);
    snprintf(bar, sizeof bar, "%s1", tag[mode]);
    barrier(bar);
    for (int i = 0; i < NRACE; i++) {
        char *dst = P(1, "%s.%d", tag[mode], i);
        int rc, e;
        if (mode == 0) {
            int fd = open(dst, O_CREAT | O_EXCL | O_WRONLY, 0644);
            rc = fd >= 0 ? 0 : -1;
            e = errno;
            if (fd >= 0) {
                if (write(fd, &R, 1) != 1) errs++;
                close(fd);
            }
        } else if (mode == 1) {
            rc = link(P(0, "lsrc.%c", R), dst);
            e = errno;
        } else {
#ifdef SYS_renameat2
            rc = syscall(SYS_renameat2, AT_FDCWD, P(0, "rs.%c.%d", R, i), AT_FDCWD, dst,
                         RENAME_NOREPLACE);
            e = errno;
#else
            rc = -1, e = ENOSYS;
#endif
        }
        claim[i] = rc == 0 ? '1' : (e == EEXIST ? '0' : 'e');
        if (claim[i] == 'e') errs++, lasterr = e;
    }
    put(P(0, "res.%s.%c", tag[mode], R), claim, NRACE);
    snprintf(bar, sizeof bar, "%s2", tag[mode]);
    barrier(bar);
    if (R == 'A') {
        long n = get(P(0, "res.%s.B", tag[mode]), other, NRACE);
        if (n != NRACE) {
            printf("%s: unknown (could not read B's claims: %ld)\n", nm[mode], n);
            return;
        }
        int aw = 0, bw = 0, both = 0, none = 0, wrong = 0, berr = 0;
        for (int i = 0; i < NRACE; i++) {
            int a = claim[i] == '1', b = other[i] == '1';
            aw += a, bw += b, both += a && b, none += !a && !b, berr += other[i] == 'e';
            char own = 0;
            if (get(P(1, "%s.%d", tag[mode], i), &own, 1) == 1 && (a ^ b) &&
                own != (a ? 'A' : 'B'))
                wrong++;
        }
        int ok = !both && !none && !wrong && !errs && !berr;
        printf("%s: %s (A won %d, B won %d, both claimed %d, none %d, content owner != "
               "claimant %d, A errors %d%s%s; B errors %d)\n",
               nm[mode], ok ? "exclusive" : "NOT exclusive/failed", aw, bw, both, none,
               wrong, errs, errs ? " last " : "", errs ? strerror(lasterr) : "",
               berr);
    }
}

/* ---- cross-host locks: 0 flock, 1 fcntl ---- */
static int lockop(int fd, int kind, int on)
{
    if (kind == 0) return flock(fd, (on ? LOCK_EX : LOCK_UN) | LOCK_NB) ? errno : 0;
    struct flock fl = {.l_type = on ? F_WRLCK : F_UNLCK, .l_whence = SEEK_SET};
    return fcntl(fd, F_SETLK, &fl) ? errno : 0;
}

static void t_lock(int kind)
{
    const char *nm = kind ? "FCNTL_SETLK_CROSS" : "FLOCK_CROSS";
    char b1[8], b2[8];
    snprintf(b1, 8, "lk%da", kind);
    snprintf(b2, 8, "lk%db", kind);
    int fd = -1, ea = 0;
    if (R == 'A') {
        fd = open(P(0, "lock.%d", kind), O_CREAT | O_RDWR, 0644);
        ea = fd < 0 ? errno : lockop(fd, kind, 1);
        if (ea) printf("%s: A could not lock: %s\n", nm, strerror(ea));
    }
    barrier(b1);
    if (R == 'B') {
        fd = open(P(0, "lock.%d", kind), O_RDWR);
        int e = fd < 0 ? errno : lockop(fd, kind, 1);
        const char *v = e == 0 ? "NO conflict (lock not seen by B; host-local or no-op)"
                        : (e == EWOULDBLOCK || e == EAGAIN || e == EACCES)
                            ? "conflict (B sees A's lock)"
                            : "error";
        printf("%s: %s (A holds exclusive lock, B non-blocking attempt -> %s)\n", nm, v,
               e ? strerror(e) : "0");
        if (fd >= 0 && !e) lockop(fd, kind, 0);
    }
    barrier(b2);
    if (fd >= 0) close(fd);
}

/* ---- timestamps ---- */
static void t_time(void)
{
    if (R == 'A') {
        struct stat st;
        long long ts[40];
        int fd = open(P(0, "mt"), O_CREAT | O_WRONLY, 0644), nts = 0;
        for (int i = 0; fd >= 0 && i < 40; i++) {
            if (write(fd, "m", 1) != 1 || fstat(fd, &st) != 0) break;
            ts[nts++] = (long long)st.st_mtim.tv_sec * 1000000000LL + st.st_mtim.tv_nsec;
            nap(700);
        }
        if (fd >= 0) close(fd);
        int all_s = 1, all_us = 1, distinct = nts > 0;
        long long mind = -1;
        for (int i = 0; i < nts; i++) {
            if (ts[i] % 1000000000LL) all_s = 0;
            if (ts[i] % 1000LL) all_us = 0;
            if (i && ts[i] != ts[i - 1]) {
                distinct++;
                long long d = ts[i] - ts[i - 1];
                if (mind < 0 || d < mind) mind = d;
            }
        }
        printf("MTIME_GRANULARITY: %s (%d writes 0.7 ms apart: %d distinct mtimes, "
               "min step %.1f us)\n", !nts ? "untested" : all_s ? "1 s" :
               all_us ? "1 us" : "1 ns", nts, distinct, mind / 1e3);
        /* atime: T1 atime 2 d old (> 24 h, older than mtime); T2 atime 60 s old,
         * newer than mtime. relatime updates T1 only, strictatime both, noatime none */
        int upd[2];
        char c;
        for (int t = 0; t < 2; t++) {
            time_t now = time(NULL);
            mkfile(P(1, "at"), "atime", 5);
            struct timespec tv[2] = {{now - (t ? 60 : 2 * 86400), 0},
                                     {now - (t ? 3600 : 3 * 86400), 0}};
            utimensat(AT_FDCWD, P(1, "at"), tv, 0);
            get(P(1, "at"), &c, 1);
            stat(P(1, "at"), &st);
            upd[t] = st.st_atime != tv[0].tv_sec;
        }
        printf("ATIME_ON_READ: %s (old atime updated %s, recent atime updated %s)\n",
               upd[1] ? "strictatime" : upd[0] ? "relatime" : "noatime/not updated",
               upd[0] ? "yes" : "no", upd[1] ? "yes" : "no");
        mkfile(P(2, "ut"), "u", 1);
    }
    barrier("ut1");
    struct timespec tv[2] = {{1000000000, 123456789}, {1000000000, 123456789}};
    if (R == 'B') {
        int e = utimensat(AT_FDCWD, P(2, "ut"), tv, 0) ? errno : 0;
        if (e) printf("UTIMENSAT_BY_PEER: B failed: %s\n", strerror(e));
    }
    barrier("ut2");
    if (R == 'A') {
        struct stat st;
        double t0 = now_ms(), dt = -1;
        while (now_ms() - t0 < 5000) {
            if (stat(P(2, "ut"), &st) == 0 && st.st_mtim.tv_sec == 1000000000) {
                dt = now_ms() - t0;
                break;
            }
            nap(1000);
        }
        if (dt < 0)
            printf("UTIMENSAT_BY_PEER: NOT visible to A within 5 s\n");
        else
            printf("UTIMENSAT_BY_PEER: visible after %.1f ms (B set mtime "
                   "1000000000.123456789, A reads nsec %ld)\n", dt,
                   (long)st.st_mtim.tv_nsec);
    }
}

/* ---- readdir consistency under concurrent create/unlink ---- */
static void t_rd(void)
{
    if (R == 'A')
        for (int i = 0; i < NSTABLE; i++) mkfile(P(0, "rd.s.%03d", i), "s", 1);
    barrier("rd1");
    if (R == 'A') {
        double t0 = now_ms();
        int j = 0;
        while (now_ms() - t0 < 3000) {
            mkfile(P(0, "rd.c.%d", j), "c", 1);
            if (j >= 20) unlink(P(0, "rd.c.%d", j - 20));
            j++;
        }
        for (int k = j - 20; k < j; k++) unlink(P(0, "rd.c.%d", k));
        mkfile(P(1, "rd.done"), "d", 1);
    } else {
        long passes = 0, pmiss = 0, pdup = 0, miss = 0, dup = 0, operr = 0;
        int lasterr = 0;
        double t0 = now_ms();
        int seen[NSTABLE];
        while (now_ms() - t0 < 15000 && exists(P(1, "rd.done")) != 1) {
            DIR *d = opendir(D);
            struct dirent *e;
            int idx, m = 0, u = 0;
            if (!d) {
                operr++, lasterr = errno;
                continue;
            }
            memset(seen, 0, sizeof seen);
            while ((e = readdir(d)))
                if (sscanf(e->d_name, "fsp.rd.s.%3d", &idx) == 1 && idx >= 0 &&
                    idx < NSTABLE)
                    seen[idx]++;
            closedir(d);
            for (int i = 0; i < NSTABLE; i++) m += !seen[i], u += seen[i] > 1;
            passes++, miss += m, dup += u, pmiss += m > 0, pdup += u > 0;
        }
        printf("READDIR_CONSISTENT: %s (%ld full readdir passes by B during A's "
               "create/unlink churn; passes with missing stable entries %ld (total %ld),"
               " with duplicates %ld (total %ld); opendir errors %ld%s%s)\n",
               (pmiss || pdup) ? "NO" : passes ? "yes" : "untested", passes, pmiss, miss,
               pdup, dup, operr, operr ? " last " : "", operr ? strerror(lasterr) : "");
    }
    barrier("rd2");
}

/* ---- read through an open fd while the peer unlinks ---- */
static void t_ul(void)
{
    static unsigned char b[ULSZ];
    for (int i = 0; i < ULSZ; i++) b[i] = (unsigned char)(i % 251);
    if (R == 'A') put(P(0, "ul"), b, ULSZ);
    barrier("ul1");
    int fd = -1;
    size_t got = 0;
    if (R == 'B') {
        fd = open(P(0, "ul"), O_RDONLY);
        if (fd >= 0) got = read(fd, b, 65536) == 65536 ? 65536 : 0;
    }
    barrier("ul2");
    if (R == 'A') unlink(P(0, "ul"));
    barrier("ul3");
    if (R == 'B') {
        int e = 0, bad = 0;
        if (fd < 0) {
            printf("READ_AFTER_PEER_UNLINK: untested (B could not open)\n");
        } else {
            posix_fadvise(fd, 0, 0, POSIX_FADV_DONTNEED);
            static unsigned char c[ULSZ];
            ssize_t r;
            while (got < ULSZ && (r = read(fd, c + got, ULSZ - got)) > 0) got += r;
            if (got < ULSZ && r < 0) e = errno;
            for (size_t i = 65536; i < got; i++)
                if (c[i] != (unsigned char)(i % 251)) bad++;
            close(fd);
            printf("READ_AFTER_PEER_UNLINK: %s (B read %zu/%d bytes via fd opened before"
                   " A's unlink, %d corrupt bytes, error %s)\n",
                   (got == ULSZ && !bad) ? "full content" : "BROKEN", got, ULSZ, bad,
                   e ? strerror(e) : "none");
        }
        double t = wait_state(P(0, "ul"), 0, 5000);
        int s = exists(P(0, "ul"));
        printf("OPEN_AFTER_PEER_UNLINK: %s (B open() after unlink barrier: %s after "
               "%.1f ms)\n", t >= 0 ? "ENOENT" : "still opens",
               s == 0 ? "ENOENT" : s == 1 ? "success" : strerror(-s), t);
    }
    barrier("ul4");
}

static int role_main(void)
{
    signal(SIGTERM, on_sig);
    signal(SIGINT, on_sig);
    signal(SIGALRM, on_sig);
    alarm(ROLE_ALARM);
    check_access();
    print_mount();
    barrier_t("hello", HELLO_MS);
    if (R == 'A') t_single();
    t_vis();
    t_ren();
    for (int m = 0; m < 3; m++) t_race(m);
    t_lock(0);
    t_lock(1);
    t_time();
    t_rd();
    t_ul();
    barrier("end");
    if (R == 'A') cleanup_all(); /* B leaves files for A: A may still poll */
    printf("FSPROBE_DONE: role %c\n", R);
    return 0;
}

static pid_t kids[2];
static void on_parent_sig(int s)
{
    for (int i = 0; i < 2; i++)
        if (kids[i] > 0) kill(kids[i], SIGTERM);
    (void)s;
}

int main(int argc, char **argv)
{
    if (argc != 3 || (strcmp(argv[1], "A") && strcmp(argv[1], "B") &&
                      strcmp(argv[1], "local"))) {
        fprintf(stderr, "usage: %s A|B|local DIR\n", argv[0]);
        return 2;
    }
    if (!realpath(argv[2], D)) {
        printf("FSPROBE_ERROR: DIR not accessible/writable: %s: realpath failed: %s%s\n",
               argv[2], strerror(errno),
               errno == EACCES ? " (missing Kerberos/AFS token?)" : "");
        return 3;
    }
    setvbuf(stdout, NULL, _IOLBF, 0);
    if (strcmp(argv[1], "local")) {
        R = argv[1][0];
        O = R == 'A' ? 'B' : 'A';
        return role_main();
    }
    check_access();
    signal(SIGTERM, on_parent_sig);
    signal(SIGINT, on_parent_sig);
    signal(SIGALRM, on_parent_sig);
    alarm(ROLE_ALARM + 10);
    for (int i = 0; i < 2; i++) {
        kids[i] = fork();
        if (kids[i] == 0) {
            R = i ? 'B' : 'A';
            O = i ? 'A' : 'B';
            return role_main();
        }
    }
    int rc = 0, st;
    for (int i = 0; i < 2; i++) {
        while (waitpid(kids[i], &st, 0) < 0 && errno == EINTR) {}
        int c = WIFEXITED(st) ? WEXITSTATUS(st) : 128 + WTERMSIG(st);
        if (c > rc) rc = c;
    }
    cleanup_all();
    return rc;
}
