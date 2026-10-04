/* The source of an executable pin must identify the running image. */
#ifdef __APPLE__
#define _DARWIN_C_SOURCE 1
#endif
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef __APPLE__
#include <crt_externs.h>
#include <mach-o/dyld.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <limits.h>
#include <dirent.h>

static char image[PATH_MAX], directory[PATH_MAX];
static int image_lock = -1, image_error = ENOTSUP;

static void remove_image(void) {
    char lock[PATH_MAX];
    if (!*directory) return;
    snprintf(lock, sizeof(lock), "%s/lock", directory);
    unlink(image);
    unlink(lock);
    rmdir(directory);
    if (image_lock >= 0) close(image_lock);
    image_lock = -1;
}

/* Only owned, known-shape directories with an unlocked lease are collected.
   A hard cap admits at most 64 bootstrap images for this uid. A killed process
   releases its lease, so the next bootstrap collects its abandoned image. */
static int collect_images(const char *root) {
    DIR *dir = opendir(root);
    struct dirent *entry;
    int retained = 0;
    if (!dir) return errno;
    while ((entry = readdir(dir))) {
        char owned[PATH_MAX], lock[PATH_MAX], executable[PATH_MAX];
        struct stat st;
        int fd;
        if (strncmp(entry->d_name, "image-", 6) != 0) continue;
        if (snprintf(owned, sizeof(owned), "%s/%s", root, entry->d_name) >=
            (int)sizeof(owned)) continue;
        if (lstat(owned, &st) || !S_ISDIR(st.st_mode) || st.st_uid != getuid() ||
            (st.st_mode & 0777) != 0700) continue;
        snprintf(lock, sizeof(lock), "%s/lock", owned);
        snprintf(executable, sizeof(executable), "%s/fo", owned);
        fd = open(lock, O_RDWR | O_NOFOLLOW);
        if (fd < 0) {
            if (errno != ENOENT || rmdir(owned) != 0) retained++;
            continue;
        }
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) {
            unlink(executable);
            unlink(lock);
            if (rmdir(owned) != 0) retained++;
        } else retained++;
        close(fd);
    }
    closedir(dir);
    return retained < 64 ? 0 : ENOSPC;
}

/* Darwin has no /proc/self/exe inode handle. Before main or lane ownership,
   execute a private read-only copy. Thereafter this process identifies that
   exact executing copy, never the public pathname. Replacement before the
   bootstrap can select the new image; no generation exists at that point. */
__attribute__((constructor)) static void bootstrap_image(void) {
    char source[PATH_MAX], root[PATH_MAX], canonical[PATH_MAX];
    char lock[PATH_MAX], fd_text[32];
    char block[65536];
    char **argv = *_NSGetArgv();
    const char *owned = getenv("FO_GREMLIN_BOOTSTRAP_IMAGE");
    const char *descriptor = getenv("FO_GREMLIN_BOOTSTRAP_LOCK");
    struct stat st, held;
    uint32_t capacity = sizeof(source);
    int in = -1, out = -1, error = 0, admission = -1;
    ssize_t n;
    if (!argv[1]) return;
    if (strcmp(argv[1], "mcp-server") != 0) {
        if (strcmp(argv[1], "gremlin") != 0 || !argv[2]) return;
        if (strcmp(argv[2], "start") != 0 && strcmp(argv[2], "run") != 0) return;
    }
    if (_NSGetExecutablePath(source, &capacity)) { image_error = ENAMETOOLONG; return; }
    snprintf(root, sizeof(root), "/var/tmp/fo-gremlin-images-%lu", (unsigned long)getuid());
    if (mkdir(root, 0700) != 0 && errno != EEXIST) { image_error = errno; return; }
    if (lstat(root, &st) || !S_ISDIR(st.st_mode) || st.st_uid != getuid() ||
        (st.st_mode & 0777) != 0700) { image_error = EPERM; return; }
    if (!realpath(root, canonical)) { image_error = errno; return; }
    strcpy(root, canonical);
    if (!realpath(source, canonical)) { image_error = errno; return; }
    strcpy(source, canonical);
    if (owned && descriptor && strcmp(source, owned) == 0 &&
        strncmp(owned, root, strlen(root)) == 0 && owned[strlen(root)] == '/') {
        char *end;
        long fd = strtol(descriptor, &end, 10);
        if (*end || fd < 0 || fd > INT_MAX) { image_error = EPERM; return; }
        snprintf(directory, sizeof(directory), "%s", owned);
        char *name = strrchr(directory, '/');
        if (!name || strcmp(name, "/fo")) { image_error = EPERM; return; }
        *name = '\0';
        snprintf(lock, sizeof(lock), "%s/lock", directory);
        if (fstat((int)fd, &held) || lstat(lock, &st) || held.st_ino != st.st_ino ||
            held.st_dev != st.st_dev || held.st_uid != getuid() ||
            lstat(owned, &st) || !S_ISREG(st.st_mode) || st.st_uid != getuid() ||
            (st.st_mode & 0777) != 0500) { *directory = '\0'; image_error = EPERM; return; }
        snprintf(image, sizeof(image), "%s", owned);
        image_lock = (int)fd;
        if (fcntl(image_lock, F_SETFD, FD_CLOEXEC)) { image_error = errno; return; }
        image_error = 0;
        atexit(remove_image);
        return;
    }
    snprintf(lock, sizeof(lock), "%s/admission", root);
    admission = open(lock, O_RDWR | O_CREAT | O_NOFOLLOW, 0600);
    if (admission < 0) { image_error = errno; return; }
    if (flock(admission, LOCK_EX)) { image_error = errno; close(admission); return; }
    error = collect_images(root);
    if (error) { image_error = error; close(admission); return; }
    in = open(source, O_RDONLY);
    if (in < 0) { image_error = errno; close(admission); return; }
    snprintf(directory, sizeof(directory), "%s/image-XXXXXX", root);
    if (!mkdtemp(directory)) {
        image_error = errno; close(in); close(admission); return;
    }
    snprintf(image, sizeof(image), "%s/fo", directory);
    snprintf(lock, sizeof(lock), "%s/lock", directory);
    image_lock = open(lock, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (image_lock < 0 || flock(image_lock, LOCK_EX | LOCK_NB)) error = errno;
    close(admission);
    if (!error) out = open(image, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (!error && out < 0) error = errno;
    while (!error && (n = read(in, block, sizeof(block))) != 0) {
        if (n < 0) { if (errno == EINTR) continue; error = errno; break; }
        ssize_t offset = 0;
        while (offset < n) {
            ssize_t written = write(out, block + offset, (size_t)(n - offset));
            if (written < 0 && errno == EINTR) continue;
            if (written <= 0) { error = errno ? errno : EIO; break; }
            offset += written;
        }
    }
    if (!error && (fsync(out) || fchmod(out, 0500))) error = errno;
    close(in);
    if (out >= 0 && close(out) && !error) error = errno;
    snprintf(fd_text, sizeof(fd_text), "%d", image_lock);
    if (!error && (setenv("FO_GREMLIN_BOOTSTRAP_IMAGE", image, 1) ||
        setenv("FO_GREMLIN_BOOTSTRAP_LOCK", fd_text, 1))) error = errno;
    if (!error) execv(image, argv);
    if (!error) error = errno;
    remove_image();
    image_error = error;
}
#endif

int fo_c_running_executable(char *path, int capacity) {
#ifdef __linux__
    const char *source = "/proc/self/exe";
#elif defined(__APPLE__)
    if (image_error) return image_error;
    const char *source = image;
#else
    (void)path; (void)capacity;
    return ENOTSUP;
#endif
#if defined(__linux__) || defined(__APPLE__)
    if (capacity <= (int)strlen(source)) return ENAMETOOLONG;
    strcpy(path, source);
    return access(source, R_OK) == 0 ? 0 : errno;
#endif
}
