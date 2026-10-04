/* Native identity and immutable-copy primitives for the running fo image.
   The public argv[0] and PATH are deliberately not consulted. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define FO_DRIVER_DIGEST_LEN 64
static int fo_running_image_fd = -1;

static int fo_valid_digest(const char *digest) {
    int i;
    if (digest == NULL || strlen(digest) != FO_DRIVER_DIGEST_LEN) return 0;
    for (i = 0; i < FO_DRIVER_DIGEST_LEN; i++) {
        if (!((digest[i] >= '0' && digest[i] <= '9') ||
              (digest[i] >= 'a' && digest[i] <= 'f')))
            return 0;
    }
    return 1;
}

#if defined(__APPLE__)
static uint32_t fo_be32(const unsigned char *p) {
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}

static uint64_t fo_be64(const unsigned char *p) {
    return ((uint64_t)fo_be32(p) << 32) | fo_be32(p + 4);
}

static int fo_read_exact(int fd, void *buffer, size_t length, off_t offset) {
    unsigned char *bytes = (unsigned char *)buffer;
    size_t total = 0;
    while (total < length) {
        ssize_t n = pread(fd, bytes + total, length - total,
                          offset + (off_t)total);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        total += (size_t)n;
    }
    return 0;
}

static int fo_uuid_from_commands(const unsigned char *commands, uint32_t ncmds,
                                 uint32_t command_bytes, uint8_t uuid[16]) {
    uint32_t offset = 0, i;
    for (i = 0; i < ncmds; i++) {
        struct load_command command;
        if (offset > command_bytes || command_bytes - offset < sizeof(command))
            return -1;
        memcpy(&command, commands + offset, sizeof(command));
        if (command.cmdsize < sizeof(command) ||
            command.cmdsize > command_bytes - offset)
            return -1;
        if (command.cmd == LC_UUID) {
            struct uuid_command value;
            if (command.cmdsize < sizeof(value)) return -1;
            memcpy(&value, commands + offset, sizeof(value));
            memcpy(uuid, value.uuid, sizeof(value.uuid));
            return 0;
        }
        offset += command.cmdsize;
    }
    return -1;
}

static int fo_file_slice_uuid(int fd, off_t base, cpu_type_t cpu,
                              cpu_subtype_t subtype, uint8_t uuid[16]) {
    struct mach_header_64 header;
    unsigned char *commands;
    int result;
    if (fo_read_exact(fd, &header, sizeof(header), base) != 0 ||
        header.magic != MH_MAGIC_64 || header.cputype != cpu ||
        header.cpusubtype != subtype || header.sizeofcmds > 16 * 1024 * 1024)
        return -1;
    commands = (unsigned char *)malloc(header.sizeofcmds ? header.sizeofcmds : 1);
    if (commands == NULL) return -1;
    result = fo_read_exact(fd, commands, header.sizeofcmds,
                           base + (off_t)sizeof(header));
    if (result == 0)
        result = fo_uuid_from_commands(commands, header.ncmds, header.sizeofcmds,
                                       uuid);
    free(commands);
    return result;
}

static int fo_file_main_uuid(int fd, cpu_type_t cpu, cpu_subtype_t subtype,
                             uint8_t uuid[16]) {
    unsigned char header[8], entry[32];
    uint32_t magic, count, i;
    if (fo_read_exact(fd, header, sizeof(header), 0) != 0) return -1;
    if (memcmp(header, "\xcf\xfa\xed\xfe", 4) == 0)
        return fo_file_slice_uuid(fd, 0, cpu, subtype, uuid);
    if (memcmp(header, "\xca\xfe\xba\xbe", 4) != 0 &&
        memcmp(header, "\xca\xfe\xba\xbf", 4) != 0)
        return -1;
    magic = fo_be32(header);
    count = fo_be32(header + 4);
    if (count == 0 || count > 4096) return -1;
    for (i = 0; i < count; i++) {
        uint32_t entry_size = magic == FAT_MAGIC_64 ? 32 : 20;
        uint32_t entry_cpu, entry_subtype;
        uint64_t offset;
        if (fo_read_exact(fd, entry, entry_size,
                (off_t)sizeof(header) + (off_t)i * entry_size) != 0)
            return -1;
        entry_cpu = fo_be32(entry);
        entry_subtype = fo_be32(entry + 4);
        if (entry_cpu != (uint32_t)cpu || entry_subtype != (uint32_t)subtype)
            continue;
        offset = magic == FAT_MAGIC_64 ? fo_be64(entry + 8) : fo_be32(entry + 8);
        if (offset > INT64_MAX) return -1;
        return fo_file_slice_uuid(fd, (off_t)offset, cpu, subtype, uuid);
    }
    return -1;
}

static int fo_matches_loaded_macos_image(int fd) {
    const struct mach_header *base = _dyld_get_image_header(0);
    const unsigned char *commands;
    const struct mach_header_64 *header64;
    uint8_t loaded_uuid[16], file_uuid[16];
    if (base == NULL || base->magic != MH_MAGIC_64) return 0;
    header64 = (const struct mach_header_64 *)base;
    if (header64->sizeofcmds > 16 * 1024 * 1024) return 0;
    commands = (const unsigned char *)(header64 + 1);
    if (fo_uuid_from_commands(commands, header64->ncmds, header64->sizeofcmds,
                              loaded_uuid) != 0 ||
        fo_file_main_uuid(fd, header64->cputype, header64->cpusubtype,
                          file_uuid) != 0)
        return 0;
    return memcmp(loaded_uuid, file_uuid, sizeof(loaded_uuid)) == 0;
}
#endif

static int fo_open_running_image(void) {
#if defined(__linux__)
    return open("/proc/self/exe", O_RDONLY | O_CLOEXEC);
#elif defined(__APPLE__)
    char local_path[PATH_MAX];
    uint32_t path_size = (uint32_t)sizeof(local_path);
    char *path = NULL;
    char resolved[PATH_MAX];
    int fd = -1;
    int allocated = 0;

    path = local_path;
    if (_NSGetExecutablePath(path, &path_size) != 0) {
        if (path_size <= sizeof(local_path) || path_size > 1024 * 1024)
            return -1;
        path = (char *)malloc((size_t)path_size);
        if (path == NULL) return -1;
        allocated = 1;
        if (_NSGetExecutablePath(path, &path_size) != 0) {
            free(path);
            return -1;
        }
    }
    if (realpath(path, resolved) != NULL)
        fd = open(resolved, O_RDONLY | O_CLOEXEC);
    if (allocated) free(path);
    /* Public dyld APIs expose the main image UUID, not its original vnode.
       Reject a path that now resolves to a different Mach-O image. If the path
       changes before this constructor opens it, the old vnode is unavailable;
       that startup window fails closed when the replacement UUID differs. The
       constructor-held fd protects later replacements. */
    if (fd >= 0 && !fo_matches_loaded_macos_image(fd)) {
        close(fd);
        errno = ESTALE;
        fd = -1;
    }
    return fd;
#else
    errno = ENOTSUP;
    return -1;
#endif
}

int fo_c_driver_image_init(void) {
    struct stat st;
    int fd;

    if (fo_running_image_fd >= 0) return 0;
    fd = fo_open_running_image();
    if (fd < 0) return -1;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) ||
        (st.st_mode & 0111) == 0) {
        close(fd);
        errno = ENOEXEC;
        return -1;
    }
    fo_running_image_fd = fd;
    return 0;
}

/* Run before the Fortran main program so macOS also retains the launch-time
   image handle across an atomic replacement of the public pathname. */
#if defined(__GNUC__) || defined(__clang__)
__attribute__((constructor))
static void fo_driver_capture_at_startup(void) {
    (void)fo_c_driver_image_init();
}
#endif

static int fo_open_private_root(const char *root) {
    struct stat st;
    int fd;

    if (root == NULL || root[0] == '\0') return -1;
    if (mkdir(root, 0700) != 0 && errno != EEXIST) return -1;
    if (lstat(root, &st) != 0 || !S_ISDIR(st.st_mode) ||
        st.st_uid != geteuid() || (st.st_mode & 0077) != 0) {
        errno = EPERM;
        return -1;
    }
    fd = open(root, O_RDONLY | O_CLOEXEC | O_DIRECTORY | O_NOFOLLOW);
    return fd;
}

static int fo_copy_image_fd(int out_fd) {
    char buffer[131072];
    off_t offset = 0;

    if (fo_c_driver_image_init() != 0) return -1;
    for (;;) {
        ssize_t nread = pread(fo_running_image_fd, buffer, sizeof(buffer), offset);
        ssize_t written = 0;
        if (nread == 0) break;
        if (nread < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        while (written < nread) {
            ssize_t nwrite = write(out_fd, buffer + written,
                                   (size_t)(nread - written));
            if (nwrite < 0) {
                if (errno == EINTR) continue;
                return -1;
            }
            if (nwrite == 0) {
                errno = EIO;
                return -1;
            }
            written += nwrite;
        }
        offset += nread;
    }
    return 0;
}

/* Create a unique owner-only staging file under root, copy the retained image,
   then make the completed bytes executable and read-only. */
int fo_c_driver_stage_copy(const char *root, char *stage_path, int cap) {
    struct timespec now;
    char leaf[96], full[PATH_MAX];
    int root_fd, out_fd = -1, attempt;

    if (stage_path == NULL || cap <= 0) return -1;
    stage_path[0] = '\0';
    root_fd = fo_open_private_root(root);
    if (root_fd < 0) return -1;
    if (clock_gettime(CLOCK_REALTIME, &now) != 0) {
        close(root_fd);
        return -1;
    }
    for (attempt = 0; attempt < 32; attempt++) {
        if (snprintf(leaf, sizeof(leaf), ".stage-%ld-%lld-%d",
                     (long)getpid(), (long long)now.tv_nsec, attempt) >=
            (int)sizeof(leaf))
            break;
        out_fd = openat(root_fd, leaf, O_WRONLY | O_CREAT | O_EXCL |
                        O_CLOEXEC | O_NOFOLLOW, 0600);
        if (out_fd >= 0 || errno != EEXIST) break;
    }
    if (out_fd < 0) {
        close(root_fd);
        return -1;
    }
    if (fo_copy_image_fd(out_fd) != 0 || fchmod(out_fd, 0555) != 0 ||
        fsync(out_fd) != 0) {
        int saved_errno = errno;
        close(out_fd);
        unlinkat(root_fd, leaf, 0);
        close(root_fd);
        errno = saved_errno;
        return -1;
    }
    if (close(out_fd) != 0) {
        int saved_errno = errno;
        unlinkat(root_fd, leaf, 0);
        close(root_fd);
        errno = saved_errno;
        return -1;
    }
    if (snprintf(full, sizeof(full), "%s/%s", root, leaf) >= (int)sizeof(full) ||
        (size_t)strlen(full) + 1 > (size_t)cap) {
        unlinkat(root_fd, leaf, 0);
        close(root_fd);
        errno = ENAMETOOLONG;
        return -1;
    }
    if (fsync(root_fd) != 0) {
        int saved_errno = errno;
        unlinkat(root_fd, leaf, 0);
        close(root_fd);
        errno = saved_errno;
        return -1;
    }
    strcpy(stage_path, full);
    close(root_fd);
    return 0;
}

static int fo_stage_leaf(const char *root, const char *stage, char *leaf,
                         size_t leaf_cap) {
    size_t root_len;
    const char *name;
    if (root == NULL || stage == NULL) return -1;
    root_len = strlen(root);
    if (strncmp(root, stage, root_len) != 0 || stage[root_len] != '/') return -1;
    name = stage + root_len + 1;
    if (name[0] == '\0' || strchr(name, '/') != NULL ||
        strlen(name) + 1 > leaf_cap)
        return -1;
    strcpy(leaf, name);
    return 0;
}

/* Publish with link(2), which cannot replace an existing digest path. Returns
   0 when published, 1 when the digest path already exists, and -1 on error. */
int fo_c_driver_publish(const char *root, const char *stage,
                        const char *digest, char *final_path, int cap) {
    char leaf[PATH_MAX], full[PATH_MAX];
    int root_fd, status = -1;

    if (!fo_valid_digest(digest) || final_path == NULL || cap <= 0 ||
        fo_stage_leaf(root, stage, leaf, sizeof(leaf)) != 0)
        return -1;
    final_path[0] = '\0';
    if (snprintf(full, sizeof(full), "%s/%s", root, digest) >= (int)sizeof(full) ||
        (size_t)strlen(full) + 1 > (size_t)cap)
        return -1;
    root_fd = fo_open_private_root(root);
    if (root_fd < 0) return -1;
    if (linkat(root_fd, leaf, root_fd, digest, 0) == 0) {
        if (unlinkat(root_fd, leaf, 0) == 0 && fsync(root_fd) == 0) {
            strcpy(final_path, full);
            status = 0;
        }
    } else if (errno == EEXIST) {
        strcpy(final_path, full);
        status = 1;
    }
    close(root_fd);
    return status;
}

int fo_c_driver_remove_stage(const char *root, const char *stage) {
    char leaf[PATH_MAX];
    int root_fd, result;

    if (fo_stage_leaf(root, stage, leaf, sizeof(leaf)) != 0) return -1;
    root_fd = fo_open_private_root(root);
    if (root_fd < 0) return -1;
    result = unlinkat(root_fd, leaf, 0);
    if (result != 0 && errno == ENOENT) result = 0;
    if (result == 0 && fsync(root_fd) != 0) result = -1;
    close(root_fd);
    return result;
}

int fo_c_driver_validate_root(const char *root) {
    struct stat st;
    int fd;

    if (root == NULL || root[0] == '\0' || lstat(root, &st) != 0 ||
        !S_ISDIR(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0077) != 0) {
        errno = EPERM;
        return -1;
    }
    fd = open(root, O_RDONLY | O_CLOEXEC | O_DIRECTORY | O_NOFOLLOW);
    if (fd < 0) return -1;
    close(fd);
    return 0;
}

/* Validate that a named pin is an owned regular executable without write
   permission. The Fortran provider independently verifies its SHA-256. */
int fo_c_driver_validate_pin(const char *path, long long *size) {
    struct stat st;
    if (path == NULL || size == NULL || lstat(path, &st) != 0 ||
        !S_ISREG(st.st_mode) || st.st_uid != geteuid() ||
        (st.st_mode & 0222) != 0 || (st.st_mode & 0111) == 0) {
        errno = EPERM;
        return -1;
    }
    *size = (long long)st.st_size;
    return 0;
}
