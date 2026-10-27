/* Shared POSIX/Windows file, lock, hash and process-identity primitives for the
   Gremlin state, journal, coverage and process C shims. */
#ifndef FO_SYS_H
#define FO_SYS_H
#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

#define FO_FNV1A_SEED UINT64_C(1469598103934665603)

/* FNV-1a over `length` bytes, or over a NUL-terminated string. */
uint64_t fo_fnv1a(uint64_t hash, const void *data, size_t length);
uint64_t fo_fnv1a_text(uint64_t hash, const char *text);

/* The functions below return 0 or an errno value unless stated otherwise. */
int fo_sys_write_all(int fd, const void *data, size_t length);
int fo_sys_sync_dir(const char *dir);
int fo_sys_sync_parent(const char *path);
int fo_sys_mkdirs(const char *path, int mode);
/* Durably replace `path` with `data` through a private temporary sibling. */
int fo_sys_publish_file(const char *path, const void *data, size_t length);

/* Open (creating) and flock a file: returns the descriptor or -errno. */
int fo_sys_lock_file(const char *path, int extra_flags, int mode, int operation);
int fo_sys_lock_fixture(const char *path);
void fo_sys_unlock(int fd);

/* Classify one complete line: 1 same id, 0 other id, -1 not a valid record. */
typedef int (*fo_sys_line_id_fn)(const char *line, size_t length, const char *id);
/* Append `record` under an exclusive lock unless a line with the same id exists.
   0 stored or identical already present; 1 invalid input or file content;
   2 I/O failure; 3 differing record for the id; 4 over-long line.
   `duplicate_result` is returned when the file holds the id twice. */
int fo_sys_append_once(const char *path, const char *id, const char *record,
                       size_t length, size_t line_max, int duplicate_result,
                       fo_sys_line_id_fn line_id);

/* Parent, group, session, start identity and liveness (not zombie) of a process.
   Any output pointer may be NULL; returns 0 or an errno value. */
int fo_sys_process_info(pid_t pid, pid_t *parent, pid_t *group, pid_t *session,
                        uint64_t *start, int *alive);
/* Start identity, or 0 when unavailable. */
uint64_t fo_sys_process_start(pid_t pid);
/* Printable start identity of a live process (ESRCH for a dead or zombie one). */
int fo_sys_identity_text(uint64_t identity, char *out, size_t cap);
int fo_sys_process_start_text(pid_t pid, char *out, size_t cap);

int fo_c_rm_rf(const char *path);
int fo_c_mkdir_p(const char *path);
#endif
