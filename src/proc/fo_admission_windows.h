/* Native admission retains durable owner records until its exact job drains.
   A user-scoped kernel mutex serializes recovery with every slot allocation;
   byte locks alone are released before crash descendants necessarily exit. */
#ifndef FO_ADMISSION_WINDOWS_H
#define FO_ADMISSION_WINDOWS_H
#include <sddl.h>

static wchar_t *admission_user_sid(void) {
    HANDLE token;
    DWORD count = 0;
    TOKEN_USER *user;
    wchar_t *sid = NULL;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) return NULL;
    GetTokenInformation(token, TokenUser, NULL, 0, &count);
    user = malloc(count);
    if (user && GetTokenInformation(token, TokenUser, user, count, &count))
        ConvertSidToStringSidW(user->User.Sid, &sid);
    free(user);
    CloseHandle(token);
    return sid;
}

static HANDLE admission_machine_gate(void) {
    wchar_t *sid = admission_user_sid();
    wchar_t name[256];
    HANDLE gate;
    if (!sid) { fx_win32_errno(GetLastError()); return NULL; }
    if (swprintf(name, 256, L"Global\\FoGremlinAdmission-%ls", sid) < 0) {
        LocalFree(sid); errno = ENAMETOOLONG; return NULL;
    }
    LocalFree(sid);
    gate = CreateMutexW(NULL, FALSE, name);
    if (!gate) { fx_win32_errno(GetLastError()); return NULL; }
    DWORD wait = WaitForSingleObject(gate, INFINITE);
    if (wait != WAIT_OBJECT_0 && wait != WAIT_ABANDONED) {
        DWORD error = GetLastError(); CloseHandle(gate);
        fx_win32_errno(error); return NULL;
    }
    return gate;
}

static void admission_machine_unlock(HANDLE gate) {
    if (gate) { ReleaseMutex(gate); CloseHandle(gate); }
}

static int admission_recover_records(const char *base) {
    char directory[PATH_MAX], scope[PATH_MAX], path[PATH_MAX];
    struct dirent *entry;
    struct admission_owner owner;
    DIR *scan;
    int error = 0;
    if (snprintf(directory, sizeof(directory), "%s/scopes", base) >= (int)sizeof(directory))
        return ENAMETOOLONG;
    scan = opendir(directory);
    if (!scan) return errno == ENOENT ? 0 : errno;
    for (;;) {
        errno = 0;
        entry = readdir(scan);
        if (!entry) { error = errno; break; }
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (snprintf(scope, sizeof(scope), "%s/%s", directory, entry->d_name) >=
            (int)sizeof(scope) || snprintf(path, sizeof(path), "%s/authority", scope) >=
            (int)sizeof(path)) { error = ENAMETOOLONG; break; }
        int fd = open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
        if (fd < 0) {
            if (errno == ENOENT) continue; /* A crash before publication launched no payload. */
            error = errno; break;
        }
        if (!fo_private_fd(fd)) error = EPERM;
        else error = admission_read(fd, &owner);
        close(fd);
        if (error) break;
        if (fo_gremlin_process_matches(owner.pid, owner.start)) continue;
        /* Records always name the owner's actual job, including standalone
           builds which had no resident process scope before admission. */
        if (!strcmp(owner.state, "-")) { error = ENOTSUP; break; }
        error = fo_c_recover_async_scope(owner.state, owner.pid, owner.start);
        if (error) break;
        if (fo_c_rm_rf(scope) != 0) { error = errno ? errno : EIO; break; }
    }
    if (closedir(scan) != 0 && !error) error = errno;
    return error;
}

struct admission_native_scope {
    int authority;
    void *prior;
    struct admission_native_scope *next;
};
static struct admission_native_scope *admission_native_scopes;
int fo_c_process_push_async_scope(const char *, void **);
int fo_c_process_pop_async_scope(void **);

static int admission_bind_native_scope(const char *scope, int authority) {
    char start[64];
    const char *directory = getenv("FO_GREMLIN_PROCESS_SCOPE_DIR");
    const char *pid = getenv("FO_GREMLIN_PROCESS_SCOPE_PID");
    const char *birth = getenv("FO_GREMLIN_PROCESS_SCOPE_START");
    int error = process_start(getpid(), start, sizeof(start));
    if (error) return error;
    if (directory && *directory && pid && atoi(pid) == getpid() &&
        birth && !strcmp(birth, start)) return 0;
    struct admission_native_scope *item = calloc(1, sizeof(*item));
    if (!item) return ENOMEM;
    item->authority = authority;
    error = fo_c_process_push_async_scope(scope, &item->prior);
    if (error) { free(item); return error; }
    item->next = admission_native_scopes;
    admission_native_scopes = item;
    return 0;
}

static int admission_unbind_native_scope(int authority) {
    struct admission_native_scope **cursor = &admission_native_scopes;
    while (*cursor) {
        struct admission_native_scope *item = *cursor;
        if (item->authority == authority) {
            int error = fo_c_process_pop_async_scope(&item->prior);
            if (error) return error; /* Retain metadata and capacity on incomplete drainage. */
            *cursor = item->next;
            free(item);
            return 0;
        }
        cursor = &item->next;
    }
    return 0;
}
#endif
