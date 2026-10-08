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

int fo_c_process_scope_identity(const char *, int, const char *, char *, int);
int fo_c_process_scope_drained(const char *, int, const char *, const char *);

/* A delegated authority records the exact ancestor incarnation while the
   caller's kernel membership is verified. Recovery cannot invent ancestry from
   a dead process's parent PID or an empty/missing job. */
struct admission_ancestor {
    int pid;
    char start[64], job[128], scope[PATH_MAX];
};
static int admission_write_ancestor(const char *scope, const char *parent_scope,
                                    const struct admission_owner *parent) {
    struct admission_ancestor ancestor;
    char record[PATH_MAX + 224];
    if (!fo_c_process_owned_by_scope(parent->state, parent->pid, parent->start))
        return EIO;
    if (!realpath(parent_scope, ancestor.scope)) return errno;
    int error = fo_c_process_scope_identity(parent->state, parent->pid,
        parent->start, ancestor.job, sizeof(ancestor.job));
    if (error) return error;
    int n = snprintf(record, sizeof(record), "%d %s %s\n%s\n",
        parent->pid, parent->start, ancestor.job, ancestor.scope);
    if (n < 0 || n >= (int)sizeof(record)) return ENAMETOOLONG;
    return atomic_write_file(scope, "ancestor", record, (size_t)n);
}
static int admission_read_ancestor(const char *scope, struct admission_ancestor *ancestor) {
    char path[PATH_MAX], record[PATH_MAX + 224], extra;
    if (snprintf(path, sizeof(path), "%s/ancestor", scope) >= (int)sizeof(path))
        return ENAMETOOLONG;
    int fd = open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) return errno;
    int error = fo_private_fd(fd) ? 0 : EPERM;
    ssize_t count = error ? -1 : read(fd, record, sizeof(record)-1);
    if (count < 0 && !error) error = errno;
    close(fd);
    if (error) return error;
    if (count <= 0 || count >= (ssize_t)sizeof(record)-1) return EINVAL;
    record[count] = '\0';
    char *directory = strchr(record, '\n');
    if (!directory) return EINVAL;
    *directory++ = '\0';
    if (sscanf(record, "%d %63s %127s %c", &ancestor->pid, ancestor->start,
               ancestor->job, &extra) != 3 || ancestor->pid <= 0) return EINVAL;
    size_t n = strcspn(directory, "\n");
    if (!n || n >= sizeof(ancestor->scope) || directory[n] != '\n' || directory[n+1])
        return EINVAL;
    memcpy(ancestor->scope, directory, n); ancestor->scope[n] = '\0';
    return 0;
}

struct admission_record {
    char *scope;
    struct admission_owner owner;
    struct admission_ancestor ancestor;
    int has_ancestor, status; /* 1 visiting, 2 proven drained, 3 live, 4 removed. */
};
static int admission_resolve_record(struct admission_record *records, size_t count, size_t index) {
    struct admission_record *item = &records[index];
    if (item->status == 2 || item->status == 4) return 0;
    if (item->status == 1) return ELOOP;
    if (fo_gremlin_process_matches(item->owner.pid, item->owner.start)) {
        item->status = 3; return 0;
    }
    item->status = 1;
    if (!strcmp(item->owner.state, "-")) return ENOTSUP;
    int error = fo_c_recover_async_scope(item->owner.state, item->owner.pid, item->owner.start);
    if (!error && fo_c_process_scope_drained(item->owner.state,
            item->owner.pid, item->owner.start, NULL)) {
        item->status = 2; return 0;
    }
    if (!item->has_ancestor) return error ? error : EIO;
    for (size_t parent = 0; parent < count; ++parent) {
        struct admission_record *source = &records[parent];
        if (strcmp(source->scope, item->ancestor.scope)) continue;
        if (source->owner.pid != item->ancestor.pid ||
            strcmp(source->owner.start, item->ancestor.start) ||
            source->owner.kind != item->owner.kind) return EIO;
        error = admission_resolve_record(records, count, parent);
        if (error) return error;
        char identity[128];
        if (fo_c_process_scope_identity(source->owner.state, source->owner.pid,
                source->owner.start, identity, sizeof(identity)) ||
            strcmp(identity, item->ancestor.job)) return EIO;
        if (source->status != 2 && !fo_c_process_scope_drained(source->owner.state,
                source->owner.pid, source->owner.start, item->ancestor.job)) return EIO;
        /* Membership was recorded before this authority/guardian publication;
           that exact ancestor's durable drain covers the nested owner and job. */
        item->status = 2; return 0;
    }
    return EIO;
}

static int admission_recover_records(const char *base) {
    char directory[PATH_MAX], scope[PATH_MAX], path[PATH_MAX], physical[PATH_MAX];
    struct admission_record *records = NULL;
    size_t count = 0;
    struct dirent *entry;
    DIR *scan;
    int error = 0;
    if (snprintf(directory, sizeof(directory), "%s/scopes", base) >= (int)sizeof(directory))
        return ENAMETOOLONG;
    scan = opendir(directory);
    if (!scan) return errno == ENOENT ? 0 : errno;
    for (;;) {
        errno = 0; entry = readdir(scan);
        if (!entry) { error = errno; break; }
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (snprintf(scope, sizeof(scope), "%s/%s", directory, entry->d_name) >=
            (int)sizeof(scope) || snprintf(path, sizeof(path), "%s/authority", scope) >=
            (int)sizeof(path)) { error = ENAMETOOLONG; break; }
        int fd = open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
        if (fd < 0) {
            if (errno == ENOENT) continue;
            error = errno; break;
        }
        struct admission_record *grown = realloc(records, (count+1) * sizeof(*records));
        if (!grown) { close(fd); error = ENOMEM; break; }
        records = grown;
        struct admission_record *item = &records[count];
        memset(item, 0, sizeof(*item));
        error = fo_private_fd(fd) ? admission_read(fd, &item->owner) : EPERM;
        close(fd);
        if (!error && !realpath(scope, physical)) error = errno;
        if (!error && !(item->scope = strdup(physical))) error = ENOMEM;
        if (error) break;
        ++count;
        error = admission_read_ancestor(scope, &item->ancestor);
        if (error == ENOENT) error = 0;
        else if (!error) item->has_ancestor = 1;
        if (error) break;
    }
    if (closedir(scan) != 0 && !error) error = errno;
    /* Resolving a parent can kill children that were live earlier in the scan.
       Revisit them with all ancestor authority and receipts still retained. */
    size_t previous;
    do {
        previous = 0;
        for (size_t i = 0; i < count; ++i) previous += records[i].status == 2;
        for (size_t i = 0; i < count && !error; ++i) {
            error = admission_resolve_record(records, count, i);
        }
        size_t proven = 0;
        for (size_t i = 0; i < count; ++i) proven += records[i].status == 2;
        if (proven == previous) break;
    } while (!error);
    /* Delete dependents before their ancestor proof. On any unresolved debt,
       retain the entire graph so the next recovery has the same authority. */
    while (!error) {
        size_t progress = 0, pending = 0;
        for (size_t i = 0; i < count; ++i) {
            if (records[i].status != 2) continue;
            ++pending;
            int dependent = 0;
            for (size_t j = 0; j < count; ++j)
                if (records[j].status != 4 && records[j].has_ancestor &&
                    !strcmp(records[j].ancestor.scope, records[i].scope)) dependent = 1;
            if (dependent) continue;
            if (fo_c_rm_rf(records[i].scope) != 0) { error = errno ? errno : EIO; break; }
            records[i].status = 4; ++progress;
        }
        if (!pending) break;
        if (!progress && !error) error = EIO;
    }
    for (size_t i = 0; i < count; ++i) free(records[i].scope);
    free(records); return error;
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
