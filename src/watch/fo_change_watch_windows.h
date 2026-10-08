/* Native Windows directory notifications. Included only by fo_change_watch.c. */
#include "fx_win32.h"
#include <stdint.h>
#include <stddef.h>

#define WW_BYTES 16384
#define WW_PATH 4096
struct ww_entry {
    struct ww_entry *next;
    char *path;
    HANDLE directory, event;
    OVERLAPPED request;
    BY_HANDLE_FILE_INFORMATION identity;
    uint64_t seen;
    int live, outstanding, tree;
    union { unsigned char bytes[WW_BYTES]; DWORD alignment; } raw;
};
struct ww_self {
    char *path;
    WIN32_FILE_ATTRIBUTE_DATA attributes;
    ULONGLONG until;
};
struct win_change_watch {
    HANDLE port;
    struct ww_entry *entries;
    char **roots, *excluded, *pending_base;
    size_t nroots, nself, position, length;
    struct ww_self *self;
    uint64_t epoch;
    int dirty, error;
    ULONGLONG due, maximum;
    char diagnostic[WW_PATH];
    union { unsigned char bytes[WW_BYTES]; DWORD alignment; } pending;
};
static int ww_fail(struct win_change_watch *w, const char *operation,
                   const char *path, DWORD code) {
    fx_win32_errno(code);
    w->error = errno;
    snprintf(w->diagnostic, sizeof(w->diagnostic),
        "%s \"%s\": Windows error %lu (errno %d)", operation,
        path ? path : "", (unsigned long)code, w->error);
    return w->error;
}
static char *ww_normalize(const char *path) {
    size_t i, n;
    char *out = _strdup(path);
    if (!out) return NULL;
    for (i = 0; out[i]; ++i) if (out[i] == '\\') out[i] = '/';
    n = strlen(out);
    while (n > 1 && out[n - 1] == '/' && !(n == 3 && out[1] == ':')) out[--n] = 0;
    return out;
}
static wchar_t *ww_wide(const char *path) {
    wchar_t *wide = fx_win32_wide(path), *extended;
    size_t i, n, skip = 0, prefix = 0;
    const wchar_t *tag = L"";
    if (!wide) return NULL;
    for (i = 0; wide[i]; ++i) if (wide[i] == L'/') wide[i] = L'\\';
    n = wcslen(wide);
    if (n >= 4 && !wcsncmp(wide, L"\\\\?\\", 4)) return wide;
    if (n >= MAX_PATH && n >= 3 && wide[1] == L':') {
        tag = L"\\\\?\\"; prefix = 4;
    } else if (n >= MAX_PATH && n >= 2 && wide[0] == L'\\' && wide[1] == L'\\') {
        tag = L"\\\\?\\UNC\\"; prefix = 8; skip = 2;
    }
    if (!prefix) return wide;
    extended = malloc((n + prefix - skip + 1) * sizeof(*extended));
    if (!extended) { free(wide); SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    wcscpy(extended, tag); wcscpy(extended + prefix, wide + skip);
    free(wide);
    return extended;
}
static int ww_within(const char *path, const char *root) {
    size_t n = strlen(root);
    return n && !_strnicmp(path, root, n) &&
        (!path[n] || root[n - 1] == '/' || path[n] == '/');
}
static int ww_excluded(const char *relative) {
    const char *part = relative;
    while (*part) {
        size_t n = strcspn(part, "/");
        if ((n == 4 && !_strnicmp(part, ".git", n)) ||
            (n == 3 && !_strnicmp(part, ".hg", n)) ||
            (n == 4 && !_strnicmp(part, ".bzr", n)) ||
            (n == 4 && !_strnicmp(part, ".svn", n)) ||
            (n == 8 && !_strnicmp(part, ".gremlin", n)) ||
            (part == relative && n == 5 && !_strnicmp(part, "build", n))) return 1;
        part += n;
        if (*part) ++part;
    }
    return 0;
}
static int ww_relevant(struct win_change_watch *w, const char *path) {
    size_t i;
    int output = w->excluded && *w->excluded && ww_within(path, w->excluded);
    for (i = 0; i < w->nroots; ++i) {
        size_t n = strlen(w->roots[i]);
        if (output && !ww_within(w->roots[i], w->excluded)) continue;
        if (!_stricmp(path, w->roots[i])) return 1;
        if (ww_within(path, w->roots[i])) {
            const char *relative = path + n;
            if (*relative == '/') ++relative;
            if (!ww_excluded(relative)) return 1;
        }
    }
    return 0;
}
static int ww_ancestor(struct win_change_watch *w, const char *path) {
    size_t i;
    for (i = 0; i < w->nroots; ++i) if (ww_within(w->roots[i], path)) return 1;
    return 0;
}
static void ww_dirty(struct win_change_watch *w) {
    ULONGLONG now = GetTickCount64();
    if (!w->dirty) w->maximum = now + 500;
    w->dirty = 1;
    w->due = now + 100;
    if (w->due > w->maximum) w->due = w->maximum;
}
static int ww_same(BY_HANDLE_FILE_INFORMATION *a, BY_HANDLE_FILE_INFORMATION *b) {
    return a->dwVolumeSerialNumber == b->dwVolumeSerialNumber &&
        a->nFileIndexHigh == b->nFileIndexHigh && a->nFileIndexLow == b->nFileIndexLow;
}
static void ww_free_entry(struct ww_entry *entry) {
    if (entry->directory != INVALID_HANDLE_VALUE) CloseHandle(entry->directory);
    if (entry->event) CloseHandle(entry->event);
    free(entry->path); free(entry);
}
static void ww_retire(struct ww_entry *entry) {
    entry->live = 0;
    if (entry->directory == INVALID_HANDLE_VALUE) return;
    if (entry->outstanding) CancelIoEx(entry->directory, &entry->request);
    CloseHandle(entry->directory); entry->directory = INVALID_HANDLE_VALUE;
}
static void ww_collect(struct win_change_watch *w) {
    struct ww_entry **slot = &w->entries;
    while (*slot) {
        struct ww_entry *entry = *slot;
        if (!entry->live && !entry->outstanding) {
            *slot = entry->next; ww_free_entry(entry);
        } else slot = &entry->next;
    }
}
static int ww_read(struct win_change_watch *w, struct ww_entry *entry) {
    DWORD flags = FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME;
    if (entry->tree) flags |= FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_SIZE |
        FILE_NOTIFY_CHANGE_ATTRIBUTES | FILE_NOTIFY_CHANGE_SECURITY;
    memset(&entry->request, 0, sizeof(entry->request));
    entry->request.hEvent = entry->event;
    ResetEvent(entry->event);
    if (!ReadDirectoryChangesW(entry->directory, entry->raw.bytes, WW_BYTES,
                              FALSE, flags, NULL, &entry->request, NULL) &&
        GetLastError() != ERROR_IO_PENDING)
        return ww_fail(w, "subscribe directory notifications", entry->path, GetLastError());
    entry->outstanding = 1;
    return 0;
}
static int ww_attributes(const char *path, WIN32_FILE_ATTRIBUTE_DATA *attributes) {
    wchar_t *wide = ww_wide(path);
    DWORD code;
    int success;
    if (!wide) return 0;
    success = GetFileAttributesExW(wide, GetFileExInfoStandard, attributes);
    code = GetLastError(); free(wide); SetLastError(code);
    return success;
}
static int ww_subscribe(struct win_change_watch *w, const char *path, int tree) {
    struct ww_entry *entry, *fresh;
    wchar_t *wide = ww_wide(path);
    HANDLE directory;
    BY_HANDLE_FILE_INFORMATION identity;
    DWORD code;
    if (!wide) return ww_fail(w, "decode watch directory", path, GetLastError());
    directory = CreateFileW(wide, FILE_LIST_DIRECTORY,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED, NULL);
    code = GetLastError(); free(wide);
    if (directory == INVALID_HANDLE_VALUE) {
        if (code == ERROR_FILE_NOT_FOUND || code == ERROR_PATH_NOT_FOUND ||
            code == ERROR_DIRECTORY) return 0;
        return ww_fail(w, "open watch directory", path, code);
    }
    if (!GetFileInformationByHandle(directory, &identity)) {
        code = GetLastError(); CloseHandle(directory);
        return ww_fail(w, "identify watch directory", path, code);
    }
    if (!(identity.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
        CloseHandle(directory); return 0;
    }
    for (entry = w->entries; entry; entry = entry->next) {
        if (!entry->live || _stricmp(entry->path, path)) continue;
        if (ww_same(&entry->identity, &identity) && (entry->tree || !tree)) {
            entry->seen = w->epoch; CloseHandle(directory); return 0;
        }
        ww_retire(entry);
    }
    fresh = calloc(1, sizeof(*fresh));
    if (!fresh) {
        CloseHandle(directory);
        return ww_fail(w, "allocate watch directory", path, ERROR_NOT_ENOUGH_MEMORY);
    }
    fresh->directory = directory; fresh->identity = identity;
    fresh->path = _strdup(path); fresh->tree = tree; fresh->live = 1;
    fresh->seen = w->epoch;
    fresh->event = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!fresh->path || !fresh->event) {
        code = fresh->path ? GetLastError() : ERROR_NOT_ENOUGH_MEMORY;
        ww_free_entry(fresh);
        return ww_fail(w, "allocate notification request", path, code);
    }
    if (!CreateIoCompletionPort(directory, w->port, (ULONG_PTR)fresh, 0)) {
        code = GetLastError(); ww_free_entry(fresh);
        return ww_fail(w, "attach notification directory", path, code);
    }
    fresh->next = w->entries; w->entries = fresh;
    return ww_read(w, fresh);
}
static int ww_tree(struct win_change_watch *w, const char *path) {
    WIN32_FIND_DATAW info;
    HANDLE search;
    wchar_t *wide, *pattern;
    char child[WW_PATH];
    DWORD code;
    size_t length;
    int error = ww_subscribe(w, path, 1);
    if (error) return error;
    wide = ww_wide(path);
    if (!wide) return ww_fail(w, "decode directory enumeration", path, GetLastError());
    length = wcslen(wide);
    pattern = malloc((length + 3) * sizeof(*pattern));
    if (!pattern) {
        free(wide);
        return ww_fail(w, "allocate directory enumeration", path, ERROR_NOT_ENOUGH_MEMORY);
    }
    wcscpy(pattern, wide); wcscpy(pattern + length, L"\\*"); free(wide);
    search = FindFirstFileW(pattern, &info); code = GetLastError(); free(pattern);
    if (search == INVALID_HANDLE_VALUE) {
        if (code == ERROR_FILE_NOT_FOUND || code == ERROR_PATH_NOT_FOUND ||
            code == ERROR_DIRECTORY) return 0;
        return ww_fail(w, "enumerate watch directory", path, code);
    }
    do {
        char *name;
        if (!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ||
            (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) ||
            !wcscmp(info.cFileName, L".") || !wcscmp(info.cFileName, L"..")) continue;
        name = fx_win32_utf8(info.cFileName);
        if (!name) { error = ww_fail(w, "decode directory entry", path, GetLastError()); break; }
        if (snprintf(child, sizeof(child), "%s/%s", path, name) >= (int)sizeof(child))
            error = ww_fail(w, "watch directory exceeds supported path", path,
                            ERROR_FILENAME_EXCED_RANGE);
        free(name);
        if (error) break;
        if (!ww_relevant(w, child)) continue;
        error = ww_tree(w, child);
        if (error) break;
    } while (FindNextFileW(search, &info));
    code = GetLastError(); FindClose(search);
    if (!error && code != ERROR_NO_MORE_FILES)
        error = ww_fail(w, "continue directory enumeration", path, code);
    return error;
}
static int ww_parent(char *path) {
    char *slash;
    if (strlen(path) == 3 && path[1] == ':' && path[2] == '/') return 0;
    if (path[0] == '/' && path[1] == '/') {
        const char *share = strchr(path + 2, '/');
        if (!share || !strchr(share + 1, '/')) return 0;
    }
    slash = strrchr(path, '/');
    if (!slash) return 0;
    if (slash == path || (slash == path + 2 && path[1] == ':')) slash[1] = 0;
    else *slash = 0;
    return 1;
}
void *fo_change_native_open(int *error) {
    struct win_change_watch *w = calloc(1, sizeof(*w));
    *error = 0;
    if (!w) { *error = ENOMEM; return NULL; }
    w->port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
    if (!w->port) {
        fx_win32_errno(GetLastError()); *error = errno; free(w); return NULL;
    }
    return w;
}
void *fo_change_native_open_diagnostic(int *error, char *diagnostic, int capacity) {
    void *handle = fo_change_native_open(error);
    if (diagnostic && capacity > 0) {
        diagnostic[0] = 0;
        if (!handle) snprintf(diagnostic, (size_t)capacity,
            "open Windows notification completion port: %s (errno %d)",
            strerror(*error), *error);
    }
    return handle;
}
void fo_change_native_diagnostic(void *handle, char *buffer, int capacity) {
    struct win_change_watch *w = handle;
    if (buffer && capacity > 0)
        snprintf(buffer, (size_t)capacity, "%s", w ? w->diagnostic : "");
}
void fo_change_native_clear_roots(void *handle) {
    struct win_change_watch *w = handle;
    size_t i;
    if (!w) return;
    for (i = 0; i < w->nroots; ++i) free(w->roots[i]);
    free(w->roots); w->roots = NULL; w->nroots = 0;
    free(w->excluded); w->excluded = NULL;
    w->error = 0; w->diagnostic[0] = 0;
}
int fo_change_native_exclude_root(void *handle, const char *path) {
    struct win_change_watch *w = handle;
    char *copy;
    if (!w || !path) return EINVAL;
    copy = ww_normalize(path);
    if (!copy) return ww_fail(w, "copy excluded output root", path, ERROR_NOT_ENOUGH_MEMORY);
    free(w->excluded); w->excluded = copy;
    return 0;
}
int fo_change_native_root(void *handle, const char *path) {
    struct win_change_watch *w = handle;
    char **roots, *copy;
    wchar_t *wide;
    size_t i;
    if (!w || !path || !*path) return EINVAL;
    if (strlen(path) >= WW_PATH)
        return ww_fail(w, "declared watch root exceeds supported path", path,
                       ERROR_FILENAME_EXCED_RANGE);
    wide = ww_wide(path);
    if (!wide) return ww_fail(w, "decode declared watch root", path, GetLastError());
    free(wide);
    for (i = 0; i < w->nroots; ++i) if (!_stricmp(path, w->roots[i])) return 0;
    copy = ww_normalize(path);
    roots = realloc(w->roots, (w->nroots + 1) * sizeof(*roots));
    if (!copy || !roots) {
        free(copy);
        if (roots) w->roots = roots;
        return ww_fail(w, "store declared watch root", path, ERROR_NOT_ENOUGH_MEMORY);
    }
    w->roots = roots; w->roots[w->nroots++] = copy;
    return 0;
}
int fo_change_native_reconcile(void *handle) {
    struct win_change_watch *w = handle;
    struct ww_entry *entry;
    size_t i;
    int error, first;
    char parent[WW_PATH];
    if (!w) return EINVAL;
    w->error = 0; w->diagnostic[0] = 0;
    ++w->epoch;
    for (i = 0; i < w->nroots; ++i) {
        strcpy(parent, w->roots[i]); first = 1;
        while (ww_parent(parent)) {
            error = ww_subscribe(w, parent, first); first = 0;
            if (error) return error;
        }
        error = ww_tree(w, w->roots[i]);
        if (error) return error;
    }
    for (entry = w->entries; entry; entry = entry->next)
        if (entry->live && entry->seen != w->epoch) ww_retire(entry);
    ww_collect(w);
    return 0;
}
static int ww_current(struct win_change_watch *w, struct ww_entry *entry) {
    wchar_t *wide = ww_wide(entry->path);
    HANDLE handle;
    BY_HANDLE_FILE_INFORMATION identity;
    DWORD code;
    int same;
    if (!wide) return -ww_fail(w, "decode notification directory", entry->path, GetLastError());
    handle = CreateFileW(wide, FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, NULL);
    code = GetLastError(); free(wide);
    if (handle == INVALID_HANDLE_VALUE) {
        if (code == ERROR_FILE_NOT_FOUND || code == ERROR_PATH_NOT_FOUND) return 0;
        return -ww_fail(w, "check notification directory identity", entry->path, code);
    }
    if (!GetFileInformationByHandle(handle, &identity)) {
        code = GetLastError(); CloseHandle(handle);
        return -ww_fail(w, "read notification directory identity", entry->path, code);
    }
    same = ww_same(&identity, &entry->identity);
    CloseHandle(handle); return same;
}
static int ww_self_event(struct win_change_watch *w, const char *path) {
    WIN32_FILE_ATTRIBUTE_DATA attributes;
    size_t i;
    ULONGLONG now = GetTickCount64();
    for (i = 0; i < w->nself; ++i) {
        struct ww_self *self = w->self + i;
        if (self->until < now || _stricmp(self->path, path)) continue;
        if (!ww_attributes(path, &attributes)) return 0;
        return attributes.nFileSizeHigh == self->attributes.nFileSizeHigh &&
            attributes.nFileSizeLow == self->attributes.nFileSizeLow &&
            !CompareFileTime(&attributes.ftLastWriteTime, &self->attributes.ftLastWriteTime);
    }
    return 0;
}
static int ww_decode(struct win_change_watch *w, char *path, int capacity, int *kind) {
    FILE_NOTIFY_INFORMATION *notice;
    wchar_t name[WW_PATH];
    char *utf8;
    char full[WW_PATH];
    WIN32_FILE_ATTRIBUTE_DATA attributes;
    struct ww_entry *entry;
    size_t available, bytes, offset;
    int directory = 0, relevant;
    if (w->position >= w->length) return 0;
    available = w->length - w->position;
    if (available < offsetof(FILE_NOTIFY_INFORMATION, FileName)) goto malformed;
    notice = (FILE_NOTIFY_INFORMATION *)(w->pending.bytes + w->position);
    bytes = notice->FileNameLength;
    offset = notice->NextEntryOffset;
    if ((bytes & 1) || bytes >= sizeof(name) ||
        bytes > available - offsetof(FILE_NOTIFY_INFORMATION, FileName) ||
        (offset && ((offset & 3) || offset < offsetof(FILE_NOTIFY_INFORMATION, FileName) + bytes ||
                    offset > available))) goto malformed;
    memcpy(name, notice->FileName, bytes); name[bytes / sizeof(wchar_t)] = 0;
    w->position = offset ? w->position + offset : w->length;
    utf8 = fx_win32_utf8(name);
    if (!utf8) return ww_fail(w, "decode directory notification", w->pending_base, GetLastError());
    if (snprintf(full, sizeof(full), "%s/%s", w->pending_base, utf8) >= (int)sizeof(full)) {
        free(utf8); return ww_fail(w, "notification path exceeds supported path", w->pending_base,
                                   ERROR_FILENAME_EXCED_RANGE);
    }
    free(utf8);
    relevant = ww_relevant(w, full);
    if (!relevant && (!ww_ancestor(w, full) || notice->Action == FILE_ACTION_MODIFIED)) return 0;
    if (ww_attributes(full, &attributes)) directory = !!(attributes.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
    else {
        DWORD code = GetLastError();
        if (code != ERROR_FILE_NOT_FOUND && code != ERROR_PATH_NOT_FOUND)
            return ww_fail(w, "inspect notified input", full, code);
    }
    for (entry = w->entries; entry && !directory; entry = entry->next)
        if (entry->live && !_stricmp(entry->path, full)) directory = 1;
    if (directory || (!relevant && ww_ancestor(w, full))) { ww_dirty(w); return 0; }
    if (!relevant) return 0;
    if (notice->Action == FILE_ACTION_MODIFIED && ww_self_event(w, full)) return 0;
    if (notice->Action == FILE_ACTION_ADDED || notice->Action == FILE_ACTION_RENAMED_NEW_NAME) *kind = 2;
    else if (notice->Action == FILE_ACTION_REMOVED || notice->Action == FILE_ACTION_RENAMED_OLD_NAME) *kind = 3;
    else if (notice->Action == FILE_ACTION_MODIFIED) *kind = 1;
    else goto malformed;
    if (strlen(full) >= (size_t)capacity)
        return ww_fail(w, "caller notification buffer too small", full, ERROR_FILENAME_EXCED_RANGE);
    strcpy(path, full); return 0;
malformed:
    w->position = w->length;
    /* An unrepresentable record is notification loss, never proof of quiet. */
    ww_dirty(w); return 0;
}
int fo_change_native_poll(void *handle, int timeout, char *path, int capacity, int *kind) {
    struct win_change_watch *w = handle;
    ULONGLONG deadline, now;
    unsigned count = 0;
    if (!w || !path || capacity < 1 || !kind || timeout < 0) return EINVAL;
    *path = 0; *kind = 0;
    if (w->error) return w->error;
    deadline = GetTickCount64() + (DWORD)timeout;
    do {
        DWORD bytes = 0, wait = 0, code;
        ULONG_PTR key = 0;
        OVERLAPPED *request = NULL;
        struct ww_entry *entry;
        BOOL success;
        int error, current;
        if (w->position < w->length) {
            error = ww_decode(w, path, capacity, kind);
            if (error || *kind) return error;
            continue;
        }
        now = GetTickCount64();
        if (timeout && w->dirty && now >= w->due) {
            error = fo_change_native_reconcile(w);
            if (error) return error;
            w->dirty = 0; *kind = 4; return 0;
        }
        if (timeout && now < deadline) {
            ULONGLONG until = deadline;
            if (w->dirty && w->due < until) until = w->due;
            wait = until > now ? (DWORD)(until - now) : 0;
        }
        success = GetQueuedCompletionStatus(w->port, &bytes, &key, &request, wait);
        code = success ? ERROR_SUCCESS : GetLastError();
        if (!request) {
            if (!success && code != WAIT_TIMEOUT)
                return ww_fail(w, "receive directory notification", "", code);
            if (timeout && w->dirty && GetTickCount64() >= w->due) continue;
            return 0;
        }
        entry = (struct ww_entry *)key;
        entry->outstanding = 0;
        if (!entry->live) { ww_collect(w); continue; }
        if (!success && code != ERROR_NOTIFY_ENUM_DIR && code != ERROR_OPERATION_ABORTED)
            return ww_fail(w, "complete directory notification", entry->path, code);
        current = code == ERROR_OPERATION_ABORTED ? 0 : ww_current(w, entry);
        if (current < 0) return -current;
        if (!current) {
            ww_retire(entry); ww_dirty(w); ww_collect(w); continue;
        }
        if (bytes > WW_BYTES) return ww_fail(w, "oversized directory notification", entry->path,
                                            ERROR_INVALID_DATA);
        free(w->pending_base); w->pending_base = _strdup(entry->path);
        if (!w->pending_base) return ww_fail(w, "copy notification root", entry->path, ERROR_NOT_ENOUGH_MEMORY);
        memcpy(w->pending.bytes, entry->raw.bytes, bytes);
        w->position = 0; w->length = bytes;
        error = ww_read(w, entry);
        if (error) return error;
        if (!bytes || code == ERROR_NOTIFY_ENUM_DIR) ww_dirty(w);
    } while (++count < 128);
    return 0;
}
int fo_change_native_pending(void *handle) {
    struct win_change_watch *w = handle;
    struct ww_entry *entry;
    if (!w || w->dirty || w->error || w->position < w->length) return 1;
    for (entry = w->entries; entry; entry = entry->next)
        if (entry->outstanding && WaitForSingleObject(entry->event, 0) != WAIT_TIMEOUT) return 1;
    return 0;
}
void fo_change_native_self(void *handle, const char *path) {
    struct win_change_watch *w = handle;
    struct ww_self *next;
    WIN32_FILE_ATTRIBUTE_DATA attributes;
    size_t i;
    if (!w || !path || !ww_attributes(path, &attributes)) return;
    for (i = 0; i < w->nself; ++i)
        if (!_stricmp(w->self[i].path, path) || w->self[i].until < GetTickCount64()) break;
    if (i == w->nself) {
        next = realloc(w->self, (w->nself + 1) * sizeof(*next));
        if (!next) return;
        w->self = next; memset(w->self + w->nself++, 0, sizeof(*next));
    }
    next = w->self + i; free(next->path); next->path = ww_normalize(path);
    if (!next->path) { *next = w->self[--w->nself]; return; }
    next->attributes = attributes; next->until = GetTickCount64() + 500;
}
void fo_change_native_close(void *handle) {
    struct win_change_watch *w = handle;
    struct ww_entry *entry;
    size_t i;
    if (!w) return;
    for (entry = w->entries; entry; entry = entry->next) ww_retire(entry);
    for (;;) {
        DWORD bytes;
        ULONG_PTR key;
        OVERLAPPED *request;
        for (entry = w->entries; entry && !entry->outstanding; entry = entry->next) {}
        if (!entry) break;
        GetQueuedCompletionStatus(w->port, &bytes, &key, &request, INFINITE);
        if (request) ((struct ww_entry *)key)->outstanding = 0;
    }
    ww_collect(w); CloseHandle(w->port);
    fo_change_native_clear_roots(w);
    for (i = 0; i < w->nself; ++i) free(w->self[i].path);
    free(w->self); free(w->pending_base); free(w);
}
int fo_change_watch_is_dir(const char *path) {
    WIN32_FILE_ATTRIBUTE_DATA attributes;
    return ww_attributes(path, &attributes) && !!(attributes.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
}
