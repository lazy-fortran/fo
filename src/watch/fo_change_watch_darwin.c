#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

/* The shared change provider uses FSEvents on Darwin. It watches complete
 * directory trees through one stream rather than opening one kqueue vnode
 * descriptor for every file. The stream is event driven; the owner pumps its
 * CoreFoundation run loop only while waiting in native_poll. */
typedef CFStringRef (*cf_string_create_fn)(CFAllocatorRef, const char *,
    CFStringEncoding);
typedef CFArrayRef (*cf_array_create_fn)(CFAllocatorRef, const void **,
    CFIndex, const CFArrayCallBacks *);
typedef void (*cf_release_fn)(CFTypeRef);
typedef CFRunLoopRef (*cf_runloop_current_fn)(void);
typedef SInt32 (*cf_runloop_run_fn)(CFStringRef, CFTimeInterval, Boolean);
typedef FSEventStreamRef (*fs_create_fn)(CFAllocatorRef, FSEventStreamCallback,
    FSEventStreamContext *, CFArrayRef, FSEventStreamEventId,
    CFTimeInterval, FSEventStreamCreateFlags);
typedef void (*fs_schedule_fn)(FSEventStreamRef, CFRunLoopRef, CFStringRef);
typedef Boolean (*fs_start_fn)(FSEventStreamRef);
typedef void (*fs_action_fn)(FSEventStreamRef);

#define APPLE_EVENT_LIMIT 2048
struct apple_event { char *path; int kind; };
struct change_watch {
    void *core, *services;
    cf_string_create_fn string_create;
    cf_array_create_fn array_create;
    cf_release_fn release;
    cf_runloop_current_fn runloop_current;
    cf_runloop_run_fn runloop_run;
    fs_create_fn stream_create;
    fs_schedule_fn stream_schedule;
    fs_start_fn stream_start;
    fs_action_fn stream_stop, stream_invalidate, stream_release;
    CFStringRef mode;
    CFRunLoopRef runloop;
    FSEventStreamRef stream;
    CFArrayRef stream_paths;
    CFStringRef *stream_strings;
    size_t nstream_strings;
    char **roots;
    size_t nroots, nevents;
    struct apple_event events[APPLE_EVENT_LIMIT];
    int dirty, error;
    long long dirty_deadline, dirty_maximum;
    char diagnostic[PATH_MAX + 192];
};

static long long apple_now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void apple_mark_dirty(struct change_watch *w) {
    long long now = apple_now_ms();
    if (!w->dirty) w->dirty_maximum = now + 500;
    w->dirty = 1;
    w->dirty_deadline = now + 100;
    if (w->dirty_deadline > w->dirty_maximum)
        w->dirty_deadline = w->dirty_maximum;
}

static void apple_error(struct change_watch *w, const char *operation,
    const char *root, int error) {
    w->error = error ? error : EIO;
    snprintf(w->diagnostic, sizeof(w->diagnostic), "%s root '%s': %s (%d)",
        operation, root ? root : "<event stream>", strerror(w->error), w->error);
}

static int apple_symbol(void *library, const char *name, void *target,
    size_t size) {
    void *symbol = dlsym(library, name);
    if (!symbol || size != sizeof(symbol)) return ENOSYS;
    memcpy(target, &symbol, sizeof(symbol));
    return 0;
}

#define APPLE_LOAD(library, name, field) do { \
    if (apple_symbol(library, name, &w->field, sizeof(w->field))) { \
        const char *detail = dlerror(); \
        snprintf(w->diagnostic, sizeof(w->diagnostic), \
            "resolve %s: %s", name, detail ? detail : "dlsym failed"); \
        w->error = ENOSYS; \
        *error = w->error; goto failed_open; \
    } \
} while (0)

static int apple_excluded(const char *path);

static int apple_relevant(const struct change_watch *w, const char *path) {
    size_t i;
    for (i = 0; i < w->nroots; ++i) {
        size_t n = strlen(w->roots[i]);
        if (!strcmp(path, w->roots[i])) return 1;
        if (!strncmp(path, w->roots[i], n) && path[n] == '/' &&
            !apple_excluded(path + n + 1)) return 1;
    }
    return 0;
}

static int apple_excluded(const char *path) {
    const char *part = path;
    while (*part) {
        size_t n = strcspn(part, "/");
        if ((n == 4 && !strncmp(part, ".git", n)) ||
            (n == 3 && !strncmp(part, ".hg", n)) ||
            (n == 4 && !strncmp(part, ".bzr", n)) ||
            (n == 4 && !strncmp(part, ".svn", n)) ||
            (n == 8 && !strncmp(part, ".gremlin", n)) ||
            (part == path && n == 5 && !strncmp(part, "build", n))) return 1;
        part += n;
        if (*part) ++part;
    }
    return 0;
}

static void apple_queue(struct change_watch *w, const char *path, int kind) {
    struct apple_event *event;
    if (kind != 4 && (!apple_relevant(w, path) || apple_excluded(path))) return;
    if (w->nevents == APPLE_EVENT_LIMIT) { apple_mark_dirty(w); return; }
    event = &w->events[w->nevents];
    event->path = strdup(path);
    if (!event->path) { apple_mark_dirty(w); return; }
    event->kind = kind;
    ++w->nevents;
}

static void apple_callback(ConstFSEventStreamRef stream, void *info,
    size_t count, void *raw_paths, const FSEventStreamEventFlags flags[],
    const FSEventStreamEventId ids[]) {
    struct change_watch *w = info;
    char **paths = raw_paths;
    size_t i;
    const FSEventStreamEventFlags lost =
        kFSEventStreamEventFlagMustScanSubDirs |
        kFSEventStreamEventFlagUserDropped |
        kFSEventStreamEventFlagKernelDropped |
        kFSEventStreamEventFlagEventIdsWrapped;
    (void)stream; (void)ids;
    for (i = 0; i < count; ++i) {
        int kind = 0;
        if (flags[i] & lost) { apple_mark_dirty(w); continue; }
        if (!apple_relevant(w, paths[i]) || apple_excluded(paths[i])) continue;
        if (flags[i] & (kFSEventStreamEventFlagRootChanged |
                kFSEventStreamEventFlagMount | kFSEventStreamEventFlagUnmount)) {
            apple_mark_dirty(w);
            continue;
        }
        if (flags[i] & kFSEventStreamEventFlagItemIsDir) {
            apple_mark_dirty(w);
            continue;
        }
        if (flags[i] & kFSEventStreamEventFlagItemRenamed) {
            apple_mark_dirty(w);
            continue;
        }
        if (flags[i] & (kFSEventStreamEventFlagItemCreated |
                kFSEventStreamEventFlagItemRemoved))
            kind = flags[i] & kFSEventStreamEventFlagItemRemoved ? 3 : 2;
        else if (flags[i] & (kFSEventStreamEventFlagItemModified |
                kFSEventStreamEventFlagItemInodeMetaMod |
                kFSEventStreamEventFlagItemXattrMod |
                kFSEventStreamEventFlagItemChangeOwner |
                kFSEventStreamEventFlagItemFinderInfoMod)) kind = 1;
        if (kind) apple_queue(w, paths[i], kind);
        else apple_mark_dirty(w);
    }
}

static void apple_drop_stream(struct change_watch *w) {
    size_t i;
    if (w->stream) {
        w->stream_stop(w->stream);
        w->stream_invalidate(w->stream);
        w->stream_release(w->stream);
        w->stream = NULL;
    }
    if (w->stream_paths) w->release(w->stream_paths);
    w->stream_paths = NULL;
    for (i = 0; i < w->nstream_strings; ++i)
        if (w->stream_strings[i]) w->release(w->stream_strings[i]);
    free(w->stream_strings);
    w->stream_strings = NULL;
    w->nstream_strings = 0;
}

static void apple_discard_events(struct change_watch *w) {
    size_t i;
    for (i = 0; i < w->nevents; ++i) free(w->events[i].path);
    w->nevents = 0;
}

static void *apple_open(int *error, char *diagnostic, int capacity) {
    struct change_watch *w = calloc(1, sizeof(*w));
    if (!w) { *error = ENOMEM; return NULL; }
    w->core = dlopen("/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation",
        RTLD_NOW | RTLD_LOCAL);
    if (!w->core) {
        const char *detail = dlerror();
        snprintf(w->diagnostic, sizeof(w->diagnostic), "load CoreFoundation: %s",
            detail ? detail : "dlopen failed");
        w->error = ENOSYS;
        *error = w->error; goto failed_open;
    }
    w->services = dlopen(
        "/System/Library/Frameworks/CoreServices.framework/CoreServices",
        RTLD_NOW | RTLD_LOCAL);
    if (!w->services) {
        const char *detail = dlerror();
        snprintf(w->diagnostic, sizeof(w->diagnostic), "load CoreServices: %s",
            detail ? detail : "dlopen failed");
        w->error = ENOSYS;
        *error = w->error; goto failed_open;
    }
    APPLE_LOAD(w->core, "CFStringCreateWithCString", string_create);
    APPLE_LOAD(w->core, "CFArrayCreate", array_create);
    APPLE_LOAD(w->core, "CFRelease", release);
    APPLE_LOAD(w->core, "CFRunLoopGetCurrent", runloop_current);
    APPLE_LOAD(w->core, "CFRunLoopRunInMode", runloop_run);
    APPLE_LOAD(w->services, "FSEventStreamCreate", stream_create);
    APPLE_LOAD(w->services, "FSEventStreamScheduleWithRunLoop", stream_schedule);
    APPLE_LOAD(w->services, "FSEventStreamStart", stream_start);
    APPLE_LOAD(w->services, "FSEventStreamStop", stream_stop);
    APPLE_LOAD(w->services, "FSEventStreamInvalidate", stream_invalidate);
    APPLE_LOAD(w->services, "FSEventStreamRelease", stream_release);
    w->mode = w->string_create(NULL, "kCFRunLoopDefaultMode", kCFStringEncodingUTF8);
    w->runloop = w->runloop_current();
    if (!w->mode || !w->runloop) {
        apple_error(w, "initialize event run loop", "<event stream>", EIO);
        *error = w->error; goto failed_open;
    }
    *error = 0;
    return w;
failed_open:
    if (diagnostic && capacity > 0)
        snprintf(diagnostic, (size_t)capacity, "%s", w->diagnostic);
    if (w->mode && w->release) w->release(w->mode);
    if (w->services) dlclose(w->services);
    if (w->core) dlclose(w->core);
    free(w);
    return NULL;
}

void *fo_change_native_open(int *error) {
    return apple_open(error, NULL, 0);
}

void *fo_change_native_open_diagnostic(int *error, char *diagnostic,
    int capacity) {
    return apple_open(error, diagnostic, capacity);
}

void fo_change_native_clear_roots(void *handle) {
    struct change_watch *w = handle;
    size_t i;
    if (!w) return;
    apple_drop_stream(w);
    for (i = 0; i < w->nroots; ++i) free(w->roots[i]);
    free(w->roots);
    w->roots = NULL;
    w->nroots = 0;
}

int fo_change_native_root(void *handle, const char *path) {
    struct change_watch *w = handle;
    char **next;
    size_t i;
    for (i = 0; i < w->nroots; ++i)
        if (!strcmp(w->roots[i], path)) return 0;
    next = realloc(w->roots, (w->nroots + 1) * sizeof(*next));
    if (!next) { apple_error(w, "store declared root", path, ENOMEM); return ENOMEM; }
    w->roots = next;
    next[w->nroots] = strdup(path);
    if (!next[w->nroots]) { apple_error(w, "copy declared root", path, ENOMEM); return ENOMEM; }
    ++w->nroots;
    return 0;
}

int fo_change_native_reconcile(void *handle) {
    struct change_watch *w = handle;
    CFStringRef *strings;
    const void **values;
    char **stream_roots;
    CFArrayRef paths = NULL;
    FSEventStreamContext context = {0, w, NULL, NULL, NULL};
    size_t i, j, used = 0;
    if (!w) return EINVAL;
    apple_drop_stream(w);
    stream_roots = calloc(2 * w->nroots, sizeof(*stream_roots));
    strings = calloc(2 * w->nroots, sizeof(*strings));
    values = calloc(2 * w->nroots, sizeof(*values));
    if (w->nroots && (!stream_roots || !strings || !values)) {
        free(stream_roots); free(strings); free(values);
        apple_error(w, "allocate event roots", "declared roots", ENOMEM);
        return w->error;
    }
    for (i = 0; i < w->nroots; ++i) {
        char *stream_root;
        if (access(w->roots[i], F_OK) == 0) {
            stream_root = strdup(w->roots[i]);
            if (!stream_root) {
                apple_error(w, "copy declared root", w->roots[i], ENOMEM);
                goto failed;
            }
        } else {
            char *parent = strdup(w->roots[i]);
            char *slash = parent ? strrchr(parent, '/') : NULL;
            if (!parent || !slash) {
                free(parent);
                apple_error(w, "resolve parent watch root", w->roots[i], EINVAL);
                goto failed;
            }
            if (slash == parent) slash[1] = 0; else *slash = 0;
            stream_root = parent;
        }
        for (j = 0; j < i; ++j) {
            if (stream_roots[j] && !strcmp(stream_roots[j], stream_root)) {
                free(stream_root);
                stream_root = NULL;
                break;
            }
        }
        stream_roots[i] = stream_root;
    }
    {
        for (i = 0; i < w->nroots; ++i) {
            if (!stream_roots[i]) continue;
            strings[used] = w->string_create(NULL, stream_roots[i],
                kCFStringEncodingUTF8);
            if (!strings[used]) {
                apple_error(w, "create event path", stream_roots[i], ENOMEM);
                goto failed;
            }
            values[used] = strings[used];
            ++used;
        }
        if (used == 0) {
            apple_error(w, "create event root list", "declared roots", EINVAL);
            goto failed;
        }
        paths = w->array_create(NULL, values, (CFIndex)used, NULL);
    }
    if (!paths) {
        apple_error(w, "create event root list", "declared roots", ENOMEM);
        goto failed;
    }
    w->stream = w->stream_create(NULL, apple_callback, &context, paths,
        kFSEventStreamEventIdSinceNow, 0.10,
        kFSEventStreamCreateFlagFileEvents | kFSEventStreamCreateFlagNoDefer |
        kFSEventStreamCreateFlagWatchRoot);
    if (!w->stream) {
        apple_error(w, "create recursive event stream", w->nroots ? w->roots[0] :
            "<no declared roots>", EIO);
        goto failed;
    }
    w->stream_schedule(w->stream, w->runloop, w->mode);
    if (!w->stream_start(w->stream)) {
        apple_error(w, "start recursive event stream", w->nroots ? w->roots[0] :
            "<no declared roots>", EIO);
        apple_drop_stream(w);
        goto failed;
    }
    /* The array has no retain callbacks. Keep both it and its CFStrings alive
     * until the stream has been stopped, invalidated, and released. */
    w->stream_paths = paths;
    w->stream_strings = strings;
    w->nstream_strings = used;
    free(values);
    for (i = 0; i < 2 * w->nroots; ++i) free(stream_roots[i]);
    free(stream_roots);
    w->error = 0;
    w->diagnostic[0] = 0;
    return 0;
failed:
    if (w->stream || w->stream_paths) apple_drop_stream(w);
    if (paths) w->release(paths);
    for (i = 0; i < 2 * w->nroots; ++i) {
        if (strings && strings[i]) w->release(strings[i]);
        free(stream_roots[i]);
    }
    free(stream_roots); free(strings); free(values);
    return w->error ? w->error : EIO;
}

void fo_change_native_diagnostic(void *handle, char *buffer, int capacity) {
    struct change_watch *w = handle;
    if (buffer == NULL || capacity <= 0) return;
    if (!w || !w->diagnostic[0]) { buffer[0] = 0; return; }
    snprintf(buffer, (size_t)capacity, "%s", w->diagnostic);
}

int fo_change_native_poll(void *handle, int timeout, char *path, int capacity,
    int *kind) {
    struct change_watch *w = handle;
    struct apple_event event;
    long long now, wait_ms;
    CFTimeInterval wait_seconds;
    if (!w || capacity <= 0) return EINVAL;
    *path = 0; *kind = 0;
    now = apple_now_ms();
    wait_ms = timeout;
    if (w->dirty) {
        long long until = w->dirty_deadline - now;
        if (until < 0) until = 0;
        if (wait_ms > until) wait_ms = until;
    }
    if (!w->nevents) {
        wait_seconds = (CFTimeInterval)wait_ms / 1000.0;
        (void)w->runloop_run(w->mode, wait_seconds, true);
    }
    if (w->error) return w->error;
    if (w->dirty) {
        now = apple_now_ms();
        if (timeout > 0 &&
            (now >= w->dirty_deadline || now >= w->dirty_maximum)) {
            w->dirty = 0;
            apple_discard_events(w);
            if (fo_change_native_reconcile(w)) return w->error;
            *kind = 4;
            return 0;
        }
        return 0;
    }
    if (!w->nevents) return 0;
    event = w->events[0];
    if (strlen(event.path) >= (size_t)capacity) {
        free(event.path);
        apple_error(w, "copy event path", "<event>", ENAMETOOLONG);
        return w->error;
    }
    strcpy(path, event.path);
    *kind = event.kind;
    free(event.path);
    memmove(w->events, w->events + 1, --w->nevents * sizeof(w->events[0]));
    return 0;
}

int fo_change_native_pending(void *handle) {
    struct change_watch *w = handle;
    return !w || w->dirty || w->nevents != 0;
}

void fo_change_native_self(void *handle, const char *path) {
    (void)handle; (void)path;
}

void fo_change_native_close(void *handle) {
    struct change_watch *w = handle;
    size_t i;
    if (!w) return;
    apple_drop_stream(w);
    for (i = 0; i < w->nevents; ++i) free(w->events[i].path);
    for (i = 0; i < w->nroots; ++i) free(w->roots[i]);
    free(w->roots);
    if (w->mode) w->release(w->mode);
    if (w->services) dlclose(w->services);
    if (w->core) dlclose(w->core);
    free(w);
}
#endif
