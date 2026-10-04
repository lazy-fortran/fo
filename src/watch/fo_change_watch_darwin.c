#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
    char **roots;
    size_t nroots, nevents;
    struct apple_event events[APPLE_EVENT_LIMIT];
    int dirty, error;
    char diagnostic[PATH_MAX + 192];
};

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
        apple_error(w, "resolve framework symbol", name, ENOSYS); \
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
    if (w->nevents == APPLE_EVENT_LIMIT) { w->dirty = 1; return; }
    event = &w->events[w->nevents];
    event->path = strdup(path);
    if (!event->path) { w->dirty = 1; return; }
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
        kFSEventStreamEventFlagEventIdsWrapped |
        kFSEventStreamEventFlagRootChanged |
        kFSEventStreamEventFlagMount | kFSEventStreamEventFlagUnmount;
    (void)stream; (void)ids;
    for (i = 0; i < count; ++i) {
        int kind = 0;
        if (flags[i] & lost) { w->dirty = 1; continue; }
        if (flags[i] & kFSEventStreamEventFlagItemIsDir) {
            w->dirty = 1;
            continue;
        }
        if (flags[i] & kFSEventStreamEventFlagItemRenamed) {
            w->dirty = 1;
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
        else w->dirty = 1;
    }
}

static void apple_drop_stream(struct change_watch *w) {
    if (!w->stream) return;
    w->stream_stop(w->stream);
    w->stream_invalidate(w->stream);
    w->stream_release(w->stream);
    w->stream = NULL;
}

static void apple_clear_events(struct change_watch *w) {
    size_t i;
    for (i = 0; i < w->nevents; ++i) free(w->events[i].path);
    w->nevents = 0;
    w->dirty = 0;
}

static void apple_settle(struct change_watch *w) {
    int attempt;
    for (attempt = 0; attempt < 3; ++attempt)
        (void)w->runloop_run(w->mode, 0.05, true);
    apple_clear_events(w);
}

void *fo_change_native_open(int *error) {
    struct change_watch *w = calloc(1, sizeof(*w));
    if (!w) { *error = ENOMEM; return NULL; }
    w->core = dlopen("/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation",
        RTLD_NOW | RTLD_LOCAL);
    w->services = dlopen("/System/Library/Frameworks/CoreServices.framework/CoreServices",
        RTLD_NOW | RTLD_LOCAL);
    if (!w->core || !w->services) {
        apple_error(w, "load CoreServices FSEvents", "<event stream>", ENOSYS);
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
    if (w->mode && w->release) w->release(w->mode);
    if (w->services) dlclose(w->services);
    if (w->core) dlclose(w->core);
    free(w);
    return NULL;
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
    CFArrayRef paths;
    FSEventStreamContext context = {0, w, NULL, NULL, NULL};
    size_t i, j;
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
        size_t used = 0;
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
    w->release(paths);
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
    /* Finish the stream's bounded startup delivery before capture begins.
     * Those events predate the next input snapshot and are represented by it. */
    for (i = 0; i < 3; ++i)
        (void)w->runloop_run(w->mode, 0.05, true);
    apple_clear_events(w);
    for (i = 0; i < 2 * w->nroots; ++i) {
        if (strings[i]) w->release(strings[i]);
        free(stream_roots[i]);
    }
    free(stream_roots); free(strings); free(values);
    w->error = 0;
    w->diagnostic[0] = 0;
    return 0;
failed:
    for (i = 0; i < 2 * w->nroots; ++i) {
        if (strings[i]) w->release(strings[i]);
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
    if (!w || capacity <= 0) return EINVAL;
    *path = 0; *kind = 0;
    if (!w->nevents && !w->dirty)
        (void)w->runloop_run(w->mode, (CFTimeInterval)timeout / 1000.0, true);
    if (w->error) return w->error;
    if (w->dirty) {
        apple_settle(w);
        if (fo_change_native_reconcile(w)) return w->error;
        apple_settle(w);
        *kind = 4;
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
