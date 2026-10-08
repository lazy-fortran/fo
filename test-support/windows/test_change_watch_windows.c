/* Independent native-Windows behavioral oracle; no repository-state assertions. */
#include "fx_win32.h"
#include <stdio.h>
#include <string.h>
#include <stdint.h>
void *fo_change_native_open(int *);
void fo_change_native_close(void *);
void fo_change_native_clear_roots(void *);
int fo_change_native_exclude_root(void *, const char *);
int fo_change_native_root(void *, const char *);
int fo_change_native_reconcile(void *);
int fo_change_native_poll(void *, int, char *, int, int *);
int fo_change_native_pending(void *);
void fo_change_native_self(void *, const char *);
void fo_change_native_diagnostic(void *, char *, int);
#ifndef WP_HELPERS_ONLY
static char base[1024];
static int failed;
static void check(int ok, const char *claim) {
    printf("%s %s\n", ok ? "PASS" : "FAIL", claim);
    if (!ok) ++failed;
}
static void path(char *out, const char *relative) {
    snprintf(out, 4096, "%s/%s", base, relative);
}
#endif
int wp_mkdir(const char *text) {
    wchar_t *wide = fx_win32_wide(text);
    int ok = wide && CreateDirectoryW(wide, NULL);
    free(wide); return ok;
}
int wp_write(const char *text, const char *contents) {
    wchar_t *wide = fx_win32_wide(text);
    HANDLE file;
    DWORD written;
    int ok;
    if (!wide) return 0;
    file = CreateFileW(wide, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE |
        FILE_SHARE_DELETE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    free(wide);
    if (file == INVALID_HANDLE_VALUE) return 0;
    ok = WriteFile(file, contents, (DWORD)strlen(contents), &written, NULL) &&
        written == strlen(contents);
    CloseHandle(file); return ok;
}
int wp_move(const char *old, const char *next) {
    wchar_t *a = fx_win32_wide(old), *b = fx_win32_wide(next);
    int ok = a && b && MoveFileExW(a, b, MOVEFILE_REPLACE_EXISTING);
    free(a); free(b); return ok;
}
int wp_delete(const char *text) {
    wchar_t *wide = fx_win32_wide(text);
    int ok = wide && DeleteFileW(wide);
    free(wide); return ok;
}
static void cleanup(const wchar_t *directory) {
    WIN32_FIND_DATAW data;
    wchar_t pattern[4096], child[4096];
    HANDLE search;
    swprintf(pattern, 4096, L"%ls\\*", directory);
    search = FindFirstFileW(pattern, &data);
    if (search != INVALID_HANDLE_VALUE) {
        do {
            if (!wcscmp(data.cFileName, L".") || !wcscmp(data.cFileName, L"..")) continue;
            swprintf(child, 4096, L"%ls\\%ls", directory, data.cFileName);
            if (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) cleanup(child);
            else DeleteFileW(child);
        } while (FindNextFileW(search, &data));
        FindClose(search);
    }
    RemoveDirectoryW(directory);
}
int wp_setup(char *out, int capacity) {
    wchar_t temporary[MAX_PATH], unique[MAX_PATH], unicode[MAX_PATH + 64];
    char *utf8;
    size_t i;
    if (!GetTempPathW(MAX_PATH, temporary) || !GetTempFileNameW(temporary, L"fow", 0, unique)) return 0;
    DeleteFileW(unique);
    swprintf(unicode, MAX_PATH + 64, L"%ls-\x03b1-\xd83d\xde00", unique);
    if (!CreateDirectoryW(unicode, NULL)) return 0;
    utf8 = fx_win32_utf8(unicode);
    if (!utf8 || strlen(utf8) >= (size_t)capacity) { free(utf8); cleanup(unicode); return 0; }
    for (i = 0; utf8[i]; ++i) if (utf8[i] == '\\') utf8[i] = '/';
    strcpy(out, utf8); free(utf8); return 1;
}
void wp_cleanup(const char *text) {
    wchar_t *wide = fx_win32_wide(text);
    if (wide) cleanup(wide);
    free(wide);
}
#ifndef WP_HELPERS_ONLY
static int quiet(void *watch) {
    ULONGLONG deadline = GetTickCount64() + 2000;
    char event[4096];
    int kind, error;
    do {
        error = fo_change_native_poll(watch, 100, event, sizeof(event), &kind);
        if (error) return 0;
        if (!kind && !fo_change_native_pending(watch)) return 1;
    } while (GetTickCount64() < deadline);
    return 0;
}
static int expect(void *watch, const char *wanted, int expected) {
    ULONGLONG deadline = GetTickCount64() + 2000;
    char event[4096];
    int kind, error;
    do {
        error = fo_change_native_poll(watch, 100, event, sizeof(event), &kind);
        if (error) { char diagnostic[4096]; fo_change_native_diagnostic(watch, diagnostic, 4096);
            printf("ERROR %s\n", diagnostic); return 0; }
        if (kind == expected && (!wanted || !_stricmp(event, wanted))) return 1;
    } while (GetTickCount64() < deadline);
    return 0;
}
int main(void) {
    void *watch;
    int error, kind, i, saw, writes;
    char source[4096], file[4096], renamed[4096], output[4096], declared[4096];
    char other[4096], missing[4096], event[4096], ignored[4096], diagnostic[4096];
    check(wp_setup(base, sizeof(base)), "private UTF8 Unicode fixture");
    if (failed) return 1;
    path(source, "src"); path(output, "out"); path(other, "other");
    wp_mkdir(source); wp_mkdir(output); wp_mkdir(other);
    path(output, "out/native"); wp_mkdir(output);
    path(declared, "out/native/declared"); wp_mkdir(declared);
    path(ignored, "build"); wp_mkdir(ignored);
    path(ignored, "build/generated.f90"); wp_write(ignored, "generated");
    path(file, "src/alpha-\xce\xb1-\xf0\x9f\x98\x80.f90"); wp_write(file, "before");
    path(missing, "absent/root");
    watch = fo_change_native_open(&error);
    check(watch && !error, "native notification provider opens");
    if (!watch) { wp_cleanup(base); return 1; }
    check(!fo_change_native_exclude_root(watch, output) && !fo_change_native_root(watch, base) &&
        !fo_change_native_root(watch, declared) && !fo_change_native_root(watch, other) &&
        !fo_change_native_root(watch, missing) && !fo_change_native_reconcile(watch), "multi-root subscribe including missing root");
    check(quiet(watch), "initial quiescence");
    wp_write(file, "after changed length");
    check(expect(watch, file, 1), "external edit exact UTF8 path"); quiet(watch);
    path(renamed, "src/renamed.f90"); wp_move(file, renamed);
    check(expect(watch, file, 3) && expect(watch, renamed, 2), "rename has old and new names"); quiet(watch);
    wp_delete(renamed); check(expect(watch, renamed, 3), "file deletion"); quiet(watch);
    wp_write(renamed, "recreated"); check(expect(watch, renamed, 2), "file creation"); quiet(watch);
    wp_write(renamed, "formatted text"); fo_change_native_self(watch, renamed);
    saw = 0;
    for (i = 0; i < 4; ++i) {
        error = fo_change_native_poll(watch, 100, event, sizeof(event), &kind);
        if (error || kind) saw = 1;
    }
    check(!saw && !fo_change_native_pending(watch), "marked formatter edit suppressed and quiet");
    wp_write(renamed, "an unmarked external replacement with different length");
    check(expect(watch, renamed, 1), "immediate unmarked edit after formatter retained"); quiet(watch);
    for (i = 0; i < 100; ++i) wp_write(ignored, "build output activity");
    path(ignored, "out/native/object.o"); wp_write(ignored, "object");
    saw = 0;
    for (i = 0; i < 3; ++i) {
        error = fo_change_native_poll(watch, 100, event, sizeof(event), &kind);
        if (error || kind) saw = 1;
    }
    check(!saw && !fo_change_native_pending(watch), "generated output writes stay quiet");
    path(file, "out/native/declared/input.f90"); wp_write(file, "authored");
    check(expect(watch, file, 2), "explicit input inside output root retained"); quiet(watch);
    path(file, "out/authored"); wp_mkdir(file); wp_write(strcat(file, "/input.f90"), "authored sibling");
    check(expect(watch, NULL, 4), "authored sibling outside excluded output reconciles"); quiet(watch);
    path(file, "absent"); wp_mkdir(file); wp_mkdir(missing);
    path(file, "absent/root/input.f90"); wp_write(file, "populated before subscription");
    error = fo_change_native_poll(watch, 0, event, sizeof(event), &kind);
    check(!error && !kind && fo_change_native_pending(watch), "zero-budget poll defers structural enumeration and retains debt");
    check(expect(watch, NULL, 4), "missing populated directory discovery"); quiet(watch);
    wp_write(file, "later missing-root edit"); check(expect(watch, file, 1), "discovered root edits watched"); quiet(watch);
    path(file, "other"); path(renamed, "other-moved"); wp_move(file, renamed);
    wp_mkdir(file); path(file, "other/replacement.f90"); wp_write(file, "replacement");
    check(expect(watch, NULL, 4), "root rename and replacement reconciles"); quiet(watch);
    wp_write(file, "edit in replacement root"); check(expect(watch, file, 1), "replacement root receives edits"); quiet(watch);
    writes = 0;
    for (i = 0; i < 6000; ++i) {
        snprintf(file, sizeof(file), "%s/src/flood-%06d-long-notification-record-name.f90", base, i);
        writes += wp_write(file, "overflow oracle");
    }
    check(writes == 6000 && fo_change_native_pending(watch), "queued flood is never reported quiet");
    check(expect(watch, NULL, 4), "notification overflow requests reconciliation");
    check(quiet(watch), "overflow recovery reaches true quiescence");
    /* IOCP has no 64-handle wait limit. */
    for (i = 0; i < 70; ++i) {
        snprintf(file, sizeof(file), "%s/src/root-%03d", base, i); wp_mkdir(file);
    }
    check(expect(watch, NULL, 4), "more than 64 directory subscriptions reconcile"); quiet(watch);
    snprintf(file, sizeof(file), "%s/src/root-069/input.f90", base); wp_write(file, "many roots");
    check(expect(watch, file, 2), "directory beyond 64 handles emits events"); quiet(watch);
    fo_change_native_clear_roots(watch); fo_change_native_root(watch, source); fo_change_native_reconcile(watch);
    check(quiet(watch), "retired subscriptions cancellation drains safely");
    path(file, "src/standalone.f90"); wp_write(file, "single file"); quiet(watch);
    fo_change_native_clear_roots(watch); fo_change_native_root(watch, file); fo_change_native_reconcile(watch);
    check(quiet(watch), "file-only declared root subscribes its parent");
    wp_write(file, "single file changed");
    check(expect(watch, file, 1), "file-only root edits retained"); quiet(watch);
    path(other, "other/unwatched.f90"); wp_write(other, "no longer declared");
    check(quiet(watch), "removed roots retire without stuck pending debt");
    fo_change_native_close(watch);
    watch = fo_change_native_open(&error);
    error = fo_change_native_root(watch, "invalid-\xff");
    fo_change_native_diagnostic(watch, diagnostic, sizeof(diagnostic));
    check(error && strstr(diagnostic, "decode declared watch root"), "invalid UTF8 reports operation and failure");
    fo_change_native_close(watch); wp_cleanup(base);
    printf("RESULT %d failures\n", failed);
    return failed ? 1 : 0;
}
#endif
