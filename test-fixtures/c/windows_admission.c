/* Independent native admission oracle, using public lease/process APIs. */
#define _WIN32_WINNT 0x0a00
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <errno.h>
#include <io.h>

extern wchar_t *fx_win32_utf16(const char *);
extern wchar_t *fx_win32_command_line(const char *, int, int);
extern int fo_gremlin_host_lease_acquire_weighted(const char *, int, int, int *, int *,
    int *, int *, char *, char *, int);
extern int fo_gremlin_host_scope_retire(int *, int *, const char *, const char *);
extern int fo_gremlin_lease_release(int);
extern int fo_gremlin_lease_is_busy(int);
extern void fo_c_start_argv_logged(const char *, const char *, int, int, const char *,
    const char *, int *, int *);

struct lease { int fds[3], slots[3], authority, guardian, weight; char scope[4096], previous[4096]; };
struct identity { DWORD pid; uint64_t birth; HANDLE handle; };
static int failures;
static char image[32768], scratch[32768];
static void check(int okay, const char *message) {
    printf("%s: %s\n", okay ? "PASS" : "FAIL", message); fflush(stdout);
    if (!okay) ++failures;
}
static uint64_t birth(HANDLE process) {
    FILETIME created, exited, kernel, user;
    if (!GetProcessTimes(process, &created, &exited, &kernel, &user)) return 0;
    return ((uint64_t)created.dwHighDateTime << 32) | created.dwLowDateTime;
}
static char *utf8(const wchar_t *wide) {
    int count = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, NULL, 0, NULL, NULL);
    char *text = count ? malloc((size_t)count) : NULL;
    if (text) WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1, text, count, NULL, NULL);
    return text;
}
static FILE *open_file(const char *path, const wchar_t *mode) {
    wchar_t *wide = fx_win32_utf16(path);
    FILE *file = wide ? _wfopen(wide, mode) : NULL;
    free(wide); return file;
}
static int append(const char *path, const char *bytes) {
    FILE *file = open_file(path, L"ab");
    if (!file) return 0;
    size_t size = strlen(bytes); int okay = fwrite(bytes, 1, size, file) == size;
    return fclose(file) == 0 && okay;
}
static int exists(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    DWORD attributes = wide ? GetFileAttributesW(wide) : INVALID_FILE_ATTRIBUTES;
    free(wide); return attributes != INVALID_FILE_ATTRIBUTES;
}
static size_t size_of(const char *path) {
    wchar_t *wide = fx_win32_utf16(path); WIN32_FILE_ATTRIBUTE_DATA data;
    int okay = wide && GetFileAttributesExW(wide, GetFileExInfoStandard, &data);
    free(wide);
    return okay ? (size_t)(((uint64_t)data.nFileSizeHigh << 32) | data.nFileSizeLow) : 0;
}
static int wait_file(const char *path, HANDLE owner) {
    ULONGLONG deadline = GetTickCount64() + 10000;
    do {
        if (exists(path)) return 1;
        if (owner && WaitForSingleObject(owner, 0) == WAIT_OBJECT_0) return 0;
        Sleep(10);
    } while (GetTickCount64() < deadline);
    return 0;
}
static int pack(char *bytes, const char **argv, int count) {
    int size = 0;
    for (int index = 0; index < count; ++index) {
        int used = (int)strlen(argv[index]) + 1;
        memcpy(bytes + size, argv[index], (size_t)used); size += used;
    }
    return size;
}
static PROCESS_INFORMATION independent_spawn(const char **argv, int count) {
    char bytes[65536]; int size = pack(bytes, argv, count);
    wchar_t *command = fx_win32_command_line(bytes, size, count);
    STARTUPINFOW startup = {0}; startup.cb = sizeof(startup);
    PROCESS_INFORMATION process = {0};
    if (!command || !CreateProcessW(NULL, command, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &process))
        check(0, "independent native control launches directly");
    free(command); if (process.hThread) CloseHandle(process.hThread); return process;
}
static int acquire(const char *kind, int capacity, int weight, struct lease *lease) {
    memset(lease, 0, sizeof(*lease)); lease->weight = weight; lease->authority = -1;
    for (int index = 0; index < 3; ++index) lease->fds[index] = lease->slots[index] = -1;
    return fo_gremlin_host_lease_acquire_weighted(kind, capacity, weight, lease->fds, lease->slots,
        &lease->authority, &lease->guardian, lease->scope, lease->previous, sizeof(lease->scope));
}
static int retire(struct lease *lease) {
    int error = fo_gremlin_host_scope_retire(&lease->authority, &lease->guardian,
        lease->scope, lease->previous);
    if (error) {
        fprintf(stderr, "public-scope-retire: error=%d errno=%d authority=%d scope=%s\n",
            error, errno, lease->authority, lease->scope);
        return error;
    }
    for (int index = 0; index < lease->weight; ++index) {
        if (lease->fds[index] >= 0) {
            error = fo_gremlin_lease_release(lease->fds[index]);
            if (error) {
                fprintf(stderr, "public-slot-release: error=%d errno=%d fd=%d slot=%d\n",
                    error, errno, lease->fds[index], lease->slots[index]);
                return error;
            }
            lease->fds[index] = -1;
        }
    }
    return 0;
}
static void record_identity(const char *path, const char *role) {
    char line[160];
    snprintf(line, sizeof(line), "%s %lu %llu\n", role, (unsigned long)GetCurrentProcessId(),
        (unsigned long long)birth(GetCurrentProcess()));
    if (!append(path, line)) ExitProcess(89);
}
static int read_identities(const char *path, struct identity *values, int wanted) {
    FILE *file = open_file(path, L"rb"); if (!file) return 0;
    char line[160], role[32]; unsigned long pid; unsigned long long created; int count = 0;
    while (count < wanted && fgets(line, sizeof(line), file)) {
        if (sscanf(line, "%31s %lu %llu", role, &pid, &created) != 3) continue;
        HANDLE handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, (DWORD)pid);
        if (!handle || birth(handle) != created || WaitForSingleObject(handle, 0) != WAIT_TIMEOUT) {
            if (handle) CloseHandle(handle);
            for (int index = 0; index < count; ++index) CloseHandle(values[index].handle);
            fclose(file); return -1;
        }
        values[count++] = (struct identity){(DWORD)pid, (uint64_t)created, handle};
    }
    fclose(file); return count;
}
/* The ready marker locates authority; kernel handles independently establish
   birth and physical directory identity before checking actual membership. */
static HANDLE parent_job(const char *ready, const struct identity *owner) {
    char scope[32768] = {0}, authority[32768], state[32768], stamp[64];
    unsigned long long kind; int pid, weight;
    FILE *file = open_file(ready, L"rb");
    if (!file) return NULL;
    int okay = fgets(scope, sizeof(scope), file) != NULL;
    fclose(file);
    scope[strcspn(scope, "\r\n")] = '\0';
    if (!okay || snprintf(authority, sizeof(authority), "%s/authority", scope) >=
        (int)sizeof(authority)) return NULL;
    file = open_file(authority, L"rb");
    if (!file) return NULL;
    okay = fscanf(file, "%d %63s %d %llx\n", &pid, stamp, &weight, &kind) == 4 &&
        fgets(state, sizeof(state), file) != NULL;
    fclose(file);
    if (!okay || pid != (int)owner->pid || strtoull(stamp, NULL, 10) != owner->birth ||
        birth(owner->handle) != owner->birth || WaitForSingleObject(owner->handle, 0) != WAIT_TIMEOUT)
        return NULL;
    state[strcspn(state, "\r\n")] = '\0';
    wchar_t *wide = fx_win32_utf16(state);
    HANDLE directory = wide ? CreateFileW(wide, 0, FILE_SHARE_READ | FILE_SHARE_WRITE |
        FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL) : INVALID_HANDLE_VALUE;
    free(wide);
    if (directory == INVALID_HANDLE_VALUE) return NULL;
    BY_HANDLE_FILE_INFORMATION info;
    okay = GetFileInformationByHandle(directory, &info) &&
        (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
    CloseHandle(directory);
    if (!okay) return NULL;
    uint64_t index = ((uint64_t)info.nFileIndexHigh << 32) | info.nFileIndexLow;
    wchar_t name[128];
    if (swprintf(name, 128, L"Local\\FoProcessScope-%lu-%llu-%08lx-%016llx",
        (unsigned long)owner->pid, (unsigned long long)owner->birth,
        (unsigned long)info.dwVolumeSerialNumber, (unsigned long long)index) < 0) return NULL;
    return OpenJobObjectW(JOB_OBJECT_QUERY, FALSE, name);
}
static BOOL WINAPI ignore_break(DWORD event) { (void)event; return TRUE; }
static void leaf(const char *tree) {
    SetConsoleCtrlHandler(ignore_break, TRUE); record_identity(tree, "leaf");
    for (;;) { append(tree, "tick\n"); Sleep(20); }
}
static int child_mode(int argc, wchar_t **argv) {
    if (argc == 4 && !wcscmp(argv[1], L"--leaf")) {
        char *tree = utf8(argv[2]), *kind = utf8(argv[3]);
        struct lease own;
        if (!tree || !kind) return 88;
        int error = acquire(kind, 3, 1, &own);
        if (error) { fprintf(stderr, "nested leaf acquire=%d\n", error); return 88; }
        leaf(tree);
    }
    if (argc == 5 && !wcscmp(argv[1], L"--delegated")) {
        char *kind = utf8(argv[2]), *tree = utf8(argv[3]), *progress = utf8(argv[4]);
        struct lease own; int error = acquire(kind, 3, 1, &own);
        if (error) { fprintf(stderr, "delegated acquire=%d\n", error); return 87; }
        record_identity(tree, "delegated");
        char bytes[65536]; const char *args[] = {image, "--leaf", tree, kind}; int pid;
        fo_c_start_argv_logged(NULL, bytes, pack(bytes, args, 4), 4, "NUL", NULL, &pid, &error);
        if (error || pid <= 0) return 86;
        SetConsoleCtrlHandler(ignore_break, TRUE);
        for (;;) { append(progress, "delegated-tick\n"); Sleep(20); }
    }
    if (argc == 9 && !wcscmp(argv[1], L"--owner")) {
        char *kind = utf8(argv[2]), *ready = utf8(argv[5]), *release = utf8(argv[6]);
        char *progress = utf8(argv[7]), *tree = utf8(argv[8]);
        struct lease own; int error = acquire(kind, _wtoi(argv[3]), _wtoi(argv[4]), &own);
        if (error) { fprintf(stderr, "owner acquire=%d\n", error); return 85; }
        if (strcmp(tree, "-")) {
            char bytes[65536]; const char *args[] = {image, "--delegated", kind, tree, progress}; int pid;
            fo_c_start_argv_logged(NULL, bytes, pack(bytes, args, 5), 5, "NUL", NULL, &pid, &error);
            if (error || pid <= 0) return 84;
            struct identity values[2] = {0}; int count = 0;
            ULONGLONG deadline = GetTickCount64() + 10000;
            do {
                count = read_identities(tree, values, 2);
                for (int index = 0; index < (count > 0 ? count : 0); ++index) CloseHandle(values[index].handle);
                if (count != 2) Sleep(10);
            } while (count != 2 && GetTickCount64() < deadline);
            if (count != 2) return 83;
        }
        if (!append(ready, own.scope) || !append(ready, "\n")) return 82;
        while (!exists(release)) { append(progress, "owner-tick\n"); Sleep(20); }
        error = retire(&own);
        if (error) { fprintf(stderr, "owner retire=%d\n", error); return 81; }
        return 0;
    }
    return 80;
}
static void path(char *value, size_t capacity, const char *name) {
    snprintf(value, capacity, "%s/%s", scratch, name);
}
static PROCESS_INFORMATION start_owner(const char *kind, const char *weight,
    const char *ready, const char *release, const char *progress, const char *tree) {
    const char *args[] = {image, "--owner", kind, "3", weight, ready, release, progress, tree};
    return independent_spawn(args, 9);
}
static int release_owner(PROCESS_INFORMATION *owner, const char *release) {
    DWORD code = 0;
    int okay = append(release, "release\n") && owner->hProcess &&
        WaitForSingleObject(owner->hProcess, 10000) == WAIT_OBJECT_0 &&
        GetExitCodeProcess(owner->hProcess, &code) && code == 0;
    if (!okay && owner->hProcess && WaitForSingleObject(owner->hProcess, 0) == WAIT_TIMEOUT) {
        TerminateProcess(owner->hProcess, 79); WaitForSingleObject(owner->hProcess, 5000);
    }
    if (owner->hProcess) CloseHandle(owner->hProcess);
    owner->hProcess = NULL; return okay;
}
static void clear_admission_hint(void) { _putenv_s("FO_GREMLIN_ADMISSION_SCOPE", ""); }
static struct identity sole_direct_child(DWORD parent) {
    struct identity result = {0};
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) return result;
    PROCESSENTRY32W entry = {0}; entry.dwSize = sizeof(entry); int count = 0;
    if (Process32FirstW(snapshot, &entry)) do {
        if (entry.th32ParentProcessID != parent) continue;
        HANDLE handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE | PROCESS_TERMINATE,
            FALSE, entry.th32ProcessID);
        if (!handle || WaitForSingleObject(handle, 0) != WAIT_TIMEOUT) {
            if (handle) CloseHandle(handle);
            continue;
        }
        uint64_t created = birth(handle);
        if (!created) { CloseHandle(handle); continue; }
        if (!count) result = (struct identity){entry.th32ProcessID, created, handle};
        else CloseHandle(handle);
        ++count;
    } while (Process32NextW(snapshot, &entry));
    CloseHandle(snapshot);
    if (count != 1 && result.handle) { CloseHandle(result.handle); result = (struct identity){0}; }
    return result;
}
int wmain(int argc, wchar_t **argv) {
    wchar_t wide_image[32768];
    if (!GetModuleFileNameW(NULL, wide_image, 32768) || !WideCharToMultiByte(CP_UTF8,
        WC_ERR_INVALID_CHARS, wide_image, -1, image, sizeof(image), NULL, NULL)) return 78;
    if (argc > 1 && !wcsncmp(argv[1], L"--", 2)) return child_mode(argc, argv);
    if (argc != 2) { fputs("usage: windows_admission.exe PRIVATE_EXISTING_SCRATCH\n", stderr); return 77; }
    char *root = utf8(argv[1]); if (!root) return 76;
    snprintf(scratch, sizeof(scratch), "%s", root); free(root);
    char local[32768], ready_a[32768], ready_b[32768], release_a[32768], release_b[32768];
    char progress_a[32768], progress_b[32768], tree[32768];
    path(local, sizeof(local), "local-app-data");
    wchar_t *wide_local = fx_win32_utf16(local);
    check(wide_local && CreateDirectoryW(wide_local, NULL), "creates isolated native admission namespace");
    free(wide_local);
    _putenv_s("LOCALAPPDATA", local); clear_admission_hint();
    _putenv_s("FO_GREMLIN_PROCESS_SCOPE_DIR", "");
    _putenv_s("FO_GREMLIN_PROCESS_SCOPE_PID", "");
    _putenv_s("FO_GREMLIN_PROCESS_SCOPE_START", "");
    path(ready_a, sizeof(ready_a), "owner-a.ready"); path(ready_b, sizeof(ready_b), "owner-b.ready");
    path(release_a, sizeof(release_a), "owner-a.release"); path(release_b, sizeof(release_b), "owner-b.release");
    path(progress_a, sizeof(progress_a), "owner-a.progress"); path(progress_b, sizeof(progress_b), "owner-b.progress");
    const char *kind = "native-admission-weighted-oracle";
    PROCESS_INFORMATION owner_a = start_owner(kind, "2", ready_a, release_a, progress_a, "-");
    check(wait_file(ready_a, owner_a.hProcess), "live independent owner holds two of three slots");
    uint64_t owner_a_birth = birth(owner_a.hProcess);
    struct lease candidate;
    int error = acquire(kind, 3, 2, &candidate);
    check(fo_gremlin_lease_is_busy(error) && candidate.authority < 0,
        "weighted request cannot partially reserve the single free slot");
    if (!error) retire(&candidate);
    clear_admission_hint();
    PROCESS_INFORMATION owner_b = start_owner(kind, "1", ready_b, release_b, progress_b, "-");
    check(wait_file(ready_b, owner_b.hProcess), "another direct owner can reserve the untouched free slot");
    uint64_t owner_b_birth = birth(owner_b.hProcess);
    error = acquire(kind, 3, 1, &candidate);
    check(fo_gremlin_lease_is_busy(error), "contention honors all three live weighted reservations");
    if (!error) retire(&candidate);
    error = acquire(kind, 2, 1, &candidate);
    check(error != 0, "live reservations prevent shrinking the published capacity");
    if (!error) retire(&candidate);
    error = acquire(kind, 4, 1, &candidate);
    check(error != 0, "live reservations prevent expanding capacity around their authority");
    if (!error) retire(&candidate);
    check(owner_a.hProcess && birth(owner_a.hProcess) == owner_a_birth &&
        WaitForSingleObject(owner_a.hProcess, 0) == WAIT_TIMEOUT,
        "failed competitors preserve the exact live owner process");
    check(release_owner(&owner_a, release_a), "only owner A retires its scope and releases its two slots");
    size_t peer_progress = size_of(progress_b); Sleep(120);
    check(owner_b.hProcess && birth(owner_b.hProcess) == owner_b_birth &&
        WaitForSingleObject(owner_b.hProcess, 0) == WAIT_TIMEOUT && size_of(progress_b) > peer_progress,
        "owner A retirement preserves unrelated owner B and its real progress");
    clear_admission_hint();
    error = acquire(kind, 3, 2, &candidate);
    check(!error, "reuses exactly the released weight while the unrelated owner keeps one slot");
    if (!error) {
        char current_scope[4096]; snprintf(current_scope, sizeof(current_scope), "%s", candidate.scope);
        clear_admission_hint();
        struct lease rejected;
        int full = acquire(kind, 3, 1, &rejected);
        check(fo_gremlin_lease_is_busy(full), "released-owner reuse does not exceed total capacity");
        if (!full) retire(&rejected);
        _putenv_s("FO_GREMLIN_ADMISSION_SCOPE", current_scope);
        check(!retire(&candidate), "current reservation owner explicitly retires before returning slots");
    }
    check(release_owner(&owner_b, release_b), "owner B independently retires its remaining reservation");
    clear_admission_hint();
    error = acquire(kind, 3, 3, &candidate);
    check(!error, "full capacity is available after both exact owners complete");
    if (!error) check(!retire(&candidate), "full weighted reservation retires without inherited debt");

    path(ready_a, sizeof(ready_a), "crash-owner.ready"); path(release_a, sizeof(release_a), "crash-owner.release");
    path(progress_a, sizeof(progress_a), "crash-owner.progress"); path(tree, sizeof(tree), "crash-tree.identities");
    clear_admission_hint();
    owner_a = start_owner(kind, "3", ready_a, release_a, progress_a, tree);
    check(wait_file(ready_a, owner_a.hProcess), "managed descendant acquires delegated capacity under live parent");
    struct identity descendants[2] = {0};
    int found = read_identities(tree, descendants, 2);
    check(found == 2, "independent oracle retains exact delegated child and continuing leaf HANDLEs");
    struct identity owner = {owner_a.dwProcessId, birth(owner_a.hProcess), owner_a.hProcess};
    HANDLE ancestor = parent_job(ready_a, &owner);
    check(ancestor != NULL, "independent physical directory and exact owner birth identify parent Job");
    for (int index = 0; index < (found > 0 ? found : 0); ++index) {
        BOOL member = FALSE;
        check(ancestor && IsProcessInJob(descendants[index].handle, ancestor, &member) && member,
            "independently held nested descendant belongs to the exact parent Job before crash");
    }
    clear_admission_hint();
    error = acquire(kind, 3, 1, &candidate);
    check(fo_gremlin_lease_is_busy(error), "delegated child spends parent weight without freeing global capacity");
    if (!error) retire(&candidate);
    check(owner.handle && TerminateProcess(owner.handle, 73) &&
        WaitForSingleObject(owner.handle, 5000) == WAIT_OBJECT_0, "crashes only the independently held owner HANDLE");
    clear_admission_hint();
    error = acquire(kind, 3, 3, &candidate);
    printf("crash-recovery-admit: error=%d authority=%d\n", error, candidate.authority);
    check(!error, "dead owner capacity is reusable only after durable scoped descendant drainage");
    check(owner.handle && birth(owner.handle) == owner.birth && WaitForSingleObject(owner.handle, 0) == WAIT_OBJECT_0,
        "original exact owner is signaled at dead-slot reuse");
    for (int index = 0; index < (found > 0 ? found : 0); ++index) {
        check(birth(descendants[index].handle) == descendants[index].birth &&
            WaitForSingleObject(descendants[index].handle, 0) == WAIT_OBJECT_0,
            "exact known descendant HANDLE is signaled before recovered capacity returns");
        CloseHandle(descendants[index].handle);
    }
    size_t stopped = size_of(tree); Sleep(150);
    check(size_of(tree) == stopped, "crash descendants cannot continue heartbeat bytes after capacity reuse");
    if (!error) check(!retire(&candidate), "recovered full reservation retires through public authority");
    if (ancestor) CloseHandle(ancestor);
    if (owner.handle) CloseHandle(owner.handle);

    /* A different private namespace isolates deliberate unresolved authority
       from positive cases. Use a real published reservation, not a fake record. */
    path(local, sizeof(local), "negative-local-app-data"); wide_local = fx_win32_utf16(local);
    check(wide_local && CreateDirectoryW(wide_local, NULL), "creates private unresolved-authority negative namespace");
    free(wide_local); _putenv_s("LOCALAPPDATA", local); clear_admission_hint();
    path(ready_a, sizeof(ready_a), "lost-guardian.ready"); path(release_a, sizeof(release_a), "lost-guardian.release");
    path(progress_a, sizeof(progress_a), "lost-guardian.progress");
    owner_a = start_owner(kind, "3", ready_a, release_a, progress_a, "-");
    check(wait_file(ready_a, owner_a.hProcess), "real native guardian arms authority before any negative payload");
    struct identity guardian = sole_direct_child(owner_a.dwProcessId);
    check(guardian.handle != NULL, "independent kernel snapshot holds sole ready owner guardian exactly");
    if (guardian.handle) {
        check(TerminateProcess(guardian.handle, 72) && WaitForSingleObject(guardian.handle, 5000) == WAIT_OBJECT_0 &&
            birth(guardian.handle) == guardian.birth, "crashes only the exact independently held scope guardian");
        CloseHandle(guardian.handle);
    }
    size_t progress = size_of(progress_a); Sleep(100);
    check(owner_a.hProcess && WaitForSingleObject(owner_a.hProcess, 0) == WAIT_TIMEOUT &&
        size_of(progress_a) > progress, "guard loss does not silently kill or steal the live owner");
    check(owner_a.hProcess && TerminateProcess(owner_a.hProcess, 71) &&
        WaitForSingleObject(owner_a.hProcess, 5000) == WAIT_OBJECT_0,
        "then crashes only the exact held owner, leaving genuine armed authority unresolved");
    if (owner_a.hProcess) CloseHandle(owner_a.hProcess);
    clear_admission_hint(); error = acquire(kind, 3, 3, &candidate);
    printf("missing-guardian-admit: error=%d authority=%d\n", error, candidate.authority);
    check(guardian.pid && error == EIO && candidate.authority < 0,
        "armed missing guardian/job cannot falsely free capacity or produce a green lease");
    if (!error) retire(&candidate);
    printf("windows-admission: %d failures\n", failures);
    return failures ? 1 : 0;
}
