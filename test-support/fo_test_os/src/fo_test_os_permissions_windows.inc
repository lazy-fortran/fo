/* Observe the real descriptor independently of Fx's permission mapper. */
extern int fx_win_chmod(const char *, mode_t);
int fo_test_prepare_executable(const char *path) { return fx_win_chmod(path, 0755); }
typedef BOOL (WINAPI *get_security_fn)(LPCWSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, LPDWORD);
static PSECURITY_DESCRIPTOR permission_descriptor(const char *path, HMODULE security) {
    union { FARPROC raw; get_security_fn typed; } get;
    get.raw = GetProcAddress(security, "GetFileSecurityW");
    wchar_t *wide = fx_win32_utf16(path);
    DWORD needed = 0;
    if (!wide || !get.raw) { free(wide); return NULL; }
    SECURITY_INFORMATION fields = OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
    (void)get.typed(wide, fields, NULL, 0, &needed);
    PSECURITY_DESCRIPTOR descriptor = needed ? malloc(needed) : NULL;
    if (descriptor && !get.typed(wide, fields, descriptor, needed, &needed)) {
        free(descriptor); descriptor = NULL;
    }
    free(wide); return descriptor;
}
int fo_test_permissions_snapshot(const char *path, char *out, int capacity) {
    if (!out || capacity < 1) return -1;
    out[0] = 0;
    HMODULE security = LoadLibraryW(L"advapi32.dll");
    if (!security) return -1;
    typedef BOOL (WINAPI *convert_fn)(PSECURITY_DESCRIPTOR, DWORD, SECURITY_INFORMATION, LPWSTR *, PULONG);
    union { FARPROC raw; convert_fn typed; } convert;
    convert.raw = GetProcAddress(security, "ConvertSecurityDescriptorToStringSecurityDescriptorW");
    PSECURITY_DESCRIPTOR descriptor = permission_descriptor(path, security);
    wchar_t *sddl = NULL;
    int result = -1;
    if (descriptor && convert.raw && convert.typed(descriptor, SDDL_REVISION_1,
            OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, &sddl, NULL)) {
        int needed = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, sddl, -1, NULL, 0, NULL, NULL);
        if (needed > 0 && needed <= capacity && WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
                sddl, -1, out, capacity, NULL, NULL)) result = 0;
    }
    if (sddl) LocalFree(sddl);
    free(descriptor); FreeLibrary(security); return result;
}
int fo_test_executable_access(const char *path) {
    wchar_t *wide = fx_win32_utf16(path);
    HANDLE file = wide ? CreateFileW(wide, FILE_READ_DATA | FILE_EXECUTE | READ_CONTROL,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, 0, NULL) : INVALID_HANDLE_VALUE;
    free(wide);
    return file != INVALID_HANDLE_VALUE && CloseHandle(file) ? 1 : 0;
}
int fo_test_executable_permissions(const char *path) {
    HMODULE security = LoadLibraryW(L"advapi32.dll");
    if (!security) return -1;
    typedef BOOL (WINAPI *owner_fn)(PSECURITY_DESCRIPTOR, PSID *, LPBOOL);
    typedef BOOL (WINAPI *dacl_fn)(PSECURITY_DESCRIPTOR, LPBOOL, PACL *, LPBOOL);
    typedef BOOL (WINAPI *ace_fn)(PACL, DWORD, LPVOID *);
    typedef BOOL (WINAPI *equal_fn)(PSID, PSID);
    typedef BOOL (WINAPI *world_fn)(WELL_KNOWN_SID_TYPE, PSID, PSID, DWORD *);
    union { FARPROC raw; owner_fn typed; } owner;
    union { FARPROC raw; dacl_fn typed; } dacl;
    union { FARPROC raw; ace_fn typed; } ace;
    union { FARPROC raw; equal_fn typed; } equal;
    union { FARPROC raw; world_fn typed; } world;
    owner.raw = GetProcAddress(security, "GetSecurityDescriptorOwner");
    dacl.raw = GetProcAddress(security, "GetSecurityDescriptorDacl");
    ace.raw = GetProcAddress(security, "GetAce");
    equal.raw = GetProcAddress(security, "EqualSid");
    world.raw = GetProcAddress(security, "CreateWellKnownSid");
    PSECURITY_DESCRIPTOR descriptor = permission_descriptor(path, security);
    DWORD world_size = SECURITY_MAX_SID_SIZE;
    unsigned char world_sid[SECURITY_MAX_SID_SIZE];
    PSID owner_sid = NULL; PACL acl = NULL; BOOL present = FALSE, defaulted = FALSE;
    int valid = descriptor && owner.raw && dacl.raw && ace.raw && equal.raw && world.raw &&
        owner.typed(descriptor, &owner_sid, &defaulted) &&
        dacl.typed(descriptor, &present, &acl, &defaulted) && present && acl &&
        world.typed(WinWorldSid, NULL, world_sid, &world_size);
    DWORD owner_rights = 0, world_rights = 0;
    const DWORD owner_required = FILE_GENERIC_READ | FILE_GENERIC_WRITE | FILE_GENERIC_EXECUTE;
    const DWORD world_required = FILE_GENERIC_READ | FILE_GENERIC_EXECUTE;
    for (DWORD i = 0; valid && i < acl->AceCount; ++i) {
        ACCESS_ALLOWED_ACE *entry = NULL;
        if (!ace.typed(acl, i, (void **)&entry)) { valid = 0; break; }
        if (entry->Header.AceFlags & INHERIT_ONLY_ACE) continue;
        if (entry->Header.AceType != ACCESS_ALLOWED_ACE_TYPE &&
            entry->Header.AceType != ACCESS_DENIED_ACE_TYPE) { valid = 0; break; }
        int is_owner = equal.typed(owner_sid, &entry->SidStart);
        int is_world = equal.typed(world_sid, &entry->SidStart);
        if (!is_owner && !is_world) continue;
        DWORD rights = entry->Mask;
        if (rights & GENERIC_ALL) rights |= FILE_ALL_ACCESS;
        if (rights & GENERIC_READ) rights |= FILE_GENERIC_READ;
        if (rights & GENERIC_WRITE) rights |= FILE_GENERIC_WRITE;
        if (rights & GENERIC_EXECUTE) rights |= FILE_GENERIC_EXECUTE;
        if (entry->Header.AceType == ACCESS_DENIED_ACE_TYPE) {
            if ((is_owner && (rights & owner_required)) || (is_world && (rights & world_required))) valid = 0;
        } else if (entry->Header.AceType == ACCESS_ALLOWED_ACE_TYPE) {
            if (is_owner) owner_rights |= rights;
            if (is_world) world_rights |= rights;
        } else valid = 0;
    }
    valid = valid && (owner_rights & owner_required) == owner_required &&
        (world_rights & world_required) == world_required &&
        !(world_rights & (FILE_WRITE_DATA | FILE_APPEND_DATA | FILE_WRITE_EA | FILE_WRITE_ATTRIBUTES | DELETE | WRITE_DAC | WRITE_OWNER));
    free(descriptor); FreeLibrary(security);
    return valid && fo_test_executable_access(path) ? 1 : 0;
}
