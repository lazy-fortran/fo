/* The crash fixture freezes its exact owner before killing descendants. */
int fo_test_freeze_identity(int pid, uint64_t birth) {
    if (pid <= 0 || !birth) return -1;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE,
        FALSE, (DWORD)pid);
    if (!process) return -1;
    FILETIME born, exited, kernel, user;
    if (!GetProcessTimes(process, &born, &exited, &kernel, &user) ||
        (((uint64_t)born.dwHighDateTime << 32) | born.dwLowDateTime) != birth ||
        WaitForSingleObject(process, 0) != WAIT_TIMEOUT) { CloseHandle(process); return -2; }
    struct { DWORD id; HANDLE handle; } threads[1024];
    int count = 0, result = -1;
    for (int pass = 0; pass < 32; ++pass) {
        HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
        if (snapshot == INVALID_HANDLE_VALUE) break;
        THREADENTRY32 entry = {0}; entry.dwSize = sizeof(entry);
        int added = 0, failed = 0;
        BOOL present = Thread32First(snapshot, &entry);
        if (!present) failed = 1;
        while (present) {
            if (entry.th32OwnerProcessID == (DWORD)pid) {
                int known = 0;
                for (int i = 0; i < count; ++i) if (threads[i].id == entry.th32ThreadID) {
                    if (WaitForSingleObject(threads[i].handle, 0) != WAIT_TIMEOUT) failed = 1;
                    known = 1;
                }
                if (failed) break;
                if (!known) {
                    HANDLE thread = OpenThread(THREAD_SUSPEND_RESUME | THREAD_QUERY_LIMITED_INFORMATION |
                        SYNCHRONIZE, FALSE, entry.th32ThreadID);
                    if (!thread || count == 1024) { if (thread) CloseHandle(thread); failed = 1; break; }
                    if (GetProcessIdOfThread(thread) != (DWORD)pid || SuspendThread(thread) == (DWORD)-1) {
                        CloseHandle(thread); failed = 1; break;
                    }
                    threads[count].id = entry.th32ThreadID; threads[count++].handle = thread; added++;
                }
            }
            present = Thread32Next(snapshot, &entry);
        }
        DWORD enumeration_error = GetLastError();
        CloseHandle(snapshot);
        if (failed || enumeration_error != ERROR_NO_MORE_FILES) break;
        if (!added && count > 0) {
            /* Query the kernel suspend count through one balanced increment. */
            result = 0;
            for (int i = 0; i < count; ++i) {
                DWORD before = SuspendThread(threads[i].handle);
                if (before == (DWORD)-1) { result = -1; break; }
                DWORD after = ResumeThread(threads[i].handle);
                if (before < 1 || after != before + 1) { result = -1; break; }
            }
            if (WaitForSingleObject(process, 0) != WAIT_TIMEOUT) result = -1;
            break;
        }
    }
    for (int i = 0; i < count; ++i) {
        if (result) (void)ResumeThread(threads[i].handle);
        CloseHandle(threads[i].handle);
    }
    CloseHandle(process);
    return result;
}
