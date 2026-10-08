/* Test-owned named-pipe gates. ACK proves the client consumed the release byte. */
typedef struct test_gate {
    char *name;
    HANDLE pipe;
    OVERLAPPED connection;
    int pending;
    struct test_gate *next;
} test_gate;
static test_gate *gates;
static test_gate *gate_for(const char *name) {
    for (test_gate *g = gates; g; g = g->next) if (!strcmp(name, g->name)) return g;
    return NULL;
}
static void gate_cancel_connection(test_gate *g) {
    if (g->pending) {
        DWORD count;
        (void)CancelIoEx(g->pipe, &g->connection);
        (void)GetOverlappedResult(g->pipe, &g->connection, &count, TRUE);
        g->pending = 0;
    }
}
int fo_test_gate_destroy(const char *name) {
    test_gate **slot = &gates;
    while (*slot && strcmp(name, (*slot)->name)) slot = &(*slot)->next;
    if (!*slot) { errno = ENOENT; return -1; }
    test_gate *g = *slot;
    gate_cancel_connection(g);
    *slot = g->next;
    BOOL closed = CloseHandle(g->pipe);
    if (!CloseHandle(g->connection.hEvent)) closed = FALSE;
    free(g->name); free(g);
    return closed ? 0 : -1;
}
int fo_test_gate_close_all(void) {
    int failed = 0;
    while (gates) if (fo_test_gate_destroy(gates->name)) failed = 1;
    return failed ? -1 : 0;
}
static int gate_listen(test_gate *g) {
    HANDLE event = g->connection.hEvent;
    memset(&g->connection, 0, sizeof(g->connection));
    g->connection.hEvent = event;
    ResetEvent(event);
    BOOL connected = ConnectNamedPipe(g->pipe, &g->connection);
    DWORD error = connected ? ERROR_SUCCESS : GetLastError();
    g->pending = error == ERROR_IO_PENDING;
    return connected || error == ERROR_PIPE_CONNECTED || g->pending ? 0 : -1;
}
int fo_test_gate_create(char *name, int capacity) {
    if (!name || capacity < 1) return -1;
    test_gate *existing = gate_for(name);
    if (existing && fo_test_gate_destroy(name)) return -1;
    char namespace_name[160];
    if (!strncmp(name, "\\\\.\\pipe\\", 9)) {
        if (strlen(name) >= sizeof(namespace_name)) return -1;
        strcpy(namespace_name, name);
    } else {
        snprintf(namespace_name, sizeof(namespace_name), "\\\\.\\pipe\\fo-test-gate-%lu-%llu-%ld",
            GetCurrentProcessId(), (unsigned long long)GetTickCount64(),
            InterlockedIncrement(&pipe_serial));
    }
    if (strlen(namespace_name) >= (size_t)capacity) return -1;
    wchar_t *wide = fx_win32_utf16(namespace_name);
    test_gate *g = calloc(1, sizeof(*g));
    if (!wide || !g) { free(wide); free(g); return -1; }
    g->pipe = CreateNamedPipeW(wide, PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED |
        FILE_FLAG_FIRST_PIPE_INSTANCE, PIPE_TYPE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS,
        1, 16, 16, 0, NULL);
    free(wide);
    if (g->pipe == INVALID_HANDLE_VALUE) { free(g); return -1; }
    g->connection.hEvent = CreateEventW(NULL, TRUE, FALSE, NULL);
    g->name = _strdup(namespace_name);
    if (!g->connection.hEvent || !g->name || gate_listen(g)) {
        if (g->connection.hEvent) CloseHandle(g->connection.hEvent);
        CloseHandle(g->pipe); free(g->name); free(g); return -1;
    }
    g->next = gates; gates = g;
    strcpy(name, namespace_name); return 0;
}
static int gate_transfer(test_gate *g, int write_byte, char *byte, uint64_t deadline) {
    OVERLAPPED operation = {0};
    operation.hEvent = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!operation.hEvent) return -1;
    DWORD count = 0;
    BOOL completed = write_byte ? WriteFile(g->pipe, byte, 1, &count, &operation) :
        ReadFile(g->pipe, byte, 1, &count, &operation);
    DWORD error = completed ? ERROR_SUCCESS : GetLastError();
    if (error == ERROR_IO_PENDING) {
        uint64_t now = GetTickCount64();
        DWORD wait = WaitForSingleObject(operation.hEvent,
            now < deadline ? (DWORD)(deadline - now) : 0);
        if (wait != WAIT_OBJECT_0) (void)CancelIoEx(g->pipe, &operation);
        completed = GetOverlappedResult(g->pipe, &operation, &count, TRUE);
    }
    CloseHandle(operation.hEvent);
    return completed && count == 1 ? 0 : -1;
}
int fo_test_gate_release(const char *name, int timeout_ms) {
    test_gate *g = gate_for(name);
    if (!g || timeout_ms < 0) return -1;
    uint64_t deadline = GetTickCount64() + (unsigned)timeout_ms;
    while (GetTickCount64() < deadline) {
        if (g->pending) {
            DWORD wait = WaitForSingleObject(g->connection.hEvent, 20);
            if (wait == WAIT_TIMEOUT) continue;
            DWORD count;
            BOOL connected = wait == WAIT_OBJECT_0 &&
                GetOverlappedResult(g->pipe, &g->connection, &count, FALSE);
            if (!connected) {
                gate_cancel_connection(g);
                (void)DisconnectNamedPipe(g->pipe);
                if (gate_listen(g)) return -1;
                continue;
            }
            g->pending = 0;
        }
        char token = 'x', acknowledgement = 0;
        if (gate_transfer(g, 1, &token, deadline)) {
            /* An obsolete generation may have closed its client handle. */
            (void)DisconnectNamedPipe(g->pipe);
            if (gate_listen(g)) return -1;
            continue;
        }
        int received = gate_transfer(g, 0, &acknowledgement, deadline);
        (void)DisconnectNamedPipe(g->pipe);
        int listening = gate_listen(g);
        return received || acknowledgement != token || listening ? -1 : 0;
    }
    errno = ETIMEDOUT; return -1;
}
