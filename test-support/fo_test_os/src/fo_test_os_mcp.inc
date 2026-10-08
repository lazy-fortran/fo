/* Common bounded interactive protocol fixture; OS IO and spawn stay below. */
#define FO_TEST_MCP_SESSIONS 4
#define FO_TEST_MCP_BUFFER (1024 * 1024)
typedef struct {
    int pid, input_fd, output_fd;
    char *pending;
    size_t pending_size;
} fo_test_mcp_session;
static fo_test_mcp_session mcp_sessions[FO_TEST_MCP_SESSIONS];
static fo_test_mcp_session *mcp_session_for(int handle) {
    if (handle < 1 || handle > FO_TEST_MCP_SESSIONS) return NULL;
    fo_test_mcp_session *session = &mcp_sessions[handle - 1];
    return session->pid > 0 ? session : NULL;
}
static int fo_test_server_start(const char *driver, const char *cwd, const char *cache,
        const char *state, const char *stderr_path, const char *command) {
    if (!driver || !cwd || !cache || !state || !stderr_path) return -1;
    int slot = -1;
    for (int i = 0; i < FO_TEST_MCP_SESSIONS; ++i)
        if (!mcp_sessions[i].pending) { slot = i; break; }
    if (slot < 0) return -2;
    int input[2] = {-1, -1}, output[2] = {-1, -1}, diagnostic[2] = {-1, -1};
    char *cache_entry = malloc(strlen(cache) + sizeof("FO_CACHE_DIR="));
    char *state_entry = malloc(strlen(state) + sizeof("FO_GREMLIN_STATE_DIR="));
    char *pending = calloc(FO_TEST_MCP_BUFFER + 1, 1);
    int pid = -1;
    if (!cache_entry || !state_entry || !pending) goto failed;
    sprintf(cache_entry, "FO_CACHE_DIR=%s", cache);
    sprintf(state_entry, "FO_GREMLIN_STATE_DIR=%s", state);
    char *const argv[] = {(char *)driver, (char *)command, NULL};
    char *const env[] = {cache_entry, state_entry, "FO_DISABLE_SELF_REFRESH=1",
        "FO_SELF_REFRESH=0", "FO_JOBS=1",
        !strcmp(command, "lsp") ? "FO_LSP_DEBOUNCE_MS=300" : NULL, NULL};
    if (fo_test_pipe_mode(input, 0) || fo_test_pipe_mode(output, 1)) goto failed;
    diagnostic[1] = fo_test_open_write(stderr_path);
    if (diagnostic[1] < 0 || fo_test_ignore_sigpipe()) goto failed;
    pid = fo_test_spawn_redirected(argv, env, cwd, input, output, diagnostic, 0);
    if (pid <= 0) goto failed;
    int child_ends[] = {input[0], output[1], diagnostic[1]};
    input[0] = output[1] = diagnostic[1] = -1;
    int close_error = 0;
    for (int i = 0; i < 3; ++i) if (fo_test_close(child_ends[i])) close_error = 1;
    if (close_error) goto failed;
    if (fo_test_set_nonblocking(input[1]) || fo_test_set_nonblocking(output[0])) goto failed;
    mcp_sessions[slot] = (fo_test_mcp_session){pid, input[1], output[0], pending, 0};
    free(cache_entry); free(state_entry); return slot + 1;
failed:
    for (int i = 0; i < 2; ++i) {
        if (input[i] >= 0) fo_test_close(input[i]);
        if (output[i] >= 0) fo_test_close(output[i]);
        if (diagnostic[i] >= 0) fo_test_close(diagnostic[i]);
    }
    if (pid > 0) {
#if defined(_WIN32) && !defined(__CYGWIN__)
        (void)fo_test_cancel(pid);
#else
        int code, signal_number;
        (void)kill(-pid, SIGKILL);
        (void)fo_test_wait_child(pid, &code, &signal_number, 1);
#endif
    }
    free(cache_entry); free(state_entry); free(pending); return -3;
}
int fo_test_mcp_start(const char *driver, const char *cwd, const char *cache,
        const char *state, const char *stderr_path) {
    return fo_test_server_start(driver, cwd, cache, state, stderr_path, "mcp-server");
}
int fo_test_lsp_start(const char *driver, const char *cwd, const char *cache,
        const char *state, const char *stderr_path) {
    return fo_test_server_start(driver, cwd, cache, state, stderr_path, "lsp");
}
int fo_test_mcp_pid(int handle) {
    fo_test_mcp_session *s = mcp_session_for(handle);
    return s ? s->pid : -1;
}
int fo_test_mcp_write(int handle, const char *message, size_t length) {
    fo_test_mcp_session *s = mcp_session_for(handle);
    if (!s || !message) return -1;
    int64_t deadline = fo_test_monotonic_ms() + 5000;
    size_t offset = 0;
    while (offset < length) {
        int64_t count = fo_test_write(s->input_fd, message + offset, length - offset);
        if (count == -2) {
            int64_t left = deadline - fo_test_monotonic_ms();
            if (left <= 0) {
                /* Cancel any overlapped write before its caller frees message. */
                fo_test_close(s->input_fd); s->input_fd = -1; return -3;
            }
            struct pollfd fd = {s->input_fd, POLLOUT, 0};
            if (fo_test_poll(&fd, 1, (int)(left < 50 ? left : 50)) < 0) {
                fo_test_close(s->input_fd); s->input_fd = -1; return -3;
            }
            continue;
        }
        if (count <= 0) return -2;
        offset += (size_t)count;
    }
    return 0;
}
int fo_test_mcp_read_line(int handle, char *out, size_t capacity, int timeout_ms) {
    fo_test_mcp_session *s = mcp_session_for(handle);
    if (!s || !out || capacity < 2 || timeout_ms < 0) return -1;
    int64_t deadline = fo_test_monotonic_ms() + timeout_ms;
    for (;;) {
        char *newline = memchr(s->pending, '\n', s->pending_size);
        if (newline) {
            size_t length = (size_t)(newline - s->pending);
            if (length && s->pending[length - 1] == '\r') --length;
            if (length + 1 > capacity) return -4;
            memcpy(out, s->pending, length); out[length] = '\0';
            size_t consumed = (size_t)(newline - s->pending) + 1;
            memmove(s->pending, s->pending + consumed, s->pending_size - consumed);
            s->pending_size -= consumed; return (int)length;
        }
        int64_t left = deadline - fo_test_monotonic_ms();
        if (left <= 0) return -2;
        struct pollfd fd = {s->output_fd, POLLIN, 0};
        int ready = fo_test_poll(&fd, 1, left > INT_MAX ? INT_MAX : (int)left);
        if (!ready) return -2;
        if (ready < 0) return -3;
        if (s->pending_size >= FO_TEST_MCP_BUFFER) return -4;
        int64_t count = fo_test_read(s->output_fd, s->pending + s->pending_size,
            FO_TEST_MCP_BUFFER - s->pending_size);
        if (count == -2) continue;
        if (count <= 0) return -5;
        s->pending_size += (size_t)count;
    }
}
int fo_test_mcp_read_bytes(int handle, char *output, size_t length, int timeout_ms) {
    fo_test_mcp_session *session = mcp_session_for(handle);
    if (session == NULL || output == NULL || length > FO_TEST_MCP_BUFFER) return -1;
    int64_t deadline = fo_test_monotonic_ms() + timeout_ms;
    while (session->pending_size < length) {
        int64_t remaining = deadline - fo_test_monotonic_ms();
        if (remaining <= 0) return -2;
        struct pollfd descriptor = {session->output_fd, POLLIN, 0};
        int ready = fo_test_poll(&descriptor, 1, remaining > INT_MAX ? INT_MAX : (int)remaining);
        if (ready < 0 && errno == EINTR) continue;
        if (ready == 0) return -2;
        if (ready < 0) return -3;
        int64_t count = fo_test_read(session->output_fd,
            session->pending + session->pending_size,
            FO_TEST_MCP_BUFFER - session->pending_size);
        if (count == -2 || (count < 0 && errno == EINTR)) continue;
        if (count <= 0) return -5;
        session->pending_size += (size_t)count;
    }
    memcpy(output, session->pending, length);
    memmove(session->pending, session->pending + length, session->pending_size - length);
    session->pending_size -= length;
    return (int)length;
}

int fo_test_mcp_wait(int handle, int timeout_ms, int *exit_code) {
    fo_test_mcp_session *s = mcp_session_for(handle);
    if (!s || !exit_code || timeout_ms < 0) return -1;
    int64_t deadline = fo_test_monotonic_ms() + timeout_ms;
    for (;;) {
        int signal_number = 0;
        int done = fo_test_wait_child(s->pid, exit_code, &signal_number, 0);
        if (done == 1) {
            if (signal_number) *exit_code = -signal_number;
            s->pid = 0; return 0;
        }
        if (done < 0) return -2;
        if (fo_test_monotonic_ms() >= deadline) return 1;
        fo_test_sleep_ms(10);
    }
}
int fo_test_mcp_close(int handle) {
    if (handle < 1 || handle > FO_TEST_MCP_SESSIONS) return -1;
    fo_test_mcp_session *s = &mcp_sessions[handle - 1];
    if (s->pid > 0) return -2;
    int result = 0;
    if (s->input_fd >= 0 && fo_test_close(s->input_fd)) result = -1;
    if (s->output_fd >= 0 && fo_test_close(s->output_fd)) result = -1;
    free(s->pending); memset(s, 0, sizeof(*s));
    s->input_fd = s->output_fd = -1; return result;
}
