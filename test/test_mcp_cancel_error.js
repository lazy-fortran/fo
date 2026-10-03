#!/usr/bin/env node
// Inject cancellation failures and verify MCP retains ownership through shutdown/EOF.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const driver = process.argv[2] || process.env.FO;
if (!driver) throw new Error('pass the newly built fo path as the first argument');
const executable = path.resolve(driver);
if (executable === path.resolve('/home/ert/.local/bin/fo')) {
  throw new Error('pass the isolated candidate binary, not the global fo install');
}
const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-cancel-error-');
const project = path.join(scratch, 'project');
const env = { ...process.env, HOME: path.join(scratch, 'home'), TMPDIR: '/var/tmp',
  FO_CACHE_DIR: path.join(scratch, 'cache'), FO_PREFIX: path.join(scratch, 'prefix'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'gremlin-state'),
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1' };
const bin = path.join(scratch, 'bin');
const shim = path.join(scratch, 'fail-kill.so');
const injected = path.join(scratch, 'first-kill-failed');
const sleepPidFile = path.join(scratch, 'sleep.pid');
const denyKillFile = path.join(scratch, 'deny-kill');
const killLog = path.join(scratch, 'kill-attempts.log');
let server;
let sleepPid = 0;

function prepare() {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.mkdirSync(bin, { recursive: true });
  fs.mkdirSync(env.HOME, { recursive: true });
  fs.mkdirSync(env.FO_PREFIX, { recursive: true });
  fs.symlinkSync(executable, path.join(bin, 'fo'));
  fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "mcp_cancel_probe"\n');
  fs.writeFileSync(path.join(project, 'test/test_cancel_hold.f90'), [
    'program test_cancel_hold', 'implicit none',
    "call execute_command_line('sleep 60')", 'end program test_cancel_hold', ''
  ].join('\n'));
  const fakeSleep = path.join(bin, 'sleep');
  fs.writeFileSync(fakeSleep, [
    '#!/bin/sh', 'printf "%s\\n" "$$" > "$FO_TEST_SLEEP_PID_FILE"',
    'exec /bin/sleep "$@"', ''
  ].join('\n'));
  fs.chmodSync(fakeSleep, 0o755);
  env.FO_TEST_SLEEP_PID_FILE = sleepPidFile;
  env.FO_TEST_CANCEL_DENY_FILE = denyKillFile;
  env.FO_TEST_CANCEL_FAIL_LOG = killLog;
  const cSource = path.join(scratch, 'fail_kill.c');
  fs.writeFileSync(cSource, [
    '#define _GNU_SOURCE', '#include <dlfcn.h>', '#include <errno.h>',
    '#include <fcntl.h>', '#include <signal.h>', '#include <stdarg.h>',
    '#include <stdio.h>', '#include <sys/syscall.h>',
    '#include <stdlib.h>', '#include <time.h>',
    '#include <sys/types.h>', '#include <unistd.h>',
    'typedef int (*kill_fn)(pid_t, int);',
    'static void record_denied_kill(void) {',
    '  const char *log_path = getenv("FO_TEST_CANCEL_FAIL_LOG");',
    '  struct timespec now;',
    '  char line[96];',
    '  int fd, length;',
    '  if (log_path == NULL || clock_gettime(CLOCK_MONOTONIC, &now) != 0) return;',
    '  length = snprintf(line, sizeof(line), "%ld %llu\\n", (long)getpid(),',
    '      (unsigned long long)now.tv_sec * 1000ULL +',
    '      (unsigned long long)now.tv_nsec / 1000000ULL);',
    '  if (length <= 0 || length >= (int)sizeof(line)) return;',
    '  fd = open(log_path, O_WRONLY | O_CREAT | O_APPEND, 0600);',
    '  if (fd >= 0) { write(fd, line, (size_t)length); close(fd); }',
    '}',
    'static int failed_signals = 0;',
    'static int deny_signal(int sig) {',
    '  const char *enabled = getenv("FO_TEST_CANCEL_FAIL_COUNT");',
    '  const char *deny_file = getenv("FO_TEST_CANCEL_DENY_FILE");',
    '  int failure_limit = enabled == NULL ? 0 : atoi(enabled);',
    '  if (sig != SIGTERM) return 0;',
    '  if (deny_file != NULL && access(deny_file, F_OK) == 0) {',
    '    record_denied_kill(); errno = EPERM; return 1;',
    '  }',
    '  if (enabled != NULL && failed_signals < failure_limit) {',
    '    const char *marker = getenv("FO_TEST_CANCEL_FAIL_MARKER");',
    '    failed_signals++;',
    '    if (failed_signals == 1 && marker != NULL) {',
    '      int fd = open(marker, O_WRONLY | O_CREAT | O_EXCL, 0600);',
    '      if (fd >= 0) close(fd);',
    '    }',
    '    record_denied_kill(); errno = EPERM; return 1;',
    '  }',
    '  return 0;',
    '}',
    'long syscall(long number, ...) {',
    '  static long (*real_syscall)(long, ...) = NULL;',
    '  va_list args;',
    '  long result;',
    '  va_start(args, number);',
    '#if defined(SYS_pidfd_send_signal)',
    '  if (number == SYS_pidfd_send_signal) {',
    '    int fd = va_arg(args, int);',
    '    int sig = va_arg(args, int);',
    '    void *info = va_arg(args, void *);',
    '    unsigned int flags = va_arg(args, unsigned int);',
    '    va_end(args);',
    '    if (deny_signal(sig)) return -1;',
    '    if (real_syscall == NULL) real_syscall = dlsym(RTLD_NEXT, "syscall");',
    '    return real_syscall(number, fd, sig, info, flags);',
    '  }',
    '#endif',
    '#if defined(SYS_pidfd_open)',
    '  if (number == SYS_pidfd_open) {',
    '    pid_t pid = va_arg(args, pid_t);',
    '    unsigned int flags = va_arg(args, unsigned int);',
    '    va_end(args);',
    '    if (real_syscall == NULL) real_syscall = dlsym(RTLD_NEXT, "syscall");',
    '    return real_syscall(number, pid, flags);',
    '  }',
    '#endif',
    '  long a1 = va_arg(args, long), a2 = va_arg(args, long);',
    '  long a3 = va_arg(args, long), a4 = va_arg(args, long);',
    '  long a5 = va_arg(args, long), a6 = va_arg(args, long);',
    '  va_end(args);',
    '  if (real_syscall == NULL) real_syscall = dlsym(RTLD_NEXT, "syscall");',
    '  result = real_syscall(number, a1, a2, a3, a4, a5, a6);',
    '  return result;',
    '}',
    'int kill(pid_t pid, int sig) {',
    '  static kill_fn real_kill = NULL;',
    '  if ((pid > 1 || pid < -1) && deny_signal(sig)) return -1;',
    '  if (real_kill == NULL) real_kill = (kill_fn)dlsym(RTLD_NEXT, "kill");',
    '  if (real_kill == NULL) { errno = ENOSYS; return -1; }',
    '  return real_kill(pid, sig);',
    '}',
    'int killpg(pid_t pgrp, int sig) { return kill(-pgrp, sig); }', ''
  ].join('\n'));
  const compiled = spawnSync('cc', ['-shared', '-fPIC', '-o', shim, cSource, '-ldl'],
    { encoding: 'utf8', maxBuffer: 1024 * 1024 });
  assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
  env.PATH = `${bin}:${env.PATH || process.env.PATH}`;
  env.LD_PRELOAD = shim;
  env.FO_TEST_CANCEL_FAIL_COUNT = '4';
  env.FO_TEST_CANCEL_FAIL_MARKER = injected;
}

function startServer() {
  const child = spawn(executable, ['mcp-server'], { cwd: project, env,
    stdio: ['pipe', 'pipe', 'pipe'] });
  let buffer = '';
  let stderr = '';
  const pending = [];
  child.stdout.on('data', chunk => {
    buffer += chunk.toString();
    let newline;
    while (pending.length && (newline = buffer.indexOf('\n')) >= 0) {
      const body = buffer.slice(0, newline);
      buffer = buffer.slice(newline + 1);
      const waiter = pending.shift();
      clearTimeout(waiter.timer);
      try { waiter.resolve(JSON.parse(body)); } catch (error) { waiter.reject(error); }
    }
  });
  child.stderr.on('data', chunk => { stderr += chunk.toString(); });
  function request(id, method, params = {}) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id, method, params }) + '\n');
    });
  }
  function rpc(id, arguments_) {
    return request(id, 'tools/call', { name: 'fo', arguments: arguments_ });
  }
  function waitForExit(timeoutMs = 5000) {
    if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve();
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        child.removeListener('exit', onExit);
        reject(new Error(`MCP server did not exit: ${stderr}`));
      }, timeoutMs);
      function onExit() {
        clearTimeout(timer);
        resolve();
      }
      child.once('exit', onExit);
    });
  }
  return { child, request, rpc, waitForExit };
}

function body(response) {
  assert.ok(response.result, JSON.stringify(response));
  if (!response.result.content) return { json: response.result, isError: false };
  return { json: JSON.parse(response.result.content[0].text),
    isError: response.result.isError };
}

async function waitForSleepPid(timeoutMs = 20000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(sleepPidFile)) {
      const pid = Number(fs.readFileSync(sleepPidFile, 'utf8').trim());
      if (Number.isInteger(pid) && pid > 1) return pid;
    }
    await new Promise(resolve => setTimeout(resolve, 25));
  }
  let detail = '';
  if (server && server.child.exitCode === null) {
    try {
      detail = JSON.stringify({ status: await server.rpc(81, { action: 'status' }),
        diagnostics: await server.rpc(82, { action: 'diagnostics' }) });
    } catch (error) { detail = String(error); }
  }
  throw new Error(`fixture sleep child did not start: ${detail}`);
}

function pidState(pid) {
  try {
    const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
    const end = stat.lastIndexOf(')');
    return end >= 0 ? stat.slice(end + 1).trimStart()[0] : null;
  } catch (_) {
    return null;
  }
}

async function waitForSleepExit(pid, timeoutMs = 5000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const state = pidState(pid);
    if (state === null || state === 'Z' || state === 'X') return;
    await new Promise(resolve => setTimeout(resolve, 25));
  }
  throw new Error(`fixture sleep child ${pid} remained alive after retry`);
}

function deniedAttemptsFor(callerPid) {
  if (!fs.existsSync(killLog)) return [];
  return fs.readFileSync(killLog, 'utf8').split('\n').filter(Boolean)
    .map(line => line.trim().split(/\s+/).map(Number))
    .filter(parts => parts.length === 2 && parts[0] === callerPid)
    .map(parts => parts[1]);
}

function delay(milliseconds) {
  return new Promise(resolve => setTimeout(resolve, milliseconds));
}

async function verifyEofRetryBackoff() {
  let persistentSleepPid = 0;
  let persistentRunId = 0;
  env.FO_TEST_CANCEL_FAIL_COUNT = '0';
  fs.writeFileSync(denyKillFile, 'deny SIGTERM until this file is removed\n');
  fs.writeFileSync(killLog, '');
  fs.rmSync(sleepPidFile, { force: true });
  server = startServer();
  try {
    await new Promise((resolve, reject) => {
      const timer = setTimeout(
        () => reject(new Error('MCP server did not start')), 5000);
      server.child.once('spawn', () => { clearTimeout(timer); resolve(); });
      server.child.once('error', reject);
    });
    const start = body(await server.rpc(11,
      { action: 'check', mode: 'start', root: project }));
    persistentRunId = start.json.run_id;
    assert.ok(persistentRunId > 0, 'persistent-denial check started');
    persistentSleepPid = await waitForSleepPid();
    assert.ok(pidState(persistentSleepPid) !== null,
      'owned sleeper is live before EOF');

    const deniedCancel = await server.rpc(12,
      { action: 'cancel', run_id: persistentRunId });
    assert.equal(deniedCancel.error.code, -32603,
      'persistent EPERM is explicit before EOF');
    const active = body(await server.rpc(13, { action: 'status' }));
    assert.equal(active.json.state, 'running');
    assert.equal(active.json.run_id, persistentRunId,
      'the active run handle remains available before EOF');

    const beforeEofCount = deniedAttemptsFor(server.child.pid).length;
    server.child.stdin.end();
    const expectedGaps = [100, 200, 400, 800, 1600, 3200, 5000, 5000];
    const attemptsPerCancel = 3;
    const requiredAttempts = attemptsPerCancel * (expectedGaps.length + 1);
    const deadline = Date.now() + 22000;
    let eofAttempts = [];
    while (Date.now() < deadline) {
      eofAttempts = deniedAttemptsFor(server.child.pid).slice(beforeEofCount);
      if (eofAttempts.length >= requiredAttempts) break;
      await delay(25);
    }
    assert.equal(server.child.exitCode, null,
      'EOF does not let the server exit while cancellation is denied');
    assert.equal(server.child.signalCode, null);
    assert.ok(pidState(persistentSleepPid) !== null,
      'owned sleeper remains live while cancellation is denied');
    assert.ok(eofAttempts.length >= requiredAttempts,
      `EOF retry loop made only ${eofAttempts.length} denied attempts`);
    for (let i = 0; i < expectedGaps.length; i++) {
      const endOfCancel = eofAttempts[attemptsPerCancel * (i + 1) - 1];
      const startOfNextCancel = eofAttempts[attemptsPerCancel * (i + 1)];
      const gap = startOfNextCancel - endOfCancel;
      const expected = expectedGaps[i];
      const early = Math.max(75, expected * 0.2);
      const late = Math.max(250, expected * 0.2);
      assert.ok(gap >= expected - early && gap <= expected + late,
        `EOF retry gap ${i + 1} was ${gap} ms; expected ${expected} ms ` +
          `within -${early}/+${late} ms`);
    }

    fs.unlinkSync(denyKillFile);
    await server.waitForExit(10000);
    await waitForSleepExit(persistentSleepPid, 5000);
    assert.equal(server.child.exitCode, 0,
      'server exits normally after cancellation succeeds');
    console.log('mcp-cancel-error: EOF keeps ownership through persistent EPERM, '
      + 'backs off, then cancels and reaps the process tree after release');
  } finally {
    fs.rmSync(denyKillFile, { force: true });
    if (server && server.child.exitCode === null && server.child.signalCode === null) {
      if (!server.child.stdin.destroyed && !server.child.stdin.writableEnded &&
          persistentRunId > 0) {
        try {
          await server.rpc(14, { action: 'cancel', run_id: persistentRunId });
        } catch (_) { /* retry through EOF below */ }
      }
      if (!server.child.stdin.destroyed && !server.child.stdin.writableEnded) {
        server.child.stdin.end();
      }
      try { await server.waitForExit(10000); } catch (_) {
        server.child.kill('SIGTERM');
        try { await server.waitForExit(3000); } catch (_) {
          server.child.kill('SIGKILL');
          await server.waitForExit(3000);
        }
      }
    }
    if (persistentSleepPid > 1) {
      const state = pidState(persistentSleepPid);
      if (state !== null && state !== 'Z' && state !== 'X') {
        try { process.kill(persistentSleepPid, 'SIGTERM'); } catch (_) { /* gone */ }
        try { await waitForSleepExit(persistentSleepPid, 1000); } catch (_) {
          try { process.kill(persistentSleepPid, 'SIGKILL'); } catch (_) { /* gone */ }
          await waitForSleepExit(persistentSleepPid, 1000);
        }
      }
    }
  }
}

async function main() {
  prepare();
  server = startServer();
  let runId = 0;
  try {
    await new Promise((resolve, reject) => {
      const timer = setTimeout(
        () => reject(new Error('MCP server did not start')), 5000);
      server.child.once('spawn', () => { clearTimeout(timer); resolve(); });
      server.child.once('error', reject);
    });
    const start = body(await server.rpc(1, { action: 'check', mode: 'start', root: project }));
    assert.equal(start.isError, false);
    runId = start.json.run_id;
    assert.ok(runId > 0, 'async check started');
    sleepPid = await waitForSleepPid();
    assert.ok(pidState(sleepPid) !== null, 'fixture sleep PID is live before shutdown');

    const failedCancel = await server.rpc(2, { action: 'cancel', run_id: runId });
    assert.ok(fs.existsSync(injected), 'LD_PRELOAD injected EPERM on the first SIGTERM');
    assert.equal(failedCancel.error.code, -32603,
      'provider cancellation error is returned explicitly');
    assert.match(failedCancel.error.message, /ownership remains active/);

    const stillRunning = body(await server.rpc(3, { action: 'status' }));
    assert.equal(stillRunning.json.state, 'running',
      'failed cancellation preserves the active PID and run handle');
    assert.equal(stillRunning.json.run_id, runId);
    assert.ok(pidState(sleepPid) !== null, 'sleep child remains while cancellation failed');

    const failedShutdown = await server.request(4, 'shutdown');
    assert.equal(failedShutdown.error.code, -32603,
      'shutdown reports cancellation failure instead of exiting');
    assert.match(failedShutdown.error.message, /shutdown remains active/);
    const afterShutdownFailure = body(await server.rpc(5, { action: 'status' }));
    assert.equal(afterShutdownFailure.json.state, 'running',
      'failed shutdown also preserves the active PID and run handle');
    assert.equal(afterShutdownFailure.json.run_id, runId);

    const retried = body(await server.rpc(6, { action: 'cancel', run_id: runId }));
    assert.equal(retried.isError, false, 'a later cancellation can complete');
    assert.equal(retried.json.cancelled, true);
    await waitForSleepExit(sleepPid);
    const finished = body(await server.rpc(7, { action: 'status' }));
    assert.equal(finished.json.state, 'finished');
    const shutdown = await server.request(8, 'shutdown');
    assert.equal(shutdown.result, null, 'shutdown completes after cancellation succeeds');
    await server.waitForExit();
    console.log('mcp-cancel-error: first EPERM preserves cancellation ownership; '
      + 'shutdown failure remains active; retry exits the sleeping child and server');
  } finally {
    if (!sleepPid) {
      try {
        if (fs.existsSync(sleepPidFile)) {
          sleepPid = Number(fs.readFileSync(sleepPidFile, 'utf8').trim());
        }
      } catch (_) { /* no fixture PID was recorded */ }
    }
    if (server && runId > 0 && server.child.exitCode === null &&
        server.child.signalCode === null) {
      for (let attempt = 0; attempt < 3; attempt++) {
        try {
          const result = await server.rpc(90 + attempt,
            { action: 'cancel', run_id: runId });
          if (!result.error) break;
        } catch (_) { continue; }
      }
    }
    if (server && server.child.exitCode === null && server.child.signalCode === null) {
      try { await server.request(99, 'shutdown'); } catch (_) { /* forced reap below */ }
      try { await server.waitForExit(5000); } catch (_) {
        server.child.kill('SIGTERM');
        try { await server.waitForExit(5000); } catch (_) {
          server.child.kill('SIGKILL');
          await server.waitForExit(5000);
        }
      }
    }
    if (sleepPid > 1) {
      const state = pidState(sleepPid);
      if (state !== null && state !== 'Z' && state !== 'X') {
        try { process.kill(sleepPid, 'SIGTERM'); } catch (_) { /* already exited */ }
        try { await waitForSleepExit(sleepPid, 1000); } catch (_) {
          try { process.kill(sleepPid, 'SIGKILL'); } catch (_) { /* already exited */ }
          await waitForSleepExit(sleepPid, 1000);
        }
      }
    }
  }
  await verifyEofRetryBackoff();
  fs.rmSync(scratch, { recursive: true, force: true });
}

main().catch(error => {
  console.error(error);
  fs.rmSync(scratch, { recursive: true, force: true });
  process.exitCode = 1;
});
