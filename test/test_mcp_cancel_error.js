#!/usr/bin/env node
// Inject repeated kill(2) failures and verify MCP retains ownership through shutdown.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const driver = process.argv[2] || process.env.FO;
if (!driver) throw new Error('pass the newly built fo path as the first argument');
const executable = path.resolve(driver);
const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-cancel-error-');
const project = path.join(scratch, 'project');
const env = { ...process.env, HOME: path.join(scratch, 'home'), TMPDIR: '/var/tmp',
  FO_CACHE_DIR: path.join(scratch, 'cache') };
const bin = path.join(scratch, 'bin');
const shim = path.join(scratch, 'fail-kill.so');
const injected = path.join(scratch, 'first-kill-failed');
const sleepPidFile = path.join(scratch, 'sleep.pid');
let server;
let sleepPid = 0;

function prepare() {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.mkdirSync(bin, { recursive: true });
  fs.mkdirSync(env.HOME, { recursive: true });
  fs.symlinkSync(executable, path.join(bin, 'fo'));
  fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "mcp_cancel_probe"\n');
  fs.writeFileSync(path.join(project, 'test/test_cancel_slow.f90'), [
    'program test_cancel_slow', 'implicit none',
    "call execute_command_line('sleep 60')", 'end program test_cancel_slow', ''
  ].join('\n'));
  const fakeSleep = path.join(bin, 'sleep');
  fs.writeFileSync(fakeSleep, [
    '#!/bin/sh', 'printf "%s\\n" "$$" > "$FO_TEST_SLEEP_PID_FILE"',
    'exec /bin/sleep "$@"', ''
  ].join('\n'));
  fs.chmodSync(fakeSleep, 0o755);
  env.FO_TEST_SLEEP_PID_FILE = sleepPidFile;
  const cSource = path.join(scratch, 'fail_kill.c');
  fs.writeFileSync(cSource, [
    '#define _GNU_SOURCE', '#include <dlfcn.h>', '#include <errno.h>',
    '#include <fcntl.h>', '#include <signal.h>', '#include <stdlib.h>',
    '#include <sys/types.h>', '#include <unistd.h>',
    'typedef int (*kill_fn)(pid_t, int);',
    'int kill(pid_t pid, int sig) {',
    '  static int failed = 0;',
    '  static kill_fn real_kill = NULL;',
    '  const char *enabled = getenv("FO_TEST_CANCEL_FAIL_COUNT");',
    '  int failure_limit = enabled == NULL ? 0 : atoi(enabled);',
    '  if (sig == SIGTERM && pid > 1 && enabled != NULL && ',
    '      failed < failure_limit) {',
    '    const char *marker = getenv("FO_TEST_CANCEL_FAIL_MARKER");',
    '    failed++;',
    '    if (failed == 1 && marker != NULL) {',
    '      int fd = open(marker, O_WRONLY | O_CREAT | O_EXCL, 0600);',
    '      if (fd >= 0) close(fd);',
    '    }',
    '    errno = EPERM; return -1;',
    '  }',
    '  if (real_kill == NULL) real_kill = (kill_fn)dlsym(RTLD_NEXT, "kill");',
    '  if (real_kill == NULL) { errno = ENOSYS; return -1; }',
    '  return real_kill(pid, sig);',
    '}', ''
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
  throw new Error('fixture sleep child did not start');
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

async function main() {
  prepare();
  server = startServer();
  let runId = 0;
  try {
    await new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('MCP server did not start')), 5000);
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
    fs.rmSync(scratch, { recursive: true, force: true });
  }
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
