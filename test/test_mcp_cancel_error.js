#!/usr/bin/env node
// Inject one kill(2) failure and verify MCP retains async cancellation ownership.
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
let server;

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
  const cSource = path.join(scratch, 'fail_kill.c');
  fs.writeFileSync(cSource, [
    '#define _GNU_SOURCE', '#include <dlfcn.h>', '#include <errno.h>',
    '#include <fcntl.h>', '#include <signal.h>', '#include <stdlib.h>',
    '#include <sys/types.h>', '#include <unistd.h>',
    'typedef int (*kill_fn)(pid_t, int);',
    'int kill(pid_t pid, int sig) {',
    '  static int failed = 0;',
    '  static kill_fn real_kill = NULL;',
    '  const char *enabled = getenv("FO_TEST_CANCEL_FAIL_ONCE");',
    '  if (sig == SIGTERM && pid > 1 && enabled != NULL && ',
    '      enabled[0] == \'1\' && !failed) {',
    '    const char *marker = getenv("FO_TEST_CANCEL_FAIL_MARKER");',
    '    failed = 1;',
    '    if (marker != NULL) {',
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
  env.FO_TEST_CANCEL_FAIL_ONCE = '1';
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
  function rpc(id, arguments_) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id, method: 'tools/call',
        params: { name: 'fo', arguments: arguments_ } }) + '\n');
    });
  }
  function shutdown() {
    if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve();
    return new Promise(resolve => {
      const onExit = () => {
        clearTimeout(termTimer);
        clearTimeout(killTimer);
        resolve();
      };
      child.once('exit', onExit);
      child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id: 99, method: 'shutdown' }) + '\n');
      const termTimer = setTimeout(() => {
        if (child.exitCode === null && child.signalCode === null) child.kill('SIGTERM');
      }, 5000);
      const killTimer = setTimeout(() => {
        if (child.exitCode === null && child.signalCode === null) child.kill('SIGKILL');
      }, 10000);
    });
  }
  return { child, rpc, shutdown };
}

function body(response) {
  assert.ok(response.result, JSON.stringify(response));
  return { json: JSON.parse(response.result.content[0].text),
    isError: response.result.isError };
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

    const failedCancel = await server.rpc(2, { action: 'cancel', run_id: runId });
    assert.ok(fs.existsSync(injected), 'LD_PRELOAD injected EPERM on the first SIGTERM');
    assert.equal(failedCancel.error.code, -32603,
      'provider cancellation error is returned explicitly');
    assert.match(failedCancel.error.message, /ownership remains active/);

    const stillRunning = body(await server.rpc(3, { action: 'status' }));
    assert.equal(stillRunning.json.state, 'running',
      'failed cancellation preserves the active PID and run handle');
    assert.equal(stillRunning.json.run_id, runId);

    const retried = body(await server.rpc(4, { action: 'cancel', run_id: runId }));
    assert.equal(retried.isError, false, 'a later cancellation can complete');
    assert.equal(retried.json.cancelled, true);
    const finished = body(await server.rpc(5, { action: 'status' }));
    assert.equal(finished.json.state, 'finished');
    console.log('mcp-cancel-error: injected EPERM is explicit, ownership remains '
      + 'active, and retry cancels the same run');
  } finally {
    if (server && runId > 0 && server.child.exitCode === null) {
      for (let attempt = 0; attempt < 2; attempt++) {
        try {
          const result = await server.rpc(90 + attempt,
            { action: 'cancel', run_id: runId });
          if (!result.error) break;
        } catch (_) { break; }
      }
    }
    if (server) await server.shutdown();
    fs.rmSync(scratch, { recursive: true, force: true });
  }
}

main().catch(error => {
  console.error(error);
  if (server) server.child.kill('SIGTERM');
  fs.rmSync(scratch, { recursive: true, force: true });
  process.exitCode = 1;
});
