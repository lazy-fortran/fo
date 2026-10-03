#!/usr/bin/env node
// Black-box proof that a live generation keeps the exact driver bytes it captured.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

const realFo = process.argv[2] || process.env.FO;
if (!realFo) throw new Error('pass the fo executable as the first argument');
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin driver pin: skipped (requires Linux async process containment)');
  process.exit(0);
}
const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-driver-pin-');
const publicDir = path.join(scratch, 'public');
const publicFo = path.join(publicDir, 'fo');
const fixture = path.join(scratch, 'project');
const gate = path.join(scratch, 'gate');
const started = path.join(scratch, 'gate.started');
const markerA = path.join(scratch, 'driver-a.calls');
const markerB = path.join(scratch, 'driver-b.calls');
const env = { ...process.env, REAL_FO: path.resolve(realFo),
  HOME: path.join(scratch, 'home'), XDG_CACHE_HOME: path.join(scratch, 'xdg'),
  FO_CACHE_DIR: path.join(scratch, 'cache'), FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'state'), FO_JOBS: '1',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1', TMPDIR: '/var/tmp',
  DRIVER_A_MARKER: markerA, DRIVER_B_MARKER: markerB };
fs.mkdirSync(publicDir, { recursive: true });
fs.mkdirSync(fixture, { recursive: true });
fs.mkdirSync(env.HOME, { recursive: true });

function writeWrapper(file, tag) {
  const marker = tag === 'A' ? 'DRIVER_A_MARKER' : 'DRIVER_B_MARKER';
  const script = '#!/bin/bash\nprintf \'%s\\n\' \' ' + tag + '\' >> \"$' + marker + '\"\n' +
    'exec -a \"$0\" \"$REAL_FO\" \"$@\"\n';
  fs.writeFileSync(file, script.replace("' " + tag + "'", "'" + tag + "'"), { mode: 0o755 });
  fs.chmodSync(file, 0o755);
}
function run(args, cwd = fixture, timeout = 30000) {
  return spawnSync(publicFo, args, { cwd, env, encoding: 'utf8', timeout,
    maxBuffer: 8 * 1024 * 1024 });
}
function json(args, cwd = fixture) {
  const result = run(args, cwd);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}
function wait(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }
async function waitFor(check, description, timeout = 30000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) {
    const value = check();
    if (value) return value;
    await wait(50);
  }
  throw new Error('timed out waiting for ' + description);
}
function testSource(name, before = []) {
  return ['program ' + name, 'implicit none', 'integer :: unit', ...before,
    "open(newunit=unit, file='" + path.join(scratch, name + '.done') + "', status='replace')",
    "write(unit, '(a)') 'done'", 'close(unit)', 'end program ' + name, ''].join('\n');
}
function status(lane, session) {
  const result = spawnSync(env.REAL_FO, ['gremlin', 'status', '--dir', fixture,
    '--lane', lane, '--session', session, '--json'], { cwd: fixture, env,
    encoding: 'utf8', timeout: 30000, maxBuffer: 8 * 1024 * 1024 });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}
async function waitEvent(lane, session, caseId) {
  return waitFor(() => (status(lane, session).events || []).find(event =>
    event.case_id === caseId && event.status === 'PASS'), 'PASS receipt for ' + caseId);
}
async function stop(lane, session) {
  if (status(lane, session).state === 'stopped') return;
  const result = spawnSync(env.REAL_FO, ['gremlin', 'stop', '--dir', fixture,
    '--lane', lane, '--session', session, '--json'], { cwd: fixture, env,
    encoding: 'utf8', timeout: 30000, maxBuffer: 8 * 1024 * 1024 });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  await waitFor(() => status(lane, session).state === 'stopped', lane + ' stop');
}
function countLines(file) {
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8').trim().split('\n').filter(Boolean).length : 0;
}
function digest(file) {
  return crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
}
function releaseFifo(file) {
  return new Promise((resolve, reject) => {
    const writer = spawn(process.execPath, ['-e',
      "require('node:fs').writeFileSync(process.argv.at(-1), 'x')", file], { stdio: 'ignore' });
    const timer = setTimeout(() => { writer.kill('SIGKILL'); reject(new Error('FIFO release timed out')); }, 3000);
    writer.once('error', error => { clearTimeout(timer); reject(error); });
    writer.once('close', code => {
      clearTimeout(timer);
      code === 0 ? resolve() : reject(new Error('FIFO writer exited ' + code));
    });
  });
}

let passed = false;
(async () => {
  try {
    writeWrapper(publicFo, 'A');
    fs.writeFileSync(path.join(fixture, 'fpm.toml'), 'name = "gremlin_driver_pin_probe"\n');
    fs.mkdirSync(path.join(fixture, 'test'));
    const fifo = spawnSync('mkfifo', [gate]);
    assert.equal(fifo.status, 0, fifo.stderr);
    const beforeGate = [
      "open(newunit=unit, file='" + started + "', status='replace')",
      "write(unit, '(a)') 'started'", 'close(unit)',
      "open(newunit=unit, file='" + gate + "', status='old', access='stream', form='unformatted', action='read')",
      'read(unit) token', 'close(unit)'
    ];
    fs.writeFileSync(path.join(fixture, 'test/test_gate.f90'),
      testSource('test_gate', ['character :: token', ...beforeGate]));
    fs.writeFileSync(path.join(fixture, 'test/test_after.f90'), testSource('test_after'));

    const startedRun = run(['gremlin', 'start', '--dir', fixture, '--lane', 'driver-a',
      '--target', 'test_gate', '--target', 'test_after', '--seed', '17',
      '--campaign-seconds', '60', '--timeout-seconds', '5']);
    assert.equal(startedRun.status, 0, startedRun.stdout + startedRun.stderr);
    const first = JSON.parse(startedRun.stdout.trim());
    await waitFor(() => fs.existsSync(started), 'gated first test start');
    await waitFor(() => status('driver-a', first.session_id).driver_digest, 'driver digest publication');
    const digestA = digest(publicFo);
    const callsBeforeSwap = countLines(markerA);
    assert.ok(callsBeforeSwap > 0, 'driver A ran before replacement');
    const replacement = path.join(publicDir, 'fo.next');
    writeWrapper(replacement, 'B');
    fs.renameSync(replacement, publicFo);
    await releaseFifo(gate);
    const gateReceipt = await waitEvent('driver-a', first.session_id, 'test_gate');
    const afterReceipt = await waitEvent('driver-a', first.session_id, 'test_after');
    assert.equal(gateReceipt.driver_digest, digestA);
    assert.equal(afterReceipt.driver_digest, digestA);
    assert.equal(afterReceipt.generation, gateReceipt.generation);
    assert.equal(countLines(markerB), 0, 'old generation never executed driver B');
    assert.ok(countLines(markerA) > callsBeforeSwap, 'second case used pinned driver A');
    await stop('driver-a', first.session_id);

    const second = json(['gremlin', 'start', '--dir', fixture, '--lane', 'driver-b',
      '--target', 'test_after', '--seed', '18', '--campaign-seconds', '60',
      '--timeout-seconds', '5']);
    const callsAfterStartB = countLines(markerB);
    const secondReceipt = await waitEvent('driver-b', second.session_id, 'test_after');
    assert.equal(secondReceipt.driver_digest, digest(publicFo));
    assert.ok(countLines(markerB) > callsAfterStartB, 'new generation executes driver B');
    assert.notEqual(secondReceipt.generation, afterReceipt.generation,
      'driver replacement changes generation execution identity');
    assert.equal(status('driver-b', second.session_id).driver_digest, digest(publicFo));
    await stop('driver-b', second.session_id);
    console.log('Gremlin driver pin: live generation used A after atomic replacement; new session used B');
    passed = true;
  } finally {
    if (passed) {
      try { fs.rmSync(scratch, { recursive: true, force: true }); } catch (_) { /* fixture cleanup */ }
    }
  }
})().catch(error => {
  console.error(error);
  console.error('fixture retained at ' + scratch);
  process.exitCode = 1;
});
