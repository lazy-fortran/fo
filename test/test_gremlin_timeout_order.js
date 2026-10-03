#!/usr/bin/env node
// Behavioral oracle for timeout ordering after a synchronous capture stalls.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const foArg = process.argv[2] || process.env.FO || 'fo';
const foPath = foArg.includes('/') ? path.resolve(foArg) :
  spawnSync('which', [foArg], { encoding: 'utf8' }).stdout.trim();
const fo = fs.realpathSync(foPath);
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin timeout ordering: skipped (requires Linux /proc and flock)');
  process.exit(0);
}
const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-timeout-order-');
const project = path.join(scratch, 'project');
const markers = path.join(scratch, 'markers');
const stateRoot = path.join(scratch, 'gremlin-state');
const lane = 'timeout-order';
const controlLane = 'timeout-order-control';
const timeoutSeconds = 5;
const compilerDir = path.join(scratch, 'compiler-bin');
const realCompiler = spawnSync('which', ['gfortran'], { encoding: 'utf8' }).stdout.trim();
assert.ok(realCompiler, 'gfortran is available');
const versionHold = path.join(markers, 'hold-version');
const versionRelease = path.join(markers, 'release-version');
const versionEntered = path.join(markers, 'version-entered');
const wrapper = path.join(compilerDir, 'gfortran');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_PREFIX: path.join(scratch, 'prefix'), FO_GREMLIN_STATE_DIR: stateRoot,
  FO_CACHE_DIR: path.join(scratch, 'cache'), TMPDIR: scratch, FO_JOBS: '1',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1',
  FO_TIMEOUT_VERSION_HOLD: versionHold,
  FO_TIMEOUT_VERSION_RELEASE: versionRelease,
  FO_TIMEOUT_VERSION_ENTERED: versionEntered,
  PATH: `${compilerDir}:${path.dirname(fo)}:${process.env.PATH || ''}` };
fs.mkdirSync(env.HOME, { recursive: true });
fs.mkdirSync(path.join(project, 'test'), { recursive: true });
fs.mkdirSync(markers, { recursive: true });
fs.mkdirSync(compilerDir, { recursive: true });
fs.writeFileSync(path.join(project, 'fpm.toml'),
  'name = "gremlin_timeout_order_probe"\n');
const shellQuote = value => `'${value.replaceAll("'", "'\\''")}'`;
fs.writeFileSync(wrapper, [
  '#!/bin/sh',
  `compiler=${shellQuote(realCompiler)}`,
  'if [ "$1" = "--version" ] && [ -e "$FO_TIMEOUT_VERSION_HOLD" ]; then',
  '  printf "%s\\n" "$$" > "$FO_TIMEOUT_VERSION_ENTERED"',
  '  while [ ! -e "$FO_TIMEOUT_VERSION_RELEASE" ]; do sleep 0.02; done',
  'fi',
  'exec "$compiler" "$@"', ''
].join('\n'));
fs.chmodSync(wrapper, 0o755);

function run(args, timeout = 30000) {
  return spawnSync(fo, args, { cwd: project, env, encoding: 'utf8', timeout,
    maxBuffer: 8 * 1024 * 1024 });
}
function json(args) {
  const result = run(args);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}
function wait(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }
async function waitFor(predicate, description, timeoutMs = 30000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const value = predicate();
    if (value) return value;
    await wait(20);
  }
  throw new Error(`timed out waiting for ${description}`);
}
function quoteFortran(value) { return value.replaceAll("'", "''"); }
function writeTestSource(startedFile, pidFile, bypassFile, gateFile, doneFile) {
  const [started, pid, bypass, gate, done] =
    [startedFile, pidFile, bypassFile, gateFile, doneFile].map(quoteFortran);
  fs.writeFileSync(path.join(project, 'test/test_timeout_order.f90'), [
    'program test_timeout_order',
    'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none',
    'interface', '    function c_getpid() bind(C, name="getpid") result(pid)',
    '        import :: c_int', '        integer(c_int) :: pid',
    '    end function c_getpid', 'end interface',
    'integer :: unit, gate_unit', 'character :: token', 'logical :: bypass',
    `open(newunit=unit, file='${started}', status='replace')`,
    "write(unit, '(a)') 'started'", 'close(unit)',
    `open(newunit=unit, file='${pid}', status='replace')`,
    "write(unit, '(i0)') c_getpid()", 'close(unit)',
    `inquire(file='${bypass}', exist=bypass)`, 'if (.not. bypass) then',
    `    open(newunit=gate_unit, file='${gate}', status='old', &`,
    "        access='stream', form='unformatted', action='read')",
    '    read(gate_unit) token', '    close(gate_unit)', 'end if',
    `open(newunit=unit, file='${done}', status='replace')`,
    "write(unit, '(a)') 'done'", 'close(unit)', 'end program test_timeout_order', ''
  ].join('\n'));
}
function fnv64(value) {
  let hash = 1469598103934665603n;
  for (const byte of Buffer.from(value)) {
    hash ^= BigInt(byte);
    hash = BigInt.asUintN(64, hash * 1099511628211n);
  }
  return hash.toString(16).padStart(16, '0');
}
function ownerFile(laneId) {
  return path.join(stateRoot, 'fo', 'gremlin', 'projects',
    fnv64(fs.realpathSync(project)), fnv64(laneId), 'owner');
}
function ownerIdentity(laneId, sessionId) {
  const lines = fs.readFileSync(ownerFile(laneId), 'utf8').trim().split('\n');
  assert.equal(lines[0], sessionId, 'owner file names the requested session');
  return { pid: Number(lines[1]), startTime: lines[2] };
}
function processInfo(pid) {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
  const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
  return { state: fields[0], startTime: fields[19],
    wchan: fs.readFileSync(`/proc/${pid}/wchan`, 'utf8').trim() };
}
function processAlive(identity) {
  try {
    const current = processInfo(identity.pid);
    return current.startTime === identity.startTime && !['Z', 'X'].includes(current.state);
  } catch (_) { return false; }
}
function status(laneId, sessionId) {
  return json(['gremlin', 'status', '--dir', project, '--lane', laneId,
    '--session', sessionId, '--json']);
}
function start(laneId) {
  return json(['gremlin', 'start', '--dir', project, '--lane', laneId,
    '--target', 'test_timeout_order', '--seed', '1729', '--campaign-seconds',
    '60', '--timeout-seconds', String(timeoutSeconds)]);
}
async function stop(laneId, sessionId, identity) {
  if (!sessionId) return;
  const result = run(['gremlin', 'stop', '--dir', project, '--lane', laneId,
    '--session', sessionId, '--json']);
  if (result.status === 0 && identity) {
    await waitFor(() => !processAlive(identity), `owner ${identity.pid} to exit`, 10000);
  }
}
function versionWaitsForOwner(identity) {
  if (!fs.existsSync(versionEntered)) return false;
  const pid = Number(fs.readFileSync(versionEntered, 'utf8').trim());
  if (!Number.isSafeInteger(pid) || !processAlive(identity)) return false;
  try {
    const status = fs.readFileSync(`/proc/${pid}/status`, 'utf8');
    return Number(status.match(/^PPid:\s*(\d+)$/m)?.[1]) === identity.pid &&
      fs.readFileSync(`/proc/${pid}/cmdline`, 'utf8').includes('--version');
  } catch (_) { return false; }
}
function releaseGate(gateFile) {
  return new Promise((resolve, reject) => {
    const writer = spawn(process.execPath, ['-e',
      "require('node:fs').writeFileSync(process.argv.at(-1), 'x')", gateFile],
    { stdio: 'ignore' });
    const timer = setTimeout(() => {
      writer.kill('SIGKILL');
      reject(new Error('timed out releasing test gate'));
    }, 3000);
    writer.once('error', error => { clearTimeout(timer); reject(error); });
    writer.once('close', code => {
      clearTimeout(timer);
      code === 0 ? resolve() : reject(new Error(`test gate writer exited ${code}`));
    });
  });
}

async function main() {
  const startedFile = path.join(markers, 'test.started');
  const pidFile = path.join(markers, 'test.pid');
  const bypassFile = path.join(markers, 'unused.bypass');
  const gateFile = path.join(markers, 'test.gate');
  const doneFile = path.join(markers, 'test.done');
  assert.equal(spawnSync('mkfifo', [gateFile]).status, 0, 'create test gate');
  writeTestSource(startedFile, pidFile, bypassFile, gateFile, doneFile);

  let session = '';
  let owner = null;
  let controlSession = '';
  let controlOwner = null;
  let primaryError = null;
  try {
    const result = start(lane);
    session = result.session_id;
    assert.ok(session, 'target start returns a session');
    owner = ownerIdentity(lane, session);
    await waitFor(() => fs.existsSync(startedFile) && fs.existsSync(pidFile),
      'first test to start');
    const startedAt = Date.now();
    const testPid = Number(fs.readFileSync(pidFile, 'utf8').trim());
    const activeGeneration = status(lane, session).active_generation;
    assert.equal(activeGeneration?.length, 64, 'test runs on a built generation');
    fs.writeFileSync(versionHold, 'hold the next synchronous capture');
    await waitFor(() => versionWaitsForOwner(owner),
      'owner capture to wait on gfortran --version', 5000);
    assert.ok(Date.now() - startedAt < (timeoutSeconds - 1) * 1000,
      'capture blocks before the test timeout deadline');

    await releaseGate(gateFile);
    await waitFor(() => fs.existsSync(doneFile), 'short test to finish', 5000);
    await waitFor(() => {
      try { return ['Z', 'X'].includes(processInfo(testPid).state); }
      catch (_) { return true; }
    }, 'completed test child to exit', 5000);
    await wait(Math.max(0, startedAt + timeoutSeconds * 1000 + 400 - Date.now()));
    assert.ok(versionWaitsForOwner(owner), 'owner remains in synchronous capture');
    assert.equal((status(lane, session).events || []).filter(event =>
      event.case_id === 'test_timeout_order').length, 0,
    'child remains unclassified until synchronous capture returns');

    fs.writeFileSync(versionRelease, 'release');
    const receipt = await waitFor(() => (status(lane, session).events || [])
      .find(event => event.case_id === 'test_timeout_order') || null,
    'completed test receipt after capture returns', 10000);
    assert.equal(status(lane, session).active_generation, activeGeneration,
      'capture preserved the same source generation');
    assert.equal(receipt.status, 'PASS',
      `completed child was misclassified after capture delay: ${receipt.status}`);
    await stop(lane, session, owner);
    session = '';

    for (const file of [startedFile, pidFile, doneFile]) fs.unlinkSync(file);
    const control = start(controlLane);
    controlSession = control.session_id;
    assert.ok(controlSession, 'control start returns a session');
    controlOwner = ownerIdentity(controlLane, controlSession);
    await waitFor(() => fs.existsSync(startedFile) && fs.existsSync(pidFile),
      'timeout control test to start');
    const controlPid = Number(fs.readFileSync(pidFile, 'utf8').trim());
    assert.ok(processAlive({ pid: controlPid, startTime: processInfo(controlPid).startTime }),
      'timeout control child is alive before its deadline');
    assert.ok(!fs.existsSync(doneFile), 'timeout control remains blocked on its gate');
    const timedOut = await waitFor(() => (status(controlLane, controlSession).events || [])
      .find(event => event.case_id === 'test_timeout_order') || null,
    'genuine timeout control receipt', 15000);
    assert.equal(timedOut.status, 'TIMEOUT',
      `blocked control case must time out: ${timedOut.status}`);
    assert.equal(timedOut.exitcode, 124, 'timeout receipt uses exit code 124');
    assert.ok(!fs.existsSync(doneFile), 'timed-out child did not finish');
    console.log('Gremlin timeout ordering: completed child PASS; blocked child TIMEOUT');
  } catch (error) {
    primaryError = error;
  } finally {
    fs.writeFileSync(versionRelease, 'cleanup');
    const cleanupErrors = [];
    for (const [laneId, sessionId, identity] of [
      [lane, session, owner], [controlLane, controlSession, controlOwner]
    ]) {
      if (!sessionId) continue;
      try { await stop(laneId, sessionId, identity); }
      catch (error) { cleanupErrors.push(error); }
    }
    if (primaryError) {
      if (cleanupErrors.length) primaryError.message += `; cleanup: ${cleanupErrors.join('; ')}`;
      throw primaryError;
    }
    assert.deepEqual(cleanupErrors, [], cleanupErrors.join('\n'));
  }
}
main().catch(error => {
  console.error(error.stack || error);
  process.exitCode = 1;
});
