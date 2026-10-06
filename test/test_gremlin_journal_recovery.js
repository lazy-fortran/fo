#!/usr/bin/env node
// Behavioral crash-recovery fixture for a live Gremlin owner and its journal.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const fo = process.argv[2] || process.env.FO || 'fo';
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin journal recovery: skipped (requires Linux /proc)');
  process.exit(0);
}

const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-journal-recovery-');
const project = path.join(scratch, 'project');
const markerRoot = path.join(scratch, 'markers');
const lane = 'journal-recovery';
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'gremlin-state'),
  FO_CACHE_DIR: path.join(scratch, 'cache'), TMPDIR: path.join(scratch, 'tmp'), FO_JOBS: '1',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1' };
fs.mkdirSync(env.HOME, { recursive: true });
fs.mkdirSync(env.TMPDIR, { recursive: true });
fs.mkdirSync(path.join(project, 'test'), { recursive: true });
fs.mkdirSync(markerRoot, { recursive: true });
fs.writeFileSync(path.join(project, 'fpm.toml'),
  'name = "gremlin_journal_recovery_probe"\n');

function run(args, timeout = 90000) {
  return spawnSync(fo, args, { cwd: project, env, encoding: 'utf8', timeout,
    maxBuffer: 8 * 1024 * 1024 });
}

function json(args) {
  const result = run(args);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}

function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function createGate(file) {
  const result = spawnSync('mkfifo', [file], { encoding: 'utf8' });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return file;
}

// Intercept completed durability operations, rather than racing startup sleeps.
// Only the explicitly armed process and exact lane owner record can block.
function buildCrashBarrier() {
  const source = path.join(scratch, 'recovery_barrier.c');
  const library = path.join(scratch, 'recovery_barrier.so');
  fs.writeFileSync(source, String.raw`
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
static int published = 0;
static char session[128];
static void barrier(void) {
    const char *ready = getenv("RECOVERY_BARRIER_READY");
    const char *gate = getenv("RECOVERY_BARRIER_GATE");
    char text[64];
    int n = snprintf(text, sizeof(text), "%ld\n", (long)getpid());
    int fd = open(ready, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd < 0 || write(fd, text, (size_t)n) != n) _exit(120);
    close(fd);
    fd = open(gate, O_RDONLY);
    if (fd < 0) _exit(121);
    char byte;
    (void)read(fd, &byte, 1);
    close(fd);
}
int rename(const char *oldpath, const char *newpath) {
    int (*real_rename)(const char *, const char *) = dlsym(RTLD_NEXT, "rename");
    int result = real_rename(oldpath, newpath);
    const char *owner = getenv("RECOVERY_BARRIER_OWNER");
    if (result == 0 && owner && strcmp(owner, newpath) == 0) {
        FILE *file = fopen(owner, "r");
        if (!file || !fgets(session, sizeof(session), file)) _exit(122);
        fclose(file);
        session[strcspn(session, "\n")] = 0;
        published = 1;
    }
    return result;
}
int fsync(int fd) {
    int (*real_fsync)(int) = dlsym(RTLD_NEXT, "fsync");
    int result = real_fsync(fd);
    const char *owner = getenv("RECOVERY_BARRIER_OWNER");
    const char *mode = getenv("RECOVERY_BARRIER_MODE");
    if (result != 0 || !owner || !mode || !published) return result;
    char link[64], path[PATH_MAX], expected[PATH_MAX];
    snprintf(link, sizeof(link), "/proc/self/fd/%d", fd);
    ssize_t n = readlink(link, path, sizeof(path) - 1);
    if (n < 0) _exit(123);
    path[n] = 0;
    snprintf(expected, sizeof(expected), "%s", owner);
    char *slash = strrchr(expected, '/');
    if (!slash) _exit(124);
    *slash = 0;
    int hit = strcmp(mode, "owner") == 0 && strcmp(path, expected) == 0;
    if (strcmp(mode, "import") == 0) {
        snprintf(expected, sizeof(expected), "/%s/journal.jsonl", session);
        size_t a = strlen(path), b = strlen(expected);
        hit = a >= b && strcmp(path + a - b, expected) == 0;
    }
    if (hit) { published = 0; barrier(); }
    return result;
}
`);
  const compiled = spawnSync('cc', ['-shared', '-fPIC', source, '-ldl', '-o', library],
    { env, encoding: 'utf8' });
  assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
  return library;
}

async function interruptRestart(library, mode) {
  const ready = path.join(markerRoot, `restart-${mode}.ready`);
  const gate = createGate(path.join(markerRoot, `restart-${mode}.fifo`));
  const child = spawn(fo, ['gremlin', mode === 'owner' ? 'start' : 'run',
    '--dir', project, '--lane', lane,
    '--random-count', '32', '--seed', '1729', '--target', 'test_blocked'], {
    cwd: project, stdio: 'ignore', env: { ...env, LD_PRELOAD: library,
      RECOVERY_BARRIER_OWNER: ownerFile(), RECOVERY_BARRIER_MODE: mode,
      RECOVERY_BARRIER_READY: ready, RECOVERY_BARRIER_GATE: gate }
  });
  const closed = new Promise(resolve => child.once('close', resolve));
  try {
    await waitForFile(ready, 30000);
    const pid = Number(fs.readFileSync(ready, 'utf8').trim());
    assert.equal(pid, child.pid, 'barrier belongs to this exact restarting process');
    const identity = processIdentity(pid);
    const owner = fs.readFileSync(ownerFile(), 'utf8').trim().split('\n');
    assert.equal(Number(owner[1]), pid, 'replacement owner pointer is published');
    assert.equal(owner[2], identity.startTime, 'replacement owner retains PID identity');
    if (mode === 'import') {
      function journals(dir) {
        return fs.readdirSync(dir, { withFileTypes: true }).flatMap(entry => {
          const file = path.join(dir, entry.name);
          return entry.isDirectory() ? journals(file) : [file];
        });
      }
      const files = journals(path.join(env.FO_GREMLIN_STATE_DIR,
        'fo', 'gremlin', 'session-journals'));
      const journal = files.find(file => path.basename(path.dirname(file)) === owner[0]);
      assert.ok(journal, 'interrupted owner has a destination journal');
      const records = fs.readFileSync(journal, 'utf8').trim().split('\n').map(JSON.parse);
      assert.equal(records.length, 1, 'import is interrupted after its first durable receipt');
      assert.equal(records[0].session_id, firstSession,
        'the first imported receipt retains its original session identity');
    }
    process.kill(pid, 'SIGKILL');
    await closed;
    await waitForOwnerExit(identity, 10000);
    console.log(`Gremlin journal recovery: interrupted ${mode} durability barrier`);
  } finally {
    if (child.exitCode === null && child.signalCode === null) {
      child.kill('SIGKILL');
      await closed;
    }
  }
}

function fortranString(file) {
  return file.replaceAll("'", "''");
}

function writePassingCase() {
  const marker = fortranString(path.join(markerRoot, 'pass.done'));
  fs.writeFileSync(path.join(project, 'test/test_pass.f90'), [
    'program test_pass', 'integer :: unit',
    `open(newunit=unit, file='${marker}', status='replace')`,
    "write(unit, '(a)') 'done'", 'close(unit)', 'end program test_pass', ''
  ].join('\n'));
}

function writeFailingCase() {
  const marker = fortranString(path.join(markerRoot, 'fail.started'));
  fs.writeFileSync(path.join(project, 'test/test_fail.f90'), [
    'program test_fail', 'integer :: unit',
    "print '(a)', 'journal-recovery-fail-token-4e812a'",
    `open(newunit=unit, file='${marker}', status='replace')`,
    "write(unit, '(a)') 'started'", 'close(unit)', 'error stop 7',
    'end program test_fail', ''
  ].join('\n'));
}

function writeBlockedCase(gate, pidPath, donePath) {
  const gatePath = fortranString(gate);
  const pidMarker = fortranString(pidPath);
  const doneMarker = fortranString(donePath);
  fs.writeFileSync(path.join(project, 'test/test_blocked.f90'), [
    'program test_blocked',
    'use, intrinsic :: iso_c_binding, only: c_int',
    'implicit none',
    'interface',
    '    function c_getpid() bind(C, name="getpid") result(pid)',
    '        import :: c_int', '        integer(c_int) :: pid',
    '    end function c_getpid', 'end interface',
    'integer :: unit, gate_unit', 'character :: token',
    `open(newunit=unit, file='${pidMarker}', status='replace')`,
    "write(unit, '(i0)') c_getpid()", 'close(unit)',
    `open(newunit=gate_unit, file='${gatePath}', status='old', &`,
    "    access='stream', form='unformatted', action='read')",
    'read(gate_unit) token', 'close(gate_unit)',
    `open(newunit=unit, file='${doneMarker}', status='replace')`,
    "write(unit, '(a)') 'done'", 'close(unit)',
    'end program test_blocked', ''
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

function ownerFile() {
  const canonical = fs.realpathSync(project);
  const root = path.join(env.FO_GREMLIN_STATE_DIR, 'fo', 'gremlin', 'projects',
    fnv64(canonical), fnv64(lane));
  return path.join(root, 'owner');
}

function processIdentity(pid) {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
  const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
  return { pid, state: fields[0], startTime: fields[19] };
}

function processAlive(identity) {
  try {
    const current = processIdentity(identity.pid);
    return current.startTime === identity.startTime &&
      !['Z', 'X'].includes(current.state);
  } catch (_) {
    return false;
  }
}

function ownerIdentity(sessionId) {
  const lines = fs.readFileSync(ownerFile(), 'utf8').trim().split('\n');
  assert.equal(lines[0], sessionId, 'state owner record matches the started session');
  assert.equal(lines[3], fs.realpathSync(project), 'owner record names this project');
  assert.equal(lines[4], lane, 'owner record names this lane');
  const identity = { pid: Number(lines[1]), startTime: lines[2] };
  const current = processIdentity(identity.pid);
  assert.equal(current.startTime, identity.startTime,
    'owner PID still has the recorded process start identity');
  assert.ok(!['Z', 'X'].includes(current.state), 'owner is a live process');
  const argv = fs.readFileSync(`/proc/${identity.pid}/cmdline`).toString()
    .split('\0').filter(Boolean);
  const command = argv.indexOf('gremlin');
  assert.ok(command >= 0 && argv[command + 1] === 'run',
    'recorded process is the Gremlin supervisor');
  assert.equal(argv[argv.indexOf('--dir') + 1], fs.realpathSync(project));
  const laneFlag = argv.indexOf('--lane-id') >= 0 ? '--lane-id' : '--lane';
  assert.equal(argv[argv.indexOf(laneFlag) + 1], lane);
  return { ...identity, sessionId };
}

async function waitForFile(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) return;
    await wait(50);
  }
  throw new Error(`timed out waiting for ${file}`);
}

async function waitForOwnerExit(identity, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (!processAlive(identity)) return;
    await wait(50);
  }
  throw new Error(`owner PID ${identity.pid} remained alive`);
}

function start(targets) {
  return json(['gremlin', 'start', '--dir', project, '--lane', lane,
    '--random-count', '32', '--seed', '1729', '--campaign-seconds', '60',
    '--timeout-seconds', '5', ...targets.flatMap(name => ['--target', name])]);
}

function status(sessionId) {
  return json(['gremlin', 'status', '--dir', project, '--lane', lane,
    '--session', sessionId, '--detail', 'full', '--json']);
}

function assertReceiptSet(events, sessionId) {
  const passes = events.filter(event => event.case_id === 'test_pass');
  const failures = events.filter(event => event.case_id === 'test_fail');
  assert.equal(passes.length, 1, 'completed pass receipt appears exactly once');
  assert.equal(failures.length, 1, 'completed fail receipt appears exactly once');
  assert.equal(passes[0].status, 'PASS');
  assert.equal(failures[0].status, 'FAIL');
  for (const receipt of [...passes, ...failures]) {
    assert.equal(receipt.session_id, sessionId,
      'recovered receipt retains its original owner session');
    assert.ok(receipt.completion_id, 'receipt keeps its unique completion identity');
    assert.ok(path.isAbsolute(receipt.log_path), 'receipt references an output artifact');
    assert.ok(fs.statSync(receipt.log_path).isFile(),
      'referenced output artifact survives owner recovery');
  }
  assert.equal(new Set(events.map(event => event.completion_id)).size, events.length,
    'recovered journal has no duplicate completion IDs');
  assert.equal(events.filter(event => event.case_id === 'test_blocked').length, 0,
    'interrupted blocked case has no completion receipt');
}

function artifactSnapshots(events) {
  const snapshots = new Map();
  for (const receipt of events.filter(event =>
    event.case_id === 'test_pass' || event.case_id === 'test_fail')) {
    const bytes = fs.readFileSync(receipt.log_path);
    if (receipt.case_id === 'test_fail') {
      assert.ok(bytes.includes(Buffer.from('journal-recovery-fail-token-4e812a')),
        'completed failure output contains its unique token');
    }
    snapshots.set(receipt.case_id, { path: receipt.log_path, bytes });
  }
  assert.equal(snapshots.size, 2, 'both completed artifacts are snapshotted');
  return snapshots;
}

function assertArtifactsUnchanged(events, snapshots) {
  for (const receipt of events.filter(event =>
    event.case_id === 'test_pass' || event.case_id === 'test_fail')) {
    const before = snapshots.get(receipt.case_id);
    assert.ok(before, `${receipt.case_id} was present before the crash`);
    assert.equal(receipt.log_path, before.path,
      `${receipt.case_id} retains its original artifact reference`);
    assert.deepEqual(fs.readFileSync(receipt.log_path), before.bytes,
      `${receipt.case_id} artifact bytes survive the restarted owner`);
  }
}

let firstSession = '';
let restartedSession = '';
let ownerToStop = null;
let completedArtifacts = null;
const workerIdentities = [];
let primaryError = null;

async function main() {
  const barrierLibrary = buildCrashBarrier();
  const firstGate = createGate(path.join(markerRoot, 'blocked-before-crash.fifo'));
  const firstPidFile = path.join(markerRoot, 'blocked-before-crash.pid');
  const firstDone = path.join(markerRoot, 'blocked-before-crash.done');
  const secondGate = createGate(path.join(markerRoot, 'blocked-after-recovery.fifo'));
  const secondPidFile = path.join(markerRoot, 'blocked-after-recovery.pid');
  const secondDone = path.join(markerRoot, 'blocked-after-recovery.done');
  writePassingCase();
  writeFailingCase();
  writeBlockedCase(firstGate, firstPidFile, firstDone);

  try {
    const first = start(['test_fail', 'test_pass', 'test_blocked']);
    firstSession = first.session_id;
    assert.ok(firstSession, 'start returns a real owner session');
    ownerToStop = ownerIdentity(firstSession);
    await waitForFile(firstPidFile, 30000);
    const firstWorkerPid = Number(fs.readFileSync(firstPidFile, 'utf8').trim());
    workerIdentities.push(processIdentity(firstWorkerPid));

    let firstStatus;
    const firstDeadline = Date.now() + 30000;
    do {
      firstStatus = status(firstSession);
      const cases = new Set((firstStatus.events || []).map(event => event.case_id));
      if (cases.has('test_pass') && cases.has('test_fail') &&
          firstStatus.current_test === 'test_blocked') break;
      await wait(100);
    } while (Date.now() < firstDeadline);
    const firstEvents = firstStatus.events || [];
    assertReceiptSet(firstEvents, firstSession);
    assert.equal(firstStatus.current_test, 'test_blocked',
      'owner is blocked in the third selected case');
    assert.equal(firstStatus.completed, 2, 'only pass and fail completed');
    completedArtifacts = artifactSnapshots(firstEvents);

    process.kill(ownerToStop.pid, 'SIGKILL');
    await waitForOwnerExit(ownerToStop, 10000);
    ownerToStop = null;
    assert.ok(!fs.existsSync(firstDone),
      'the test blocked at owner crash has no completion marker');

    // Keep only the blocked test in the new generation so no old case reruns.
    fs.renameSync(path.join(project, 'test/test_pass.f90'),
      path.join(project, 'test/test_pass.disabled'));
    fs.renameSync(path.join(project, 'test/test_fail.f90'),
      path.join(project, 'test/test_fail.disabled'));
    writeBlockedCase(secondGate, secondPidFile, secondDone);
    await interruptRestart(barrierLibrary, 'owner');
    if (!process.argv.includes('--handoff-only')) {
      await interruptRestart(barrierLibrary, 'import');
    }
    const restarted = start(['test_blocked']);
    restartedSession = restarted.session_id;
    assert.ok(restartedSession && restartedSession !== firstSession,
      'stale owner recovery creates a new session on the same lane');
    ownerToStop = ownerIdentity(restartedSession);
    await waitForFile(secondPidFile, 30000);
    workerIdentities.push(processIdentity(
      Number(fs.readFileSync(secondPidFile, 'utf8').trim())));

    const deadline = Date.now() + 30000;
    let recovered;
    do {
      recovered = status(restartedSession);
      if (recovered.current_test === 'test_blocked' &&
          (recovered.events || []).some(event => event.case_id === 'test_fail')) break;
      await wait(100);
    } while (Date.now() < deadline);
    assert.equal(recovered.current_test, 'test_blocked',
      'restarted owner remains on the interrupted case');
    assertReceiptSet(recovered.events || [], firstSession);
    assertArtifactsUnchanged(recovered.events || [], completedArtifacts);
    assert.equal(recovered.completed, 0,
      'the restarted blocked case has not completed');
    assert.ok(!fs.existsSync(firstDone) && !fs.existsSync(secondDone),
      'neither blocked execution reached its completion marker');
    console.log('Gremlin journal recovery: crash receipts and unknown case passed');
  } catch (error) {
    primaryError = error;
  } finally {
    const sessionsToStop = [...new Set([
      ownerToStop && ownerToStop.sessionId, restartedSession, firstSession
    ].filter(Boolean))];
    for (const sessionId of sessionsToStop) {
      try {
        run(['gremlin', 'stop', '--dir', project, '--lane', lane,
          '--session', sessionId, '--json']);
      } catch (_) { /* owner may already have exited */ }
    }
    if (ownerToStop) {
      const deadline = Date.now() + 10000;
      while (Date.now() < deadline && processAlive(ownerToStop)) await wait(50);
    }
    for (const identity of workerIdentities) {
      if (processAlive(identity)) {
        try { process.kill(identity.pid, 'SIGTERM'); } catch (_) { /* exited */ }
        await waitForOwnerExit(identity, 3000).catch(() => {});
      }
    }
  }
  if (primaryError) throw primaryError;
}

main().then(() => {
  function makeWritableTree(target) {
    if (!fs.existsSync(target)) return;
    const stats = fs.lstatSync(target);
    if (stats.isSymbolicLink()) return;
    fs.chmodSync(target, stats.mode | 0o700);
    if (stats.isDirectory()) {
      for (const entry of fs.readdirSync(target)) {
        makeWritableTree(path.join(target, entry));
      }
    }
  }
  makeWritableTree(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
}).catch(error => {
  console.error(error);
  console.error(`scratch preserved for diagnosis: ${scratch}`);
  process.exitCode = 1;
});
