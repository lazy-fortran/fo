#!/usr/bin/env node
// Black-box generation replacement oracle for Gremlin issue #141.
// Run: node test/test_continuous_preemption.js [/path/to/fo]
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const fo = path.resolve(process.argv[2] || process.env.FO ||
  path.join(__dirname, '..', 'build', 'default', 'app', 'fo'));
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('continuous preemption: skipped (requires Linux /proc)');
  process.exit(0);
}
assert.ok(fs.existsSync(fo), `fo executable not found: ${fo}`);

const scratch = fs.mkdtempSync('/var/tmp/fo-141-preemption-');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg'), FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'state'),
  FO_CACHE_DIR: path.join(scratch, 'cache'), TMPDIR: path.join(scratch, 'tmp'),
  FO_JOBS: '1', FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1' };
fs.mkdirSync(env.HOME, { recursive: true });
fs.mkdirSync(env.TMPDIR, { recursive: true });
const project = path.join(scratch, 'project-a');
const other = path.join(scratch, 'project-other-lane');
const marks = path.join(scratch, 'markers');
fs.mkdirSync(path.join(project, 'test'), { recursive: true });
fs.mkdirSync(path.join(project, 'src'), { recursive: true });
fs.mkdirSync(path.join(other, 'test'), { recursive: true });
fs.mkdirSync(marks);
for (const dir of [project, other]) fs.writeFileSync(path.join(dir, 'fpm.toml'),
  `name = "${path.basename(dir).replaceAll('-', '_')}"\n`);

function q(s) { return s.replaceAll("'", "''"); }
function write(file, text) { fs.writeFileSync(file, text); }
function fifo(name) {
  const file = path.join(scratch, name);
  const made = spawnSync('mkfifo', [file], { encoding: 'utf8' });
  assert.equal(made.status, 0, made.stdout + made.stderr);
  return file;
}
function fortranBlocked(name, gate, started, pidfile, readfile = null, ownpid = null) {
  const out = path.join(marks, `${name}.txt`);
  const child = pidfile ? `call execute_command_line('sh -c ''sleep 300 & echo $! > ${q(pidfile)}; wait''', wait=.false.)\n` : '';
  const waitChild = pidfile ? `do i = 1, 200\n    inquire(file='${q(pidfile)}', exist=exists)\n    if (exists) exit\n    call execute_command_line('sleep 0.05')\nend do\nif (.not. exists) error stop 'descendant did not start'\n` : '';
  const read = readfile ? `open(newunit=u, file='${q(readfile)}', status='old')\nread(u, '(a)') value\nclose(u)\n` : '';
  const value = readfile ? 'character(len=32) :: value\n' : '';
  const marker = readfile ? `write(u, '(a,a,a)') probe_value, ':', trim(value)` : `write(u, '(a)') 'done'`;
  const outputOpen = readfile ?
    `open(newunit=u, file='${q(path.join(marks, name + '-'))}'//probe_value//'.txt', status='replace')` :
    `open(newunit=u, file='${q(out)}', status='replace')`;
  return [
    `program ${name}`, readfile ? 'use probe, only: probe_value' : '',
    'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none',
    'interface', 'function c_getpid() bind(C, name="getpid") result(pid)',
    'import :: c_int', 'integer(c_int) :: pid', 'end function c_getpid', 'end interface',
    'integer :: u, g, i', 'logical :: exists', 'character :: gate_token', value.trimEnd(),
    child.trimEnd(), waitChild.trimEnd(),
    ownpid ? `open(newunit=u, file='${q(ownpid)}', status='replace')` : '',
    ownpid ? "write(u, '(i0)') c_getpid()" : '', ownpid ? 'close(u)' : '',
    `open(newunit=u, file='${q(started)}', status='replace')`, "write(u, '(a)') 'started'", 'close(u)',
    readfile ? 'if (probe_value == "A") then' : '',
    `open(newunit=g, file='${q(gate)}', status='old', access='stream', &`,
    "    form='unformatted', action='read')", 'read(g) gate_token', 'close(g)', readfile ? 'end if' : '', read.trimEnd(),
    outputOpen, marker, 'close(u)', `end program ${name}`, ''
  ].filter(Boolean).join('\n');
}
function status(dir, lane, session) {
  return json(['gremlin', 'status', '--dir', dir, '--lane', lane, '--session', session, '--json']);
}
function run(args, dir, timeout = 90000) {
  const r = spawnSync(fo, args, { cwd: dir, env, encoding: 'utf8', timeout,
    maxBuffer: 8 * 1024 * 1024 });
  return r;
}
function json(args, dir = project) {
  const r = run(args, dir);
  assert.equal(r.status, 0, `${args.join(' ')} failed\n${r.stdout}\n${r.stderr}`);
  return JSON.parse(r.stdout.trim());
}
function start(dir, lane, targets) {
  return json(['gremlin', 'start', '--dir', dir, '--lane', lane, '--random', '1', '--seed', '141',
    '--campaign-seconds', '60', '--timeout-seconds', '5',
    ...targets.flatMap(x => ['--target', x])], dir);
}
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
async function waitFile(file, timeout = 30000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) { if (fs.existsSync(file)) return; await pause(50); }
  throw new Error(`timed out waiting for ${file}`);
}
async function waitState(dir, lane, session, pred, what, timeout = 30000) {
  const end = Date.now() + timeout;
  let s;
  while (Date.now() < end) {
    s = status(dir, lane, session);
    if (pred(s)) return s;
    await pause(100);
  }
  throw new Error(`timed out waiting for ${what}; last state=${JSON.stringify(s)}`);
}
function processIdentity(pid) {
  const fields = fs.readFileSync(`/proc/${pid}/stat`, 'utf8')
    .slice(fs.readFileSync(`/proc/${pid}/stat`, 'utf8').lastIndexOf(')') + 2).trim().split(/\s+/);
  return { pid, start: fields[19] };
}
function alive(id) {
  try {
    const data = fs.readFileSync(`/proc/${id.pid}/stat`, 'utf8');
    const f = data.slice(data.lastIndexOf(')') + 2).trim().split(/\s+/);
    return f[19] === id.start && !['Z', 'X'].includes(f[0]);
  } catch (_) { return false; }
}
async function waitDead(id, timeout = 5000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) { if (!alive(id)) return; await pause(50); }
  throw new Error(`process ${id.pid} survived the cancellation grace period`);
}
async function release(file) {
  const deadline = Date.now() + 1000;
  do {
    let fd;
    try {
      fd = fs.openSync(file, fs.constants.O_WRONLY | fs.constants.O_NONBLOCK);
      fs.writeSync(fd, 'x');
      return;
    } catch (error) {
      if (error.code !== 'ENXIO') throw error;
    } finally {
      if (fd !== undefined) fs.closeSync(fd);
    }
    await pause(10);
  } while (Date.now() < deadline);
  throw new Error(`setup exhausted the five-second case budget: no reader for ${file}`);
}

let sessions = [];
let sentinel = null;
let primaryError = null;
async function main() {
  const aGate = fifo('a-gate');
  const bGate = fifo('b-gate');
  const otherGate = fifo('other-gate');
  const aStarted = path.join(marks, 'a-started');
  const aBlockedPid = path.join(marks, 'a-blocked.pid');
  const otherStarted = path.join(marks, 'other-started');
  const otherPid = path.join(marks, 'other.pid');
  const aProgressStarted = path.join(marks, 'a-progress-started');
  const aProgressPid = path.join(marks, 'a-progress.pid');
  const aProgressGate = fifo('a-progress-gate');
  write(path.join(project, 'token.txt'), 'A\n');
  write(path.join(project, 'src', 'probe.f90'), 'module probe\ncharacter(len=*), parameter :: probe_value = "A"\nend module probe\n');
  write(path.join(project, 'test', 'test_fail.f90'), [
    'program test_fail', "print '(a)', 'issue-141-a-failure'", 'error stop 7', 'end program test_fail', ''
  ].join('\n'));
  write(path.join(project, 'test', 'test_blocked.f90'), fortranBlocked('test_blocked', aGate, aStarted, null, 'token.txt', aBlockedPid));
  write(path.join(project, 'test', 'test_progress.f90'), [
    'program test_progress', 'use probe, only: probe_value', 'implicit none', 'integer :: u, g', 'character :: gate_token',
    'if (probe_value == "A") then',
    `    call execute_command_line('sh -c ''sleep 300 & echo $! > ${q(aProgressPid)}; wait''', wait=.false.)`,
    `    open(newunit=u, file='${q(aProgressStarted)}', status='replace')`, "    write(u, '(a)') probe_value", '    close(u)',
    `    open(newunit=g, file='${q(aProgressGate)}', status='old', access='stream', &`,
    "        form='unformatted', action='read')", '    read(g) gate_token', '    close(g)', 'else',
    `    open(newunit=u, file='${q(path.join(marks, 'b-progress'))}', status='replace')`,
    "    write(u, '(a)') probe_value", '    close(u)', 'end if', 'end program test_progress', ''
  ].join('\n'));
  write(path.join(project, 'test', 'test_capture.f90'), [
    'program test_capture', 'use probe, only: probe_value', 'implicit none', 'integer :: u, g', 'character :: gate_token', 'character(len=32) :: token',
    `open(newunit=u, file='${q(path.join(marks, 'capture-started'))}', status='replace')`,
    "write(u, '(a)') probe_value", 'close(u)',
    "open(newunit=u, file='token.txt', status='old')", "read(u, '(a)') token", 'close(u)',
    `open(newunit=g, file='${q(bGate)}', status='old', access='stream', &`,
    "    form='unformatted', action='read')", 'read(g) gate_token', 'close(g)',
    `open(newunit=u, file='${q(path.join(marks, 'b-result'))}', status='replace')`,
    "write(u, '(a,a,a)') probe_value, ':', trim(token)", 'close(u)', 'end program test_capture', ''
  ].join('\n'));

  write(path.join(other, 'test', 'test_other.f90'), fortranBlocked('test_other', otherGate, otherStarted, otherPid));
  const sentinelFile = path.join(marks, 'sentinel.log');
  sentinel = spawn('sh', ['-c', `while :; do echo alive >> '${sentinelFile}'; sleep 0.1; done`],
    { cwd: scratch, env, stdio: 'ignore' });
  const sentinelIdentity = processIdentity(sentinel.pid);

  const a = start(project, 'issue-141-a', ['test_fail', 'test_blocked', 'test_progress', 'test_capture']);
  sessions.push([project, 'issue-141-a', a.session_id]);
  assert.ok(a.session_id, 'A start returns its owner session');
  await waitFile(aStarted);
  await waitFile(aBlockedPid);
  const blockedIdentity = processIdentity(Number(fs.readFileSync(aBlockedPid, 'utf8').trim()));
  let aState = await waitState(project, 'issue-141-a', a.session_id,
    s => (s.events || []).some(e => e.case_id === 'test_fail') && s.current_test === 'test_blocked',
    'A failure receipt and blocked descendant');
  assert.equal(aState.events.filter(e => e.case_id === 'test_fail').length, 1,
    'A records its nonzero test exactly once');
  const aGeneration = aState.active_generation;
  const aFailure = aState.events.find(e => e.case_id === 'test_fail' && e.generation === aGeneration);
  assert.ok(aFailure, 'A failure is attributed to A generation');
  assert.equal(aState.selected, 4, 'A runs only the four explicitly selected cases');

  // A's current test remains usable while this frozen candidate fails to build.
  write(path.join(project, 'src', 'probe.f90'), 'module broken\nthis is not Fortran\n');
  write(path.join(project, 'token.txt'), 'BROKEN\n');
  await waitState(project, 'issue-141-a', a.session_id,
    s => s.state === 'build_failed' && s.active_generation && s.active_generation !== '',
    'failed intermediate build while A stays active');
  aState = status(project, 'issue-141-a', a.session_id);
  const failedGeneration = aState.candidate_generation;
  const failedCaseName = aState.current_test;
  assert.ok(alive(blockedIdentity), 'A blocked case is alive during failed-build publication');
  assert.equal(aState.active_generation, aGeneration, 'failed candidate retains exact A generation');
  const blockedCwd = fs.readlinkSync(`/proc/${blockedIdentity.pid}/cwd`);
  assert.equal(blockedCwd, aState.active_project,
    'A gated process runs inside the published frozen generation');
  assert.equal(fs.readFileSync(path.join(blockedCwd, 'token.txt'), 'utf8').trim(), 'A',
    'A frozen runtime input is still A before its gate is released');
  fs.writeFileSync(path.join(scratch, 'failed-build-child.json'), JSON.stringify({
    identity: blockedIdentity, alive: alive(blockedIdentity), cwd: blockedCwd,
    argv: fs.readFileSync(`/proc/${blockedIdentity.pid}/cmdline`).toString().split('\0')
  }, null, 2));
  fs.writeFileSync(path.join(scratch, 'failed-build-status.json'), JSON.stringify(aState, null, 2));
  assert.ok(aState.active_generation, 'failed build retains A as last compilable generation');
  // Check status after completing the other contract checks. The exact PID
  // and unreleased FIFO prove A is running at this failed-build publication.
  assert.ok(!fs.existsSync(aProgressStarted), 'A remains blocked until released');
  await release(aGate);
  await waitFile(aProgressStarted);
  await waitFile(aProgressPid);
  const aChild = processIdentity(Number(fs.readFileSync(aProgressPid, 'utf8').trim()));
  assert.ok(alive(aChild), 'A continues its campaign and owns a descendant after the failed build');
  assert.equal(fs.readFileSync(path.join(marks, 'test_blocked-A.txt'), 'utf8').trim(), 'A:A',
    'A reads its frozen runtime input after the live checkout changed');

  const lane = start(other, 'issue-141-other', ['test_other']);
  sessions.push([other, 'issue-141-other', lane.session_id]);
  await waitFile(otherStarted);
  const otherChild = processIdentity(Number(fs.readFileSync(otherPid, 'utf8').trim()));

  // The corrected B source and runtime input form one successful generation.
  write(path.join(project, 'token.txt'), 'B\n');
  write(path.join(project, 'src', 'probe.f90'), 'module probe\ncharacter(len=*), parameter :: probe_value = "B"\nend module probe\n');
  const bGeneration = aState.active_generation;
  const bState = await waitState(project, 'issue-141-a', a.session_id,
    s => s.active_generation && s.active_generation !== bGeneration && s.current_test === 'test_capture',
    'B successful generation to start its selected test');
  assert.equal(bState.selected, 4, 'B retains the bounded four-case selection');
  await waitState(project, 'issue-141-a', a.session_id,
    () => fs.existsSync(path.join(marks, 'capture-started')) &&
      fs.readFileSync(path.join(marks, 'capture-started'), 'utf8').trim() === 'B',
    'B capture executable to reach its gate');
  await waitDead(aChild, 6000);
  assert.equal(fs.readFileSync(sentinelFile, 'utf8').trim().split('\n').length > 2, true,
    'unrelated sentinel continues through A preemption');
  assert.ok(alive(sentinelIdentity), 'unrelated sentinel process survives A preemption');
  assert.ok(alive(otherChild), 'the other lane descendant survives A preemption');
  assert.equal(status(other, 'issue-141-other', lane.session_id).current_test, 'test_other',
    'the other lane remains in its own test');
  const afterPreempt = status(project, 'issue-141-a', a.session_id);
  const failures = afterPreempt.events.filter(e => e.completion_id === aFailure.completion_id);
  assert.ok(!afterPreempt.events.some(e => e.generation === aGeneration &&
    e.case_id === 'test_progress' && e.status === 'TIMEOUT'),
    'setup exhausted the five-second case budget before A preemption');
  assert.equal(failures.length, 1, 'A failure receipt survives successful B replacement exactly once');
  assert.deepEqual(failures[0], aFailure, 'A failure receipt keeps its original fields');
  assert.ok(!afterPreempt.events.some(e => e.case_id === 'test_capture' && e.generation === failedGeneration),
    'failed intermediate build never receives a test verdict');
  assert.ok(!afterPreempt.events.some(e => e.case_id === 'test_progress' && e.generation === bGeneration),
    'preempted A case remains unknown rather than receiving a verdict');

  await release(bGate);
  await waitFile(path.join(marks, 'b-result'));
  assert.equal(fs.readFileSync(path.join(marks, 'b-result'), 'utf8').trim(), 'B:B',
    'B test uses its compiled source and matching frozen runtime input');
  const completedB = await waitState(project, 'issue-141-a', a.session_id,
    s => s.events.some(e => e.case_id === 'test_capture' &&
      e.generation === bState.active_generation && e.status === 'PASS'),
    'B capture PASS receipt');
  fs.writeFileSync(path.join(scratch, 'completed-b-status.json'), JSON.stringify(completedB, null, 2));
  assert.equal(fs.readFileSync(path.join(marks, 'test_blocked-A.txt'), 'utf8').trim(), 'A:A',
    'no obsolete A rerun reads the live B input');

  assert.equal(fs.readFileSync(path.join(marks, 'test_blocked-B.txt'), 'utf8').trim(), 'B:B',
    'B mandatory case reads its own frozen runtime input');

  // Explicit stop is bounded and leaves no interrupted verdict for the other lane.
  const stopStarted = Date.now();
  const stop = json(['gremlin', 'stop', '--dir', other, '--lane', 'issue-141-other',
    '--session', lane.session_id, '--json'], other);
  assert.ok(stop.cancelled || stop.stopped || stop.state, 'shutdown returns a structured response');
  assert.ok(Date.now() - stopStarted < 6000, 'shutdown returns within the process grace bound');
  await waitDead(otherChild, 6000);
  const otherEvents = status(other, 'issue-141-other', lane.session_id).events || [];
  assert.equal(otherEvents.filter(e => e.case_id === 'test_other').length, 0,
    'shutdown leaves the interrupted case without a fabricated receipt');
  assert.ok(alive(sentinelIdentity), 'unrelated sentinel survives explicit shutdown');
  console.log('continuous preemption: frozen runtime, replacement, receipts, and scoped cancellation passed');
  assert.equal(failedCaseName, 'test_blocked',
    'failed-build publication retains the still-running A test name');
  console.log('continuous preemption: failed-build status progress passed');
}

main().catch(error => { primaryError = error; }).finally(async () => {
  for (const [dir, lane, session] of sessions) {
    try { run(['gremlin', 'stop', '--dir', dir, '--lane', lane, '--session', session, '--json'], dir, 10000); }
    catch (_) { /* owner may already have exited */ }
  }
  if (sentinel) { try { sentinel.kill('SIGTERM'); } catch (_) {} }
  if (primaryError) {
    process.stderr.write(`${primaryError.stack || primaryError}\nScratch: ${scratch}\n`);
    process.exitCode = 1;
  }
});
