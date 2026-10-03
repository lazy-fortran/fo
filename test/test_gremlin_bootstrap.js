#!/usr/bin/env node
// Black-box fixture for immutable generations and the owned Gremlin supervisor.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const fo = process.argv[2] || process.env.FO || 'fo';
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin bootstrap: skipped (requires Linux async process containment)');
  process.exit(0);
}
const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-bootstrap-');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'gremlin-state'),
  TMPDIR: '/var/tmp', FO_CACHE_DIR: path.join(scratch, 'cache'), FO_JOBS: '1',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1' };
fs.mkdirSync(env.HOME, { recursive: true });

function run(args, cwd) {
  return spawnSync(fo, args, { cwd, env, encoding: 'utf8', timeout: 30000,
    maxBuffer: 8 * 1024 * 1024 });
}

function runAsync(args, cwd, onResponse) {
  return new Promise((resolve, reject) => {
    const child = spawn(fo, args, { cwd, env, stdio: ['ignore', 'pipe', 'pipe'] });
    let stdout = '';
    let stderr = '';
    let responseReceived = false;
    const timer = setTimeout(() => {
      child.kill('SIGKILL');
      reject(new Error(`command timed out: ${args.join(' ')}`));
    }, 30000);
    child.stdout.setEncoding('utf8').on('data', chunk => {
      stdout += chunk;
      if (responseReceived || !stdout.includes('\n')) return;
      responseReceived = true;
      if (onResponse) {
        try { onResponse(JSON.parse(stdout.slice(0, stdout.indexOf('\n')))); }
        catch (_) { /* the final parse reports malformed output after cleanup */ }
      }
    });
    child.stderr.setEncoding('utf8').on('data', chunk => { stderr += chunk; });
    child.once('error', error => { clearTimeout(timer); reject(error); });
    child.once('close', (status, signal) => {
      clearTimeout(timer);
      resolve({ status, signal, stdout, stderr });
    });
  });
}

function json(args, cwd) {
  const result = run(args, cwd);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}

function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function waitFor(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) return;
    await wait(50);
  }
  throw new Error(`timed out waiting for ${file}`);
}

function createGate(file) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  const result = spawnSync('mkfifo', [file], { encoding: 'utf8' });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return file;
}

function releaseGate(file) {
  return new Promise((resolve, reject) => {
    const writer = spawn(process.execPath, ['-e',
      "require('node:fs').writeFileSync(process.argv.at(-1), 'x')", file],
    { stdio: 'ignore' });
    const timer = setTimeout(() => {
      writer.kill('SIGKILL');
      reject(new Error(`timed out releasing test gate ${file}`));
    }, 3000);
    writer.once('error', error => { clearTimeout(timer); reject(error); });
    writer.once('close', code => {
      clearTimeout(timer);
      if (code === 0) resolve();
      else reject(new Error(`gate writer exited ${code}: ${file}`));
    });
  });
}

function markerStatement(file, value) {
  const literal = file.replaceAll("'", "''");
  return [
    `open(newunit=unit, file='${literal}', status='unknown', position='append')`,
    `write(unit, '(a)') '${value}'`, 'close(unit)'
  ].join('\n');
}

function runtimeVersionStatement(markerFile) {
  const literal = markerFile.replaceAll("'", "''");
  return [
    "open(newunit=unit, file='test/obsolete.version', status='old', action='read')",
    "read(unit, '(a)') version_value", 'close(unit)',
    `open(newunit=unit, file='${literal}', status='unknown', position='append')`,
    "write(unit, '(a)') trim(version_value)", 'close(unit)'
  ].join('\n');
}

function programSource(name, marker, gate, beforeGate = '', pidMarker = '') {
  const body = [];
  const gateLiteral = gate.replaceAll("'", "''");
  if (beforeGate) body.push(beforeGate);
  body.push(`open(newunit=gate_unit, file='${gateLiteral}', status='old', &`,
    "    access='stream', form='unformatted', action='read')",
    'read(gate_unit) gate_token', 'close(gate_unit)');
  const pidPath = pidMarker && pidMarker.replaceAll("'", "''");
  const pidUse = pidMarker ? ['use iso_c_binding, only: c_int'] : [];
  const pidInterface = pidMarker ? [
    'interface',
    '  function c_getpid() bind(C, name="getpid") result(value)',
    '    import :: c_int',
    '    integer(c_int) :: value',
    '  end function c_getpid',
    'end interface'
  ] : [];
  const pidBody = pidMarker ? [
    `open(newunit=unit, file='${pidPath}', status='replace')`,
    "write(unit, '(i0)') c_getpid()", 'close(unit)'
  ] : [];
  return [
    `program ${name}`,
    ...pidUse,
    'implicit none',
    ...pidInterface,
    'integer :: unit',
    'integer :: gate_unit',
    'character :: gate_token',
    'character(len=64) :: version_value',
    markerStatement(marker, 'started'),
    ...pidBody,
    ...body,
    markerStatement(marker, 'done'),
    `end program ${name}`,
    ''
  ].join('\n');
}

function writeProject(project, markerRoot, version, invalid = false) {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.mkdirSync(markerRoot, { recursive: true });
  fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "gremlin_bootstrap_probe"\n');
  const invalidSource = path.join(project, 'src/gremlin_invalid.f90');
  if (invalid) {
    fs.mkdirSync(path.dirname(invalidSource), { recursive: true });
    fs.writeFileSync(invalidSource, [
      'module gremlin_invalid', 'implicit none',
      'integer, parameter :: invalid_value = "not an integer"',
      'end module gremlin_invalid', ''
    ].join('\n'));
  } else if (fs.existsSync(invalidSource)) {
    fs.unlinkSync(invalidSource);
    try { fs.rmdirSync(path.dirname(invalidSource)); } catch (_) { /* other source remains */ }
  }
  const mainGate = createGate(path.join(markerRoot, `${version}.test_generation.gate`));
  const source = programSource('test_generation',
    path.join(markerRoot, `${version}.events`), mainGate);
  fs.writeFileSync(path.join(project, 'test/test_generation.f90'), source);
  const obsoleteGate = createGate(path.join(markerRoot, `${version}.test_obsolete.gate`));
  fs.writeFileSync(path.join(project, 'test/test_obsolete.f90'),
    programSource('test_obsolete', path.join(markerRoot, `${version}.obsolete.events`),
      obsoleteGate,
      runtimeVersionStatement(path.join(markerRoot, `${version}.obsolete.version`)),
      path.join(markerRoot, `${version}.obsolete.pid`)));
  fs.writeFileSync(path.join(project, 'test/obsolete.version'), `${version}\n`);
  const laneGate = createGate(path.join(markerRoot, `${version}.test_lane_b.gate`));
  fs.writeFileSync(path.join(project, 'test/test_lane_b.f90'),
    programSource('test_lane_b', path.join(markerRoot, `${version}.lane.events`), laneGate));
}

function startArgs(project, lane, targets) {
  return ['gremlin', 'start', '--dir', project, '--lane', lane,
    ...targets.flatMap(target => ['--target', target]), '--seed', '1729',
    '--campaign-seconds', '60', '--timeout-seconds', '5'];
}

function startLane(project, lane, target = 'test_generation') {
  return json(startArgs(project, lane, [target]), project);
}

function status(project, lane, sessionId, cursor) {
  const args = ['gremlin', 'status', '--dir', project, '--lane', lane,
    '--session', sessionId, '--json'];
  if (cursor !== undefined) args.push('--cursor', String(cursor));
  return json(args, project);
}

function eventLines(file) {
  if (!fs.existsSync(file)) return [];
  return fs.readFileSync(file, 'utf8').trim().split('\n').filter(Boolean);
}

function processIdentity(pid) {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
  const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
  return { pid, state: fields[0], startTime: fields[19] };
}

function processIdentityAlive(identity) {
  try {
    const current = processIdentity(identity.pid);
    return current.startTime === identity.startTime && !['Z', 'X'].includes(current.state);
  } catch (_) {
    return false;
  }
}

async function waitForProcessExit(identity, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (!processIdentityAlive(identity)) return;
    await wait(50);
  }
  throw new Error(`obsolete test process ${identity.pid} remained alive`);
}

async function waitForPidMarker(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) {
      const value = fs.readFileSync(file, 'utf8').trim();
      if (/^[1-9]\d*$/.test(value)) return Number(value);
    }
    await wait(50);
  }
  throw new Error(`timed out waiting for process identity in ${file}`);
}

async function waitForEventLines(file, marker, count, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (eventLines(file).filter(value => value === marker).length >= count) return;
    await wait(50);
  }
  throw new Error(`timed out waiting for ${count} ${marker} events in ${file}`);
}

async function waitForEvent(project, lane, sessionId, predicate, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let latest = [];
  while (Date.now() < deadline) {
    const value = status(project, lane, sessionId, 0);
    latest = Array.isArray(value.events) ? value.events : [];
    const found = latest.find(predicate);
    if (found) return found;
    await wait(100);
  }
  throw new Error(`timed out waiting for event; latest=${JSON.stringify(latest)}`);
}

async function waitForStopped(project, lane, sessionId, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const value = status(project, lane, sessionId);
    if (value.state === 'stopped') return;
    await wait(100);
  }
  throw new Error(`lane ${lane} did not stop`);
}

async function collectEvents(project, lane, sessionId, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const first = status(project, lane, sessionId, 0);
    const events = Array.isArray(first.events) ? [...first.events] : [];
    let cursor = first.next_cursor || 0;
    let more = first.has_more === true;
    while (more) {
      const page = status(project, lane, sessionId, cursor);
      events.push(...(Array.isArray(page.events) ? page.events : []));
      cursor = page.next_cursor || cursor;
      more = page.has_more === true && cursor !== 0;
    }
    if (events.length > 0) return events;
    await wait(100);
  }
  return [];
}

async function stopLane(project, lane, sessionId) {
  const args = ['gremlin', 'stop', '--dir', project, '--lane', lane];
  if (sessionId) {
    if (status(project, lane, sessionId).state === 'stopped') {
      await waitForOwnerExit(project, lane, 10000);
      return;
    }
    args.push('--session', sessionId);
  }
  args.push('--json');
  const stopped = run(args, project);
  assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
  const owner = JSON.parse(stopped.stdout.trim());
  const stoppedId = owner.session_id || sessionId;
  if (stoppedId) await waitForStopped(project, lane, stoppedId, 10000);
  await waitForOwnerExit(project, lane, 10000);
}

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

function gremlinOwnerPids(project, lane) {
  const owners = [];
  for (const entry of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(entry)) continue;
    try {
      const args = fs.readFileSync(`/proc/${entry}/cmdline`).toString().split('\0');
      const command = args.indexOf('gremlin');
      const laneAt = args.indexOf('--lane-id') >= 0
        ? args.indexOf('--lane-id') : args.indexOf('--lane');
      const dirAt = args.indexOf('--dir');
      if (command >= 0 && args[command + 1] === 'run' &&
          laneAt >= 0 && args[laneAt + 1] === lane &&
          dirAt >= 0 && args[dirAt + 1] === project) owners.push(Number(entry));
    } catch (_) { /* process exited during scan */ }
  }
  return owners;
}

function ownerProcessGroup(pid) {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
  const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
  return { group: Number(fields[2]), session: Number(fields[3]) };
}

async function waitForOwnerExit(project, lane, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (gremlinOwnerPids(project, lane).length === 0) return;
    await wait(50);
  }
  throw new Error(`Gremlin owner remained after cleanup: ${lane}`);
}

async function forceStopOwnedGremlinGroup(project, lane) {
  const signalOwners = signal => {
    for (const pid of gremlinOwnerPids(project, lane)) {
      const { group, session } = ownerProcessGroup(pid);
      assert.equal(group, pid, `refusing to signal non-owner process group ${pid}`);
      assert.equal(session, pid, `refusing to signal non-owner session ${pid}`);
      try { process.kill(-group, signal); }
      catch (error) { if (error.code !== 'ESRCH') throw error; }
    }
  };
  signalOwners('SIGTERM');
  try { await waitForOwnerExit(project, lane, 1500); return; }
  catch (_) { /* escalate only within the verified owner group */ }
  signalOwners('SIGKILL');
  await waitForOwnerExit(project, lane, 5000);
}

async function main() {
  const project = path.join(scratch, 'project');
  const markerRoot = path.join(scratch, 'markers');
  writeProject(project, markerRoot, 'old');
  const oldMarker = path.join(markerRoot, 'old.events');
  const obsoleteMarker = path.join(markerRoot, 'old.obsolete.events');
  const obsoletePidMarker = path.join(markerRoot, 'old.obsolete.pid');
  const newMarker = path.join(markerRoot, 'new.events');
  const obsoleteGate = path.join(markerRoot, 'old.test_obsolete.gate');
  const newGate = path.join(markerRoot, 'new.test_generation.gate');
  const laneBGate = path.join(markerRoot, 'new.test_lane_b.gate');
  const cleanupGates = [
    [path.join(markerRoot, 'old.test_generation.gate'), oldMarker],
    [obsoleteGate, obsoleteMarker],
    [newGate, newMarker],
    [laneBGate, path.join(markerRoot, 'new.lane.events')]
  ];
  let sessionA = '';
  let sessionB = '';
  let obsoleteIdentity = null;
  let primaryError;
  try {
    const rememberOwner = value => { if (!sessionA) sessionA = value.session_id; };
    const pair = await Promise.all([
      runAsync(startArgs(project, 'replacement',
        ['test_generation', 'test_obsolete']), project, rememberOwner),
      runAsync(startArgs(project, 'replacement',
        ['test_generation', 'test_obsolete']), project, rememberOwner)
    ]);
    for (const result of pair) assert.equal(result.status, 0, result.stdout + result.stderr);
    const starts = pair.map(result => JSON.parse(result.stdout.trim()));
    assert.ok(starts.every(value => ['attached', 'running'].includes(value.state)),
      'simultaneous same-place starts return the shared active session state');
    sessionA = starts[0].session_id;
    assert.ok(sessionA);
    assert.equal(starts[1].session_id, sessionA,
      'simultaneous starts return the same owner ID');
    assert.ok(sessionA, 'start returns an owner ID before the campaign completes');
    await waitForEventLines(oldMarker, 'started', 1, 30000);
    const firstStatus = status(project, 'replacement', sessionA);
    assert.ok(firstStatus.active_generation, 'status publishes the frozen generation');

    // A failed replacement build must leave the active frozen test running.
    writeProject(project, markerRoot, 'invalid', true);
    await waitForEvent(project, 'replacement', sessionA, event =>
      event.case_id === '<build>' && event.status === 'BUILD_FAIL', 30000);
    assert.equal(eventLines(oldMarker).includes('done'), false,
      'the old test is still running when the replacement build fails');
    assert.equal(eventLines(oldMarker).filter(value => value === 'started').length, 1,
      'simultaneous starts launch one copy of the first case');
    await releaseGate(path.join(markerRoot, 'old.test_generation.gate'));
    await waitForEventLines(oldMarker, 'done', 1, 30000);
    await waitForEvent(project, 'replacement', sessionA, event =>
      event.case_id === 'test_generation' &&
      event.generation === firstStatus.active_generation && event.status === 'PASS',
    30000);
    assert.equal(eventLines(oldMarker).filter(value => value === 'done').length, 1,
      'the failed candidate does not duplicate the old test receipt');
    const firstGenerationEvents = await collectEvents(project, 'replacement', sessionA, 1000);
    assert.equal(firstGenerationEvents.filter(event =>
      event.case_id === 'test_generation' &&
      event.generation === firstStatus.active_generation && event.status === 'PASS').length, 1,
    'one concurrent start produces exactly one pass receipt for the first case');
    await waitForEventLines(obsoleteMarker, 'started', 1, 30000);
    obsoleteIdentity = processIdentity(await waitForPidMarker(obsoletePidMarker, 30000));
    assert.ok(processIdentityAlive(obsoleteIdentity),
      'the obsolete test child is blocked in the first generation');
    await waitFor(path.join(markerRoot, 'old.obsolete.version'), 30000);
    assert.deepEqual(eventLines(path.join(markerRoot, 'old.obsolete.version')), ['old'],
      'a later case reads runtime input from the captured generation after live edits');
    assert.equal(fs.readFileSync(path.join(project, 'test/obsolete.version'), 'utf8').trim(),
      'invalid', 'the live source tree changed before the later case ran');
    assert.equal(fs.existsSync(path.join(markerRoot, 'invalid.obsolete.version')), false,
      'the in-flight generation does not execute the replacement source');

    // A newer valid generation starts while the old test is still active.
    writeProject(project, markerRoot, 'new');
    await waitForEventLines(newMarker, 'started', 1, 30000);
    const secondStatus = status(project, 'replacement', sessionA);
    assert.notEqual(secondStatus.active_generation, firstStatus.active_generation,
      'successful replacement activates a new immutable generation');
    await waitForProcessExit(obsoleteIdentity, 5000);
    assert.equal(eventLines(obsoleteMarker).includes('done'), false,
      'successful replacement terminates the still-running obsolete test');
    assert.equal(eventLines(obsoleteMarker).filter(value => value === 'started').length, 1,
      'simultaneous starts do not duplicate a later case');
    await releaseGate(newGate);
    await waitForEvent(project, 'replacement', sessionA, event =>
      event.case_id === 'test_generation' &&
      event.generation === secondStatus.active_generation && event.status === 'PASS',
    30000);
    const replacementEvents = await collectEvents(project, 'replacement', sessionA, 1000);
    assert.ok(!replacementEvents.some(event =>
      event.case_id === 'test_obsolete' &&
      event.generation === firstStatus.active_generation && event.status === 'PASS'),
    'obsolete generation does not receive a fabricated pass receipt');

    // A second lane continues while the first lane is stopped.
    const second = startLane(project, 'other', 'test_lane_b');
    assert.equal(second.state, 'running');
    sessionB = second.session_id;
    const laneBMarker = path.join(markerRoot, 'new.lane.events');
    await waitForEventLines(laneBMarker, 'started', 1, 30000);
    const secondLaneStatus = status(project, 'other', sessionB);
    const stopA = run(['gremlin', 'stop', '--dir', project, '--lane',
      'replacement', '--session', sessionA, '--json'], project);
    assert.equal(stopA.status, 0, stopA.stdout + stopA.stderr);
    await waitForStopped(project, 'replacement', sessionA, 5000);
    assert.equal(eventLines(laneBMarker).includes('done'), false,
      'lane B remains active after lane A stops');
    await releaseGate(laneBGate);
    await waitForEventLines(laneBMarker, 'done', 1, 30000);
    await waitForEvent(project, 'other', sessionB, event =>
      event.case_id === 'test_lane_b' &&
      event.generation === secondLaneStatus.active_generation && event.status === 'PASS',
    10000);

    console.log('Gremlin bootstrap: atomic attach, failed/new generations, '
      + 'durable receipts, and lane-scoped stop passed');
  } catch (error) {
    primaryError = error;
  } finally {
    await Promise.all(cleanupGates.map(async ([gate, marker]) => {
      if (!fs.existsSync(marker)) return;
      try { await releaseGate(gate); } catch (_) { /* no reader or owner */ }
    }));
    const failures = [];
    for (const [lane, id] of [['replacement', sessionA], ['other', sessionB]]) {
      try { await stopLane(project, lane, id); }
      catch (error) {
        try {
          if (gremlinOwnerPids(project, lane).length > 0) {
            await forceStopOwnedGremlinGroup(project, lane);
            failures.push(new Error(`API stop failed; exact owner group was terminated: ${lane}; ${error}`));
          } else failures.push(error);
        } catch (cleanupError) { failures.push(cleanupError); }
      }
    }
    if (failures.length) {
      if (primaryError) failures.unshift(primaryError);
      throw new AggregateError(failures,
        primaryError ? 'Gremlin run and cleanup failed' : 'Gremlin cleanup failed');
    }
  }
  if (primaryError) throw primaryError;
}

main().then(() => {
  makeWritableTree(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
})
  .catch(error => {
    console.error(error);
    console.error(`scratch preserved for diagnosis: ${scratch}`);
    process.exitCode = 1;
  });
