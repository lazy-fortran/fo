#!/usr/bin/env node
// Process-kill oracle for a deterministic, generation-scoped 40-case coverage epoch.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin coverage restart: skipped (requires Linux /proc)');
  process.exit(0);
}

const fo = path.resolve(process.argv[2] || process.env.FO || 'fo');
const scratch = fs.mkdtempSync('/var/tmp/fo-coverage-restart-');
const project = path.join(scratch, 'project');
const marker = path.join(scratch, 'cases.log');
const lane = 'coverage-restart';
let concurrentReplay = null;
const priorityLane = 'coverage-priority-jump';
const names = Array.from({ length: 40 }, (_, i) =>
  `test_case_${String(i + 1).padStart(2, '0')}`);
const seed = 1729;
const order = expectedOrder(names, seed);
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg'), FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'state'),
  FO_CACHE_DIR: path.join(scratch, 'cache'), TMPDIR: '/var/tmp', FO_JOBS: '1',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1' };
fs.mkdirSync(env.HOME, { recursive: true });
fs.mkdirSync(path.join(project, 'test'), { recursive: true });
fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "coverage_restart_probe"\n');

function run(args) {
  const result = spawnSync(fo, args, { cwd: project, env, encoding: 'utf8',
    timeout: 120000, maxBuffer: 8 * 1024 * 1024 });
  if (result.error) throw result.error;
  return result;
}

function json(args) {
  const result = run(args);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}

function literal(value) { return value.replaceAll("'", "''"); }

const gate = path.join(scratch, 'case18.fifo');
const ready = path.join(scratch, 'case18.ready');
const failFlag = path.join(scratch, 'fail-');
const replayFlag = path.join(scratch, 'replay-');
const replayGate = path.join(scratch, 'replay.fifo');
const replayReady = path.join(scratch, 'replay.ready');
assert.equal(spawnSync('mkfifo', [replayGate]).status, 0, 'creates replay gate');
assert.equal(spawnSync('mkfifo', [gate]).status, 0, 'creates case 18 gate');
for (const name of names) {
  const lines = [`program ${name}`];
  if (name === order[17]) {
    lines.push('use, intrinsic :: iso_c_binding, only: c_int',
      'implicit none', 'interface',
      '  function c_getpid() bind(C, name="getpid") result(pid)',
      '    import :: c_int', '    integer(c_int) :: pid', '  end function c_getpid',
      'end interface', 'integer :: unit, gate_unit, replay_unit',
      'character :: token', 'logical :: flagged',
      `open(newunit=unit, file='${literal(ready)}', status='replace')`,
      "write(unit, '(i0)') c_getpid()", 'close(unit)',
      `open(newunit=gate_unit, file='${literal(gate)}', status='old', &`,
      "    access='stream', form='unformatted', action='read')",
      'read(gate_unit) token', 'close(gate_unit)');
  } else {
    lines.push('implicit none', 'integer :: unit, replay_unit',
      'character :: token', 'logical :: flagged');
  }
  lines.push(`inquire(file='${literal(replayFlag + name)}', exist=flagged)`,
    'if (flagged) then',
    `open(newunit=unit, file='${literal(replayReady)}', status='replace')`,
    'close(unit)',
    `open(newunit=replay_unit, file='${literal(replayGate)}', status='old', &`,
    "    access='stream', form='unformatted', action='read')",
    'read(replay_unit) token', 'close(replay_unit)', 'end if');
  lines.push(`open(newunit=unit, file='${literal(marker)}', status='unknown', &`,
    "    position='append')", `write(unit, '(a)') '${name}'`, 'close(unit)',
    `inquire(file='${literal(failFlag + name)}', exist=flagged)`,
    'if (flagged) error stop 1', `end program ${name}`, '');
  fs.writeFileSync(path.join(project, 'test', `${name}.f90`), lines.join('\n'));
}

function expectedOrder(values, initialSeed) {
  const result = [...values];
  let state = ((initialSeed % 2147483646) + 2147483646) % 2147483646 + 1;
  for (let i = result.length; i > 1; i--) {
    const q = Math.floor(state / 127773);
    state = 16807 * (state - q * 127773) - 2836 * q;
    if (state <= 0) state += 2147483647;
    const j = state % i;
    [result[i - 1], result[j]] = [result[j], result[i - 1]];
  }
  return result;
}

async function start(laneName = lane, target = null) {
  const args = ['gremlin', 'start', '--dir', project, '--lane', laneName,
    '--random-count', '4', '--seed', String(seed), '--campaign-seconds', '60',
    '--timeout-seconds', '5', '--json'];
  if (target) args.push('--target', target);
  for (let attempt = 0; attempt < 5; attempt++) {
    const result = run(args);
    if (result.status === 0) return JSON.parse(result.stdout.trim());
    if (!result.stdout.includes('owner did not publish ready state')) {
      assert.equal(result.status, 0, result.stdout + result.stderr);
    }
    await wait(500);
  }
  throw new Error('Gremlin owner did not publish ready state after retries');
}

function events(session, laneName = lane) {
  return json(['gremlin', 'events', '--dir', project, '--lane', laneName,
    '--session', session, '--cursor', '0', '--max-records', '128',
    '--max-bytes', '262144', '--json']).events.filter(event => names.includes(event.case_id));
}

function wait(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }

async function waitUntil(predicate, description, timeout = 60000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    const result = predicate();
    if (result) return result;
    await wait(50);
  }
  throw new Error(`timed out waiting for ${description}`);
}

function ownerPid(laneName = lane) {
  for (const entry of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(entry)) continue;
    try {
      const argv = fs.readFileSync(`/proc/${entry}/cmdline`, 'utf8').split('\0');
      const at = argv.indexOf('gremlin');
      if (at >= 0 && argv[at + 1] === 'run' && argv.includes(project) &&
          argv.includes('--lane-id') && argv[argv.indexOf('--lane-id') + 1] === laneName) {
        return Number(entry);
      }
    } catch (_) { /* process ended while scanning */ }
  }
  return 0;
}

function ownedDescendants(owner) {
  const parents = new Map();
  for (const entry of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(entry)) continue;
    try {
      const stat = fs.readFileSync(`/proc/${entry}/stat`, 'utf8');
      const fields = stat.slice(stat.lastIndexOf(')') + 2).split(' ');
      parents.set(Number(entry), Number(fields[1]));
    } catch (_) { /* process ended */ }
  }
  const owned = new Set([owner]);
  let changed = true;
  while (changed) {
    changed = false;
    for (const [pid, parent] of parents) {
      if (!owned.has(pid) && owned.has(parent)) { owned.add(pid); changed = true; }
    }
  }
  owned.delete(owner);
  return [...owned];
}

function killOwnedTree(owner) {
  const children = ownedDescendants(owner);
  for (const pid of [owner, ...children.reverse()]) {
    try { process.kill(pid, 'SIGKILL'); } catch (error) {
      if (error.code !== 'ESRCH') throw error;
    }
  }
}

function readMarkers() {
  if (!fs.existsSync(marker)) return [];
  return fs.readFileSync(marker, 'utf8').split(/\r?\n/).filter(Boolean);
}

function findCoverageState(root) {
  if (!fs.existsSync(root)) return null;
  for (const entry of fs.readdirSync(root, { withFileTypes: true })) {
    const full = path.join(root, entry.name);
    if (entry.isDirectory()) {
      const nested = findCoverageState(full);
      if (nested) return nested;
    } else if (/^coverage-[0-9a-f]{64}\.state$/.test(entry.name)) {
      return full;
    }
  }
  return null;
}

async function main() {
  const first = await start();
  assert.ok(first.session_id, 'first process publishes its session id');
  await waitUntil(() => events(first.session_id).length >= 17, 'seventeen receipts');
  await waitUntil(() => fs.existsSync(ready), 'the eighteenth case reaches its gate');
  const before = readMarkers();
  assert.equal(before.length, 17, 'campaign marker freezes exactly seventeen cases');
  assert.deepEqual(before, order.slice(0, 17), 'first seventeen match independent permutation');
  const coveragePath = await waitUntil(() => findCoverageState(env.FO_GREMLIN_STATE_DIR),
    'durable coverage state');
  const coverageBeforeReproduce = fs.readFileSync(coveragePath);
  const partialStatus = json(['gremlin', 'status', '--dir', project, '--lane', lane,
    '--session', first.session_id, '--json']);
  assert.equal(partialStatus.coverage.running, 1);
  assert.equal(partialStatus.coverage.unknown, 23);
  assert.equal(partialStatus.coverage.remaining, 23);
  const reproducedCase = before[0];
  const unseenCase = order[18];
  const unseen = run(['gremlin', 'reproduce', unseenCase, '--dir', project,
    '--lane', lane, '--json']);
  assert.equal(unseen.status, 0, unseen.stdout + unseen.stderr);
  assert.deepEqual(fs.readFileSync(coveragePath), coverageBeforeReproduce,
    'unseen reproduction cannot satisfy an epoch obligation');
  const pid = ownerPid();
  assert.ok(pid > 0, 'finds the exact lane owner process');
  const blockedPid = Number(fs.readFileSync(ready, 'utf8').trim());
  // Freeze the exact owned descendant set before killing the supervisor.
  // Killing just the leaf can let its still-live fo test parent retry it.
  killOwnedTree(pid);
  await waitUntil(() => ownerPid() === 0, 'old supervisor exits');

  const coverageHeader = fs.readFileSync(coveragePath, 'utf8').split(/\r?\n/, 1)[0];
  const [oldGeneration, inventoryDigest] = coverageHeader.split('|');
  assert.equal(inventoryDigest.length, 64, 'coverage file carries inventory digest');
  const campaignJournal = path.join(path.dirname(coveragePath), 'campaign-journal.jsonl');
  fs.appendFileSync(campaignJournal, `${JSON.stringify({
    completion_id: 'stale-generation-pass', session_id: first.session_id,
    lane_id: lane, generation: 'f'.repeat(64), case_id: order[17],
    outcome: 'pass', status: 'PASS', exitcode: 0, seed,
    coverage_epoch: 1, inventory_digest: inventoryDigest, order: 18, log_path: 'stale'
  })}\n`);
  assert.notEqual(oldGeneration, 'f'.repeat(64), 'stale receipt has a different generation');

  const second = await start();
  assert.ok(second.session_id, 'replacement process publishes its session id');
  await waitUntil(() => {
    if (!fs.existsSync(ready)) return false;
    return Number(fs.readFileSync(ready, 'utf8').trim()) !== blockedPid;
  }, 'replacement reaches the interrupted case');
  fs.writeFileSync(replayFlag + reproducedCase, 'replay');
  const replay = spawn(fo, ['gremlin', 'reproduce', reproducedCase, '--dir', project,
    '--lane', lane, '--generation', oldGeneration, '--json'],
    { cwd: project, env, stdio: ['ignore', 'pipe', 'pipe'] });
  concurrentReplay = replay;
  let replayOutput = '', replayError = '';
  replay.stdout.on('data', data => { replayOutput += data; });
  replay.stderr.on('data', data => { replayError += data; });
  let replayClosed = false;
  const replayCompletion = new Promise((resolve, reject) => {
    replay.on('error', reject);
    replay.on('close', code => { replayClosed = true; resolve(code); });
  });
  await waitUntil(() => {
    if (replayClosed) throw new Error(`replay exited before gate: ${replayOutput} ${replayError}`);
    return fs.existsSync(replayReady);
  }, 'concurrent reproduction reaches gate');
  const released = spawnSync(process.execPath, ['-e',
    "require('node:fs').writeFileSync(process.argv.at(-1), 'x')", gate],
  { encoding: 'utf8', timeout: 10000 });
  assert.equal(released.status, 0, released.stdout + released.stderr);
  await waitUntil(() => events(second.session_id).some(event =>
    event.case_id === order[17] && event.evidence_kind === 'campaign'),
    'campaign completion while reproduction is gated');
  fs.writeFileSync(failFlag + reproducedCase, 'fail');
  // fo retries failed tests once; only the first invocation needs the gate.
  fs.unlinkSync(replayFlag + reproducedCase);
  const releaseReplay = spawnSync(process.execPath, ['-e',
    "require('node:fs').writeFileSync(process.argv.at(-1), 'x')", replayGate],
    { encoding: 'utf8', timeout: 10000 });
  assert.equal(releaseReplay.status, 0, releaseReplay.stdout + releaseReplay.stderr);
  assert.equal(await replayCompletion, 1, replayOutput + replayError);
  concurrentReplay = null;
  assert.equal(JSON.parse(replayOutput.trim()).state, 'FAIL');
  fs.unlinkSync(failFlag + reproducedCase);
  const afterReplay = fs.readFileSync(coveragePath, 'utf8').split(/\r?\n/);
  assert.ok(afterReplay.includes(`${reproducedCase}|PASS`),
    'concurrent differing reproduction cannot replace the campaign PASS');
  let latestStatus;
  await waitUntil(() => {
    latestStatus = json(['gremlin', 'status', '--dir', project, '--lane', lane,
      '--session', second.session_id, '--json']);
    return latestStatus.coverage && latestStatus.coverage.full_coverage;
  }, 'forty current-epoch results and completion', 180000).catch(error => {
    throw new Error(`${error.message}; last status ${JSON.stringify(latestStatus)}`);
  });
  const coverageBeforeDiffering = fs.readFileSync(coveragePath);
  fs.writeFileSync(failFlag + reproducedCase, 'fail');
  const reproduction = run(['gremlin', 'reproduce', reproducedCase, '--dir', project,
    '--lane', lane, '--json']);
  assert.equal(reproduction.status, 1, reproduction.stdout + reproduction.stderr);
  assert.equal(JSON.parse(reproduction.stdout.trim()).state, 'FAIL',
    'completed PASS independently reproduces a differing FAIL');
  fs.unlinkSync(failFlag + reproducedCase);
  assert.deepEqual(fs.readFileSync(coveragePath), coverageBeforeDiffering,
    'differing reproduction cannot replace completed PASS or reopen epoch');
  const stopped = run(['gremlin', 'stop', '--dir', project, '--lane', lane,
    '--session', second.session_id, '--json']);
  assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
  await waitUntil(() => ownerPid() === 0, 'replacement owner stops');
  const after = readMarkers();
  assert.equal(after.length, 45, 'forty epoch markers and five reproduction executions');
  assert.equal(after[before.length], unseenCase, 'unseen reproduction has its own marker');
  const campaignMarkers = [...after];
  campaignMarkers.splice(before.length, 1);
  // Remove four failed executions belonging to the two differing reproductions.
  for (let i = 0; i < 4; i++) {
    const at = campaignMarkers.lastIndexOf(reproducedCase);
    assert.ok(at > 0, 'gated reproduction is after the original completed case');
    campaignMarkers.splice(at, 1);
  }
  assert.deepEqual(campaignMarkers.slice(0, 40), order,
    'restart resumes the exact permutation cursor');
  assert.equal(new Set(campaignMarkers.slice(0, 40)).size, 40,
    'epoch is without replacement');
  const allReceipts = events(second.session_id);
  const receipts = allReceipts.filter(event =>
    event.generation === latestStatus.active_generation &&
    event.coverage_epoch === latestStatus.coverage.epoch &&
    event.inventory_digest === latestStatus.coverage.inventory_digest &&
    event.seed === latestStatus.coverage.seed);
  assert.equal(receipts.length, 40, 'receipt ledger covers the exact eligible inventory');
  const campaignReceipts = fs.readFileSync(campaignJournal, 'utf8').trim().split(/\r?\n/)
    .map(line => JSON.parse(line));
  assert.ok(campaignReceipts.some(event => event.generation === 'f'.repeat(64) &&
    event.case_id === order[17] && event.status === 'PASS'),
  'stale-generation PASS is present in the recovery source');
  assert.ok(allReceipts.some(event => event.case_id === reproducedCase &&
    event.evidence_kind === 'reproduction' && event.status === 'FAIL' &&
    event.coverage_epoch === undefined && event.inventory_digest === undefined),
  'reproduction evidence has no coverage identity');
  assert.ok(receipts.every(event => ['PASS', 'TIMEOUT'].includes(event.status)),
    'every current-generation outcome remains a behavioral result');
  assert.ok(receipts.filter(event => event.status === 'TIMEOUT').every(event =>
    campaignMarkers.includes(event.case_id)),
  'timeout receipts retain their independent case marker');
  assert.ok(campaignMarkers.includes(order[17]),
    'stale-generation PASS does not satisfy current coverage');

  fs.writeFileSync(marker, '');
  const formerlyGated = order[17];
  fs.writeFileSync(path.join(project, 'test', `${formerlyGated}.f90`), [
    `program ${formerlyGated}`, 'implicit none', 'integer :: unit',
    `open(newunit=unit, file='${literal(marker)}', status='unknown', position='append')`,
    `write(unit, '(a)') '${formerlyGated}'`, 'close(unit)',
    `end program ${formerlyGated}`, ''
  ].join('\n'));
  const priorityTarget = names[0];
  assert.ok(order.indexOf(priorityTarget) >= 5,
    'priority fixture is positioned after the first coverage chunk');
  const prioritySession = await start(priorityLane, priorityTarget);
  await waitUntil(() => events(prioritySession.session_id, priorityLane).length >= 40,
    'priority-jump epoch coverage', 180000);
  const priorityStatus = json(['gremlin', 'status', '--dir', project, '--lane', priorityLane,
    '--session', prioritySession.session_id, '--json']);
  const priorityReceipts = events(prioritySession.session_id, priorityLane);
  const priorityTimeouts = new Set(priorityReceipts.filter(event => event.status === 'TIMEOUT')
    .map(event => event.case_id));
  assert.equal(priorityStatus.coverage.eligible, 40, 'status exposes typed inventory count');
  assert.equal(priorityStatus.coverage.pass + priorityStatus.coverage.timeout, 40,
    'status distinguishes pass and timeout outcomes for every case');
  assert.equal(priorityStatus.coverage.unknown, 0);
  assert.equal(priorityStatus.coverage.remaining, 0);
  assert.equal(priorityStatus.coverage.full_coverage, true, 'status exposes full coverage');
  assert.equal(priorityStatus.coverage.green, priorityTimeouts.size === 0,
    'any non-pass result prevents a green status');
  const markersAtExhaustion = readMarkers().length;
  await wait(1500);
  assert.equal(readMarkers().length, markersAtExhaustion,
    'completed epoch quiesces without starting a repeat');
  const priorityStop = run(['gremlin', 'stop', '--dir', project, '--lane', priorityLane,
    '--session', prioritySession.session_id, '--json']);
  assert.equal(priorityStop.status, 0, priorityStop.stdout + priorityStop.stderr);
  await waitUntil(() => ownerPid(priorityLane) === 0, 'priority owner stops');
  const priorityMarkers = readMarkers();
  assert.equal(priorityMarkers.length, 40 - priorityTimeouts.size,
    'priority run marks every PASS case');
  assert.equal(priorityMarkers[0], priorityTarget, 'supervisor runs priority before its slot');
  assert.deepEqual(priorityMarkers, [priorityTarget, ...order.filter(name =>
    name !== priorityTarget && !priorityTimeouts.has(name))],
    'early priority receipt satisfies its later permutation slot');
  assert.equal(new Set(priorityMarkers).size, priorityMarkers.length,
    'priority slot is not repeated');

  console.log('Gremlin coverage restart: PASS (17/23 recovery, isolated concurrent reproduction, priority jump)');
}

main().catch(error => {
  for (const laneName of [lane, priorityLane]) {
    const pid = ownerPid(laneName);
    if (pid > 0) killOwnedTree(pid);
  }
  if (concurrentReplay) killOwnedTree(concurrentReplay.pid);
  if (fs.existsSync(ready)) {
    const child = Number(fs.readFileSync(ready, 'utf8').trim());
    if (child > 0) { try { process.kill(child, 'SIGKILL'); } catch (_) { /* cleanup */ } }
  }
  throw error;
});
