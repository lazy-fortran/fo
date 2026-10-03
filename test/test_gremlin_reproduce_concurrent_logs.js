#!/usr/bin/env node
// Probe whether two reproduce requests overlap within one live Gremlin session.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const foArgument = process.argv[2] || process.env.FO;
assert.ok(foArgument, 'usage: node test_gremlin_reproduce_concurrent_logs.js /path/to/fo');
const fo = path.resolve(foArgument);
assert.ok(fs.existsSync(fo), `fo candidate does not exist: ${fo}`);
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin concurrent reproduce logs: skipped (requires Linux process support)');
  process.exit(0);
}

const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-reproduce-concurrent-');
const project = path.join(scratch, 'project');
const stateRoot = path.join(scratch, 'gremlin-state');
const gateRoot = path.join(scratch, 'gates');
const lane = 'reproduce-concurrent-log-oracle';
const cases = [
  { id: 'test_reproduce_gate_first', token: 'FO_REPRODUCE_GATE_FIRST_48ef31' },
  { id: 'test_reproduce_gate_second', token: 'FO_REPRODUCE_GATE_SECOND_0ba742' }
];
const env = { ...process.env,
  HOME: path.join(scratch, 'home'),
  TMPDIR: path.join(scratch, 'tmp'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_CACHE_DIR: path.join(scratch, 'cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: stateRoot,
  FO_JOBS: '1',
  FO_SELF_REFRESH: '0',
  FO_DISABLE_SELF_REFRESH: '1'
};
fs.mkdirSync(env.HOME, { recursive: true });
fs.mkdirSync(env.TMPDIR, { recursive: true });
fs.mkdirSync(gateRoot, { recursive: true });

function run(args, cwd = project) {
  const result = spawnSync(fo, args, { cwd, env, encoding: 'utf8',
    timeout: 120000, maxBuffer: 8 * 1024 * 1024 });
  if (result.error) throw result.error;
  return result;
}

function json(args, cwd = project) {
  const result = run(args, cwd);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}

function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function startChild(args) {
  const child = spawn(fo, args, { cwd: project, env, stdio: ['ignore', 'pipe', 'pipe'] });
  let stdout = '';
  let stderr = '';
  child.stdout.setEncoding('utf8').on('data', data => { stdout += data; });
  child.stderr.setEncoding('utf8').on('data', data => { stderr += data; });
  const done = new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('close', (code, signal) => resolve({ code, signal, stdout, stderr }));
  });
  return { child, done };
}

function journalFor(sessionId) {
  if (!fs.existsSync(stateRoot)) return null;
  const pending = [stateRoot];
  while (pending.length) {
    const current = pending.pop();
    for (const entry of fs.readdirSync(current, { withFileTypes: true })) {
      const full = path.join(current, entry.name);
      if (entry.isDirectory()) pending.push(full);
      else if (entry.name === 'journal.jsonl') {
        const rows = fs.readFileSync(full, 'utf8').trim().split('\n').filter(Boolean);
        if (rows.some(row => row.includes(`"session_id":"${sessionId}"`))) {
          return rows.map(row => JSON.parse(row));
        }
      }
    }
  }
  return null;
}

function ownerPids() {
  const matches = [];
  for (const entry of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(entry)) continue;
    try {
      const args = fs.readFileSync(`/proc/${entry}/cmdline`).toString().split('\0');
      const command = args.indexOf('gremlin');
      const laneAt = args.indexOf('--lane-id') >= 0
        ? args.indexOf('--lane-id') : args.indexOf('--lane');
      const dirAt = args.indexOf('--dir');
      if (command >= 0 && args[command + 1] === 'run' && laneAt >= 0 &&
          args[laneAt + 1] === lane && dirAt >= 0 && args[dirAt + 1] === project) {
        matches.push(Number(entry));
      }
    } catch (_) { /* process exited during scan */ }
  }
  return matches;
}

async function waitForPath(filename, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(filename)) return true;
    await wait(20);
  }
  return fs.existsSync(filename);
}

async function waitForReady(sessionId, timeoutMs = 120000) {
  const deadline = Date.now() + timeoutMs;
  let lastStatus = {};
  while (Date.now() < deadline) {
    lastStatus = json(['gremlin', 'status', '--dir', project, '--lane', lane,
      '--session', sessionId, '--json']);
    if (lastStatus.state === 'stopped' || lastStatus.state === 'error') {
      throw new Error(`lane stopped before generation became usable: ${JSON.stringify(lastStatus)}`);
    }
    const rows = journalFor(sessionId) || [];
    if (lastStatus.active_generation && rows.some(row => row.case_id === 'test_reproduce_anchor' &&
        row.status === 'PASS' && row.generation === lastStatus.active_generation)) {
      return lastStatus.active_generation;
    }
    await wait(100);
  }
  throw new Error(`timed out waiting for a successful generation: ${JSON.stringify(lastStatus)}`);
}

function gateTestSource(testCase) {
  const entered = path.join(gateRoot, `${testCase.id}.entered`);
  const release = path.join(gateRoot, `${testCase.id}.release`);
  // fs.mkdtemp uses a path without single quotes, so shell quoting is stable.
  return [
    `program ${testCase.id}`,
    'implicit none',
    'integer :: unit, wait_status',
    `open(newunit=unit, file='${entered}', status='replace')`,
    'write(unit,\'(a)\') \'entered\'',
    'close(unit)',
    `call execute_command_line("while [ ! -e '${release}' ]; do sleep 0.02; done", &`,
    '    exitstat=wait_status)',
    'if (wait_status /= 0) error stop 8',
    `print '(a)', '${testCase.token}'`,
    'error stop 7',
    `end program ${testCase.id}`,
    ''
  ].join('\n');
}

function writeFixture() {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "gremlin_concurrent_reproduce_probe"\n');
  fs.writeFileSync(path.join(project, 'test/test_reproduce_anchor.f90'), [
    'program test_reproduce_anchor', 'implicit none',
    "print '(a)', 'FO_REPRODUCE_ANCHOR_OUTPUT'", 'end program test_reproduce_anchor', ''
  ].join('\n'));
  for (const testCase of cases) {
    fs.writeFileSync(path.join(project, `test/${testCase.id}.f90`), gateTestSource(testCase));
  }
}

function reproductionArgs(testCase, sessionId, generation) {
  return ['gremlin', 'reproduce', testCase.id, '--dir', project, '--lane', lane,
    '--session', sessionId, '--generation', generation, '--json'];
}

async function finishChild(operation, expectedExit = 1) {
  const result = await operation.done;
  assert.equal(result.code, expectedExit, result.stdout + result.stderr);
  const response = JSON.parse(result.stdout.trim());
  assert.equal(response.state, 'FAIL', JSON.stringify(response));
  return response;
}

async function stop(sessionId) {
  if (ownerPids().length === 0) return;
  const result = run(['gremlin', 'stop', '--dir', project, '--lane', lane,
    '--session', sessionId, '--json']);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  const deadline = Date.now() + 15000;
  while (Date.now() < deadline && ownerPids().length > 0) await wait(50);
  assert.equal(ownerPids().length, 0, `Gremlin owner remains: ${ownerPids().join(',')}`);
}

function makeWritableTree(target) {
  if (!fs.existsSync(target)) return;
  const stats = fs.lstatSync(target);
  if (stats.isSymbolicLink()) return;
  fs.chmodSync(target, stats.mode | 0o700);
  if (stats.isDirectory()) {
    for (const entry of fs.readdirSync(target)) makeWritableTree(path.join(target, entry));
  }
}

function verifyLogs(rows, generation) {
  const receipts = cases.map(testCase => rows.find(row => row.case_id === testCase.id &&
    row.generation === generation && row.status === 'FAIL' && row.log_path));
  assert.ok(receipts.every(Boolean), `missing reproduction receipt: ${JSON.stringify(rows)}`);
  assert.notEqual(receipts[0].log_path, receipts[1].log_path,
    'concurrent reproduction receipts must reference distinct logs');
  cases.forEach((testCase, index) => {
    const contents = fs.readFileSync(receipts[index].log_path, 'utf8');
    assert.ok(contents.includes(testCase.token),
      `${testCase.id} receipt log lacks its output token: ${receipts[index].log_path}`);
    const other = cases[1 - index];
    assert.ok(!contents.includes(other.token),
      `${testCase.id} receipt log contains the other run output: ${receipts[index].log_path}`);
  });
}

async function main() {
  writeFixture();
  let sessionId = '';
  let operations = [];
  let primaryError;
  try {
    const started = json(['gremlin', 'start', '--dir', project, '--lane', lane,
      '--target', 'test_reproduce_anchor', '--seed', '1729', '--campaign-seconds', '60',
      '--timeout-seconds', '5']);
    sessionId = started.session_id;
    assert.ok(sessionId, `start omitted session_id: ${JSON.stringify(started)}`);
    const generation = await waitForReady(sessionId);
    const enteredPaths = cases.map(testCase => path.join(gateRoot, `${testCase.id}.entered`));
    const releasePaths = cases.map(testCase => path.join(gateRoot, `${testCase.id}.release`));

    operations.push(startChild(reproductionArgs(cases[0], sessionId, generation)));
    assert.ok(await waitForPath(enteredPaths[0], 10000),
      'first reproduce request did not reach its file-backed gate');
    operations.push(startChild(reproductionArgs(cases[1], sessionId, generation)));

    const secondEnteredDuringFirst = await waitForPath(enteredPaths[1], 1200);
    let secondRejected = false;
    if (secondEnteredDuringFirst) {
      console.log('Gremlin concurrent reproduce logs: both test gates entered before release');
    } else {
      const secondSettled = await Promise.race([
        operations[1].done.then(result => ({ settled: true, result })),
        wait(100).then(() => ({ settled: false }))
      ]);
      if (secondSettled.settled) {
        secondRejected = true;
        console.log(`Gremlin reproduce contract: second request completed before entering its ` +
          `gate while first held its gate (exit ${secondSettled.result.code}; ` +
          `${secondSettled.result.stdout.trim() || secondSettled.result.stderr.trim()})`);
      } else {
        console.log('Gremlin reproduce contract: second request waited until first released its gate');
      }
    }

    fs.writeFileSync(releasePaths[0], 'release\n');
    await finishChild(operations[0]);
    if (secondRejected) {
      operations[1] = startChild(reproductionArgs(cases[1], sessionId, generation));
    }
    if (!secondEnteredDuringFirst || secondRejected) {
      assert.ok(await waitForPath(enteredPaths[1], 10000),
        'second request did not enter after first completion or a fresh sequential retry');
    }
    fs.writeFileSync(releasePaths[1], 'release\n');
    await finishChild(operations[1]);

    const rows = journalFor(sessionId) || [];
    verifyLogs(rows, generation);
    console.log('Gremlin concurrent reproduce logs: separate receipt output bytes passed');
  } catch (error) {
    primaryError = error;
  } finally {
    for (const operation of operations) {
      if (operation.child.exitCode === null && operation.child.signalCode === null) {
        operation.child.kill('SIGTERM');
      }
    }
    try { await stop(sessionId); }
    catch (error) {
      if (primaryError) primaryError = new AggregateError([primaryError, error],
        'reproducer and cleanup failed');
      else primaryError = error;
    }
  }
  if (primaryError) throw primaryError;
}

main().then(() => {
  makeWritableTree(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
}).catch(error => {
  console.error(error);
  console.error(`scratch preserved for diagnosis: ${scratch}`);
  process.exitCode = 1;
});
