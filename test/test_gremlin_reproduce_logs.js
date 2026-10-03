#!/usr/bin/env node
// Prove repeated reproductions retain the output addressed by each receipt.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const argument = process.argv[2] || process.env.FO;
assert.ok(argument, 'usage: node test_gremlin_reproduce_logs.js /path/to/fo');
const fo = path.resolve(argument);
assert.ok(fs.existsSync(fo), `fo candidate does not exist: ${fo}`);
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('Gremlin reproduce logs: skipped (requires Linux process support)');
  process.exit(0);
}

const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-reproduce-logs-');
const project = path.join(scratch, 'project');
const stateRoot = path.join(scratch, 'gremlin-state');
const lane = 'reproduce-log-oracle';
const firstCase = 'test_reproduce_first';
const secondCase = 'test_reproduce_second';
const firstToken = 'FO_REPRODUCE_FIRST_OUTPUT_61c8c1';
const secondToken = 'FO_REPRODUCE_SECOND_OUTPUT_9d09d2';
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
          return { path: full, rows };
        }
      }
    }
  }
  return null;
}

function recordsFromJournal(journal) {
  return journal.rows.map(row => JSON.parse(row));
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
      if (command >= 0 && args[command + 1] === 'run' &&
          laneAt >= 0 && args[laneAt + 1] === lane &&
          dirAt >= 0 && args[dirAt + 1] === project) matches.push(Number(entry));
    } catch (_) { /* process exited during scan */ }
  }
  return matches;
}

function writeTest(name, token, shouldFail = false) {
  fs.writeFileSync(path.join(project, `test/${name}.f90`), [
    `program ${name}`,
    'implicit none',
    `print '(a)', '${token}'`,
    ...(shouldFail ? ['error stop 7'] : []),
    `end program ${name}`,
    ''
  ].join('\n'));
}

function writeFixture() {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.writeFileSync(path.join(project, 'fpm.toml'),
    'name = "gremlin_reproduce_log_probe"\n');
  writeTest('test_reproduce_anchor', 'FO_REPRODUCE_ANCHOR_OUTPUT');
  writeTest(firstCase, firstToken, true);
  writeTest(secondCase, secondToken, true);
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
    const journal = journalFor(sessionId);
    if (lastStatus.active_generation && journal &&
        recordsFromJournal(journal).some(record => record.case_id === 'test_reproduce_anchor' &&
          record.status === 'PASS' && record.generation === lastStatus.active_generation)) {
      return { generation: lastStatus.active_generation, journal };
    }
    await wait(100);
  }
  throw new Error(`timed out waiting for a successful generation and anchor receipt: ` +
    JSON.stringify(lastStatus));
}

async function waitForStopped(sessionId, timeoutMs = 15000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const state = json(['gremlin', 'status', '--dir', project, '--lane', lane,
      '--session', sessionId, '--json']).state;
    if (state === 'stopped' && ownerPids().length === 0) return;
    await wait(50);
  }
  throw new Error(`Gremlin lane did not stop cleanly: ${ownerPids().join(',')}`);
}

async function stop(sessionId) {
  if (!sessionId || ownerPids().length === 0) return;
  const result = run(['gremlin', 'stop', '--dir', project, '--lane', lane,
    '--session', sessionId, '--json']);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  await waitForStopped(sessionId);
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

async function main() {
  writeFixture();
  let sessionId = '';
  let primaryError;
  try {
    const started = json(['gremlin', 'start', '--dir', project, '--lane', lane,
      '--target', 'test_reproduce_anchor', '--seed', '1729',
      '--campaign-seconds', '60', '--timeout-seconds', '5']);
    sessionId = started.session_id;
    assert.ok(sessionId, `start omitted session_id: ${JSON.stringify(started)}`);
    const { generation } = await waitForReady(sessionId);

    for (const caseId of [firstCase, secondCase]) {
      const result = run(['gremlin', 'reproduce', caseId, '--dir', project,
        '--lane', lane, '--session', sessionId, '--generation', generation, '--json']);
      assert.equal(result.status, 1, result.stdout + result.stderr);
      const reproduced = JSON.parse(result.stdout.trim());
      assert.equal(reproduced.state, 'FAIL',
        `${caseId} did not run successfully: ${JSON.stringify(reproduced)}`);
    }

    const journal = journalFor(sessionId);
    assert.ok(journal, `journal disappeared for live session ${sessionId}`);
    const records = recordsFromJournal(journal);
    const firstReceipt = records.find(record => record.case_id === firstCase &&
      record.generation === generation && record.status === 'FAIL' &&
      path.basename(record.log_path).includes('reproduce'));
    const secondReceipt = records.find(record => record.case_id === secondCase &&
      record.generation === generation && record.status === 'FAIL' &&
      path.basename(record.log_path).includes('reproduce'));
    assert.ok(firstReceipt?.log_path, `no log_path in first reproduction receipt`);
    assert.ok(secondReceipt?.log_path, `no log_path in second reproduction receipt`);

    const firstLog = fs.readFileSync(firstReceipt.log_path, 'utf8');
    const secondLog = fs.readFileSync(secondReceipt.log_path, 'utf8');
    assert.ok(firstLog.includes(firstToken),
      `first reproduction receipt's log was overwritten by the second run; ` +
      `expected ${firstToken}, found log bytes at ${firstReceipt.log_path}`);
    assert.ok(!firstLog.includes(secondToken),
      `first reproduction receipt's log was overwritten by the second run: ${firstReceipt.log_path}`);
    assert.ok(secondLog.includes(secondToken),
      `second reproduction receipt's log lacks ${secondToken}: ${secondReceipt.log_path}`);
    assert.ok(!secondLog.includes(firstToken),
      `second reproduction receipt's log contains the first run's output: ${secondReceipt.log_path}`);
    assert.notEqual(firstReceipt.log_path, secondReceipt.log_path,
      'each reproduction receipt must reference its own output log');
    console.log('Gremlin reproduce logs: separate receipt output bytes passed');
  } catch (error) {
    primaryError = error;
  } finally {
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
