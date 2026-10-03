#!/usr/bin/env node
// Live behavioral oracle for campaign seed replay and lane history/debt.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const driver = path.resolve(process.argv[2] || process.env.FO || 'fo');
const scratch = fs.mkdtempSync('/var/tmp/fo-campaign-history-');
const state = path.join(scratch, 'state');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'cache'), FO_CACHE_DIR: path.join(scratch, 'fo-cache'),
  FO_GREMLIN_STATE_DIR: state, TMPDIR: '/var/tmp', FO_JOBS: '1',
  FO_DISABLE_SELF_REFRESH: '1', PATH: `${path.dirname(driver)}:${process.env.PATH}` };
fs.mkdirSync(env.HOME, { recursive: true });

function run(args, cwd) {
  const result = spawnSync(driver, args, { cwd, env, encoding: 'utf8',
    timeout: 120000, maxBuffer: 8 * 1024 * 1024 });
  if (result.error) throw result.error;
  return result;
}

function json(args, cwd) {
  const result = run(args, cwd);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}

function writeCase(project, name, marker, mode = 'pass') {
  const literal = marker.replaceAll("'", "''");
  const lines = [`program ${name}`];
  if (mode === 'blocked') {
    lines.push('use iso_c_binding, only: c_int');
  }
  lines.push('implicit none');
  if (mode === 'blocked') {
    lines.push('interface', '  function c_pause() bind(C, name="pause") result(rc)',
      '    import :: c_int', '    integer(c_int) :: rc', '  end function c_pause', 'end interface');
  }
  lines.push('integer :: u');
  if (mode === 'blocked') lines.push('integer(c_int) :: rc');
  lines.push(
    `open(newunit=u,file='${literal}',status='unknown',position='append')`,
    `write(u,'(a)') '${name}'`, 'close(u)');
  if (mode === 'blocked') lines.push('rc = c_pause()');
  if (mode === 'fail') lines.push('error stop 9');
  lines.push(`end program ${name}`, '');
  fs.writeFileSync(path.join(project, 'test', `${name}.f90`), lines.join('\n'));
}

function makeProject(dir, names, marker, modes = {}) {
  fs.mkdirSync(path.join(dir, 'test'), { recursive: true });
  fs.writeFileSync(path.join(dir, 'fpm.toml'), 'name = "gremlin_campaign_probe"\n');
  for (const name of names) writeCase(dir, name, marker, modes[name]?.mode || 'pass');
}

function start(project, lane, { seed, random = 1, targets = [], shuffle = false }) {
  return json(['gremlin', 'start', '--dir', project, '--lane', lane,
    '--random-count', String(random), '--seed', String(seed),
    '--campaign-seconds', '60', '--timeout-seconds', '5',
    ...(shuffle ? ['--shuffle'] : []),
    ...targets.flatMap(name => ['--target', name]), '--json'], project);
}

function status(project, lane, session) {
  return json(['gremlin', 'status', '--dir', project, '--lane', lane,
    '--session', session, '--json'], project);
}

function eventPage(project, lane, session, cursor) {
  return json(['gremlin', 'events', '--dir', project, '--lane', lane,
    '--session', session, '--cursor', String(cursor), '--max-records', '128',
    '--max-bytes', '262144', '--json'], project);
}

async function waitForReceipts(project, lane, session, count, timeout = 60000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) {
    const page = eventPage(project, lane, session, 0);
    const cases = page.events.filter(event => event.case_id !== '<build>');
    if (cases.length >= count) return cases;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error(`timed out waiting for ${count} receipts in ${lane}`);
}

async function waitForMarker(file, name, timeout = 60000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) {
    if (fs.existsSync(file) && fs.readFileSync(file, 'utf8').split(/\r?\n/).includes(name)) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`timed out waiting for independent marker ${name}`);
}

async function waitForMarkerAfter(file, name, offset, timeout = 60000) {
  const end = Date.now() + timeout;
  while (Date.now() < end) {
    const values = fs.existsSync(file) ? fs.readFileSync(file, 'utf8').split(/\r?\n/).filter(Boolean) : [];
    if (values.slice(offset).includes(name)) return values;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`timed out waiting for new marker ${name}`);
}

async function stop(project, lane, session) {
  const stopped = run(['gremlin', 'stop', '--dir', project, '--lane', lane,
    '--session', session, '--json'], project);
  assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
  const end = Date.now() + 10000;
  while (Date.now() < end) {
    try {
      if (status(project, lane, session).state === 'stopped') break;
    } catch (_) { /* owner is between terminal publication and release */ }
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  if (Date.now() >= end) throw new Error(`owner did not stop: ${lane}/${session}`);
  const ownerEnd = Date.now() + 10000;
  while (Date.now() < ownerEnd) {
    let alive = false;
    for (const entry of fs.readdirSync('/proc')) {
      if (!/^\d+$/.test(entry)) continue;
      try {
        const argv = fs.readFileSync(`/proc/${entry}/cmdline`, 'utf8').split('\0');
        if (argv.includes('gremlin') && argv.includes('run') && argv.includes(project) &&
            argv.includes('--lane-id') && argv[argv.indexOf('--lane-id') + 1] === lane) {
          alive = true;
          break;
        }
      } catch (_) { /* process exited during inspection */ }
    }
    if (!alive) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`owner process remained after stop: ${lane}/${session}`);
}

function randomPermutation(names, seed) {
  const result = [...names];
  let state = ((seed % 2147483646) + 2147483646) % 2147483646 + 1;
  for (let i = result.length - 1; i > 0; i--) {
    const q = Math.floor(state / 127773);
    state = 16807 * (state - q * 127773) - 2836 * q;
    if (state <= 0) state += 2147483647;
    const j = state % (i + 1);
    [result[i], result[j]] = [result[j], result[i]];
  }
  return result;
}

function expectedSelection(inventory, debt, failures, seed, randomCount, cursorSeed) {
  const selected = [];
  const add = name => { if (!selected.includes(name)) selected.push(name); };
  for (const name of failures) add(name);
  const debtCount = Math.min(8, debt.length);
  const startAt = debt.length ? ((cursorSeed % debt.length) + debt.length) % debt.length : 0;
  for (let i = 0; i < debt.length && selected.length - failures.length < 32 - debtCount; i++) {
    add(debt[(startAt + i) % debt.length]);
  }
  const slots = Math.min(randomCount, Math.max(0, 32 - (selected.length - failures.length)));
  const candidates = inventory.filter(name => !selected.includes(name));
  selected.push(...randomPermutation(candidates, seed).slice(0, slots));
  return randomPermutation(selected, seed);
}

function updateHistory(event, debt, failures) {
  const old = debt.indexOf(event.case_id);
  if (old >= 0) debt.splice(old, 1);
  debt.push(event.case_id);
  if (event.status === 'FAIL' || event.status === 'TIMEOUT') {
    const prior = failures.indexOf(event.case_id);
    if (prior >= 0) failures.splice(prior, 1);
    failures.unshift(event.case_id);
    if (failures.length > 128) failures.pop();
  }
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

function verifySeedReceipts(events, inventory, randomCount) {
  const cases = events.filter(event => event.case_id !== '<build>' &&
    ['PASS', 'FAIL', 'TIMEOUT'].includes(event.status));
  const debt = [];
  const failures = [];
  let queue = [];
  let cursorSeed = 0;
  let launches = 0;
  for (const event of cases) {
    if (event.order === 1) queue = expectedSelection(inventory, debt, failures,
      event.seed, randomCount, cursorSeed);
    assert.ok(event.order > 0 && event.order <= queue.length,
      `receipt order ${event.order} lies outside independently calculated selection`);
    assert.equal(event.case_id, queue[event.order - 1],
      `launched ${event.case_id} at receipt seed ${event.seed}; independent selector expected `
        + `${queue[event.order - 1]}`);
    updateHistory(event, debt, failures);
    cursorSeed = event.seed;
    launches++;
  }
  assert.ok(new Set(cases.map(event => event.seed)).size >= 3,
    'oracle observed at least three distinct campaign seeds');
  assert.ok(launches >= 12, 'oracle compared at least twelve actual launches');
}

async function main() {
  const seedProject = path.join(scratch, 'seed-project');
  const seedMarker = path.join(scratch, 'seed-markers');
  const seedNames = Array.from({ length: 40 }, (_, i) => `test_case_${String(i + 1).padStart(2, '0')}`);
  makeProject(seedProject, seedNames, seedMarker);
  const seedOwner = start(seedProject, 'seed-oracle', { seed: 1729, random: 4, shuffle: true });
  try {
    const cases = await waitForReceipts(seedProject, 'seed-oracle', seedOwner.session_id, 20);
    verifySeedReceipts(cases, seedNames, 4);
    const markers = fs.readFileSync(seedMarker, 'utf8').split(/\r?\n/).filter(Boolean);
    assert.deepEqual(markers.slice(0, cases.length), cases.map(event => event.case_id),
      'test-written launch markers match durable receipt launch order');
  } finally { await stop(seedProject, 'seed-oracle', seedOwner.session_id); }

  const historyProject = path.join(scratch, 'history-project');
  const historyMarker = path.join(scratch, 'history-markers');
  const historyNames = ['test_failure', 'test_unknown', 'test_alpha', 'test_beta'];
  makeProject(historyProject, historyNames, historyMarker, {
    test_failure: { mode: 'fail' }, test_unknown: { mode: 'blocked' }
  });
  const first = start(historyProject, 'history', { seed: 11, random: 1,
    targets: ['test_failure'] });
  try {
    const firstCases = await waitForReceipts(historyProject, 'history', first.session_id, 1);
    assert.equal(firstCases[0].case_id, 'test_failure');
    assert.equal(firstCases[0].status, 'FAIL');
  } finally { await stop(historyProject, 'history', first.session_id); }

  const second = start(historyProject, 'history', { seed: 22, random: 1 });
  try {
    const priorMarkers = fs.readFileSync(historyMarker, 'utf8').split(/\r?\n/).filter(Boolean).length;
    await waitForMarkerAfter(historyMarker, 'test_failure', priorMarkers);
    const secondCases = await waitForReceipts(historyProject, 'history', second.session_id, 1);
    assert.equal(secondCases[0].case_id, 'test_failure',
      'failure receipt from session A is the first launched test in session B');
  } finally { await stop(historyProject, 'history', second.session_id); }

  const third = start(historyProject, 'history', { seed: 33, random: 1,
    targets: ['test_unknown'] });
  try { await waitForMarker(historyMarker, 'test_unknown'); }
  finally { await stop(historyProject, 'history', third.session_id); }
  const unknownReceipts = eventPage(historyProject, 'history', third.session_id, 0)
    .events.filter(event => event.case_id === 'test_unknown');
  assert.equal(unknownReceipts.length, 0,
    'stopped gated work has no completion receipt and stays unclassified');

  const fourth = start(historyProject, 'history', { seed: 44, random: 1 });
  try {
    const priorMarkers = fs.readFileSync(historyMarker, 'utf8').split(/\r?\n/).filter(Boolean).length;
    await waitForMarkerAfter(historyMarker, 'test_failure', priorMarkers);
    const fourthCases = await waitForReceipts(historyProject, 'history', fourth.session_id, 1);
    assert.equal(fourthCases[0].case_id, 'test_failure',
      'unknown work did not displace the durable failure priority');
  } finally { await stop(historyProject, 'history', fourth.session_id); }

  const debtProject = path.join(scratch, 'debt-project');
  const debtMarker = path.join(scratch, 'debt-markers');
  const debtNames = Array.from({ length: 10 }, (_, i) => `test_debt_${String(i + 1).padStart(2, '0')}`);
  makeProject(debtProject, debtNames, debtMarker);
  const debtFirst = start(debtProject, 'debt', { seed: 501, random: 1, targets: debtNames });
  try {
    await waitForReceipts(debtProject, 'debt', debtFirst.session_id, debtNames.length);
  } finally { await stop(debtProject, 'debt', debtFirst.session_id); }
  const debtSecond = start(debtProject, 'debt', { seed: 777, random: 1 });
  try {
    const priorMarkers = fs.readFileSync(debtMarker, 'utf8').split(/\r?\n/).filter(Boolean).length;
    const secondMarkers = await waitForMarkerAfter(debtMarker, 'test_debt_02', priorMarkers);
    assert.equal(secondMarkers[priorMarkers], 'test_debt_02',
      'persisted cursor starts the next session at the independently predicted debt case');
    await waitForReceipts(debtProject, 'debt', debtSecond.session_id, 1);
  } finally { await stop(debtProject, 'debt', debtSecond.session_id); }
  const debtThird = start(debtProject, 'debt', { seed: 888, random: 1 });
  try {
    const priorMarkers = fs.readFileSync(debtMarker, 'utf8').split(/\r?\n/).filter(Boolean).length;
    const thirdMarkers = await waitForMarkerAfter(debtMarker, 'test_debt_09', priorMarkers);
    assert.equal(thirdMarkers[priorMarkers], 'test_debt_09',
      'completed debt and receipt seed advance the cursor for the following session');
  } finally { await stop(debtProject, 'debt', debtThird.session_id); }

  console.log('Gremlin campaign history: exact receipt-seed launches, cross-session failure priority, '
    + 'unknown exclusion, LRU debt cursor, and independent markers PASS');
  makeWritableTree(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
}

main().catch(error => {
  console.error(error.stack || error);
  process.exitCode = 1;
});
