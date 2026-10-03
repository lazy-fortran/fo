#!/usr/bin/env node
// Behavioral contract for shared CLI/MCP work modes and argv worker adapters.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const fo = path.resolve(process.argv[2] || process.env.FO || 'fo');
const scratch = fs.mkdtempSync('/var/tmp/fo-work-modes-');
const env = {
  ...process.env,
  HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_CACHE_DIR: path.join(scratch, 'cache'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'state'),
  FO_WORKER_CAPACITY: '4',
  FO_WORK_CPU_CAPACITY: '4',
  FO_WORK_MEMORY_CAPACITY: '4',
  FO_WORK_BUILD_CAPACITY: '1',
  FO_WORK_TEST_CAPACITY: '2',
  FO_JOBS: '1',
  FO_SELF_REFRESH: '0',
  FO_DISABLE_SELF_REFRESH: '1',
  TMPDIR: '/var/tmp'
};
fs.mkdirSync(env.HOME, { recursive: true });
const activeRoots = new Set();
const rootEnvs = new Map();

function run(args, cwd, environment = env) {
  return spawnSync(fo, args, { cwd, env: environment, encoding: 'utf8', timeout: 30000,
    maxBuffer: 8 * 1024 * 1024 });
}

function json(args, cwd, environment = env) {
  const result = run(args, cwd, environment);
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
  return JSON.parse(result.stdout.trim());
}

function runAsync(args, cwd, environment = env) {
  return new Promise((resolve, reject) => {
    const child = spawn(fo, args, { cwd, env: environment });
    let stdout = '', stderr = '';
    child.stdout.on('data', chunk => { stdout += chunk; });
    child.stderr.on('data', chunk => { stderr += chunk; });
    child.once('error', reject);
    child.once('close', status => resolve({ status, stdout, stderr }));
  });
}

function project(name) {
  const root = path.join(scratch, name);
  fs.mkdirSync(path.join(root, 'test'), { recursive: true });
  fs.writeFileSync(path.join(root, 'fpm.toml'), `name = "${name}"\n`);
  fs.writeFileSync(path.join(root, 'test', 'test_smoke.f90'), [
    'program test_smoke', 'implicit none', 'end program test_smoke', ''
  ].join('\n'));
  return root;
}

const adapter = path.join(scratch, 'worker.js');
fs.writeFileSync(adapter, [
  "const fs = require('node:fs');",
  "const [events, gate, id, result, ...args] = process.argv.slice(2);",
  "function record(phase) { fs.appendFileSync(events, JSON.stringify({phase,id,pid:process.pid,args,at:Date.now()})+'\\n'); }",
  "record('start');",
  "function finish() { record('end'); process.exit(Number(result)); }",
  "if (gate === '-') setTimeout(finish, 120);",
  "else { const timer=setInterval(()=>{ if(fs.existsSync(gate)){clearInterval(timer);finish();}},10); }",
  ''
].join('\n'));

const resistantAdapter = path.join(scratch, 'resistant.js');
fs.writeFileSync(resistantAdapter, [
  "const fs = require('node:fs');",
  "const { spawn } = require('node:child_process');",
  "const [role, events] = process.argv.slice(2);",
  "function record(phase) { fs.appendFileSync(events, JSON.stringify({phase,id:role,pid:process.pid,at:Date.now()})+'\\n'); }",
  "process.on('SIGTERM', () => record('term'));",
  "record('start');",
  "if (role === 'parent') spawn(process.execPath, [__filename, 'child', events], {stdio:'ignore'});",
  "setInterval(() => record('heartbeat'), 25);",
  ''
].join('\n'));

function task(events, id, options = {}) {
  return {
    id,
    ...(options.worktree ? { worktree: options.worktree } : {}),
    ...(options.depends_on ? { depends_on: options.depends_on } : {}),
    ...(options.files ? { files: options.files } : {}),
    ...(options.apis ? { apis: options.apis } : {}),
    ...(options.abis ? { abis: options.abis } : {}),
    ...(options.resources ? { resources: options.resources } : {}),
    argv: [process.execPath, adapter, events, options.gate || '-', id,
      String(options.exit || 0), ...(options.args || [])]
  };
}

function saveTasks(name, tasks) {
  const filename = path.join(scratch, `${name}.json`);
  fs.writeFileSync(filename, JSON.stringify(tasks));
  return filename;
}

function eventsFor(filename) {
  if (!fs.existsSync(filename)) return [];
  return fs.readFileSync(filename, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line));
}

function wait(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }

function cleanScratch() {
  function unlockDirs(dir) {
    if (!fs.existsSync(dir)) return;
    fs.chmodSync(dir, 0o700);
    for (const entry of fs.readdirSync(dir)) {
      const child = path.join(dir, entry);
      if (fs.lstatSync(child).isDirectory()) unlockDirs(child);
    }
  }
  unlockDirs(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
}

async function until(predicate, message, timeoutMs = 15000) {
  const end = Date.now() + timeoutMs;
  while (Date.now() < end) {
    const value = predicate();
    if (value) return value;
    await wait(30);
  }
  throw new Error(`timed out: ${message}`);
}

async function waitTasks(root, states, timeoutMs = 15000, environment = env) {
  return until(() => {
    const status = json(['work', 'status', '--dir', root], root, environment);
    if (status.tasks && status.tasks.every(t => states.includes(t.state))) return status;
    return null;
  }, `tasks reach ${states.join(',')}`, timeoutMs);
}

async function cancel(root, environment = env) {
  const status = json(['work', 'cancel', '--dir', root], root, environment);
  assert.equal(status.status, 'cancel_requested');
  const finished = await until(() => {
    const current = json(['work', 'status', '--dir', root], root, environment);
    return current.status === 'cancelled' ? current : null;
  }, 'work owner to publish cancellation', 15000);
  activeRoots.delete(root);
  rootEnvs.delete(root);
  return finished;
}

function sendMcp(child, id, method, params) {
  return new Promise((resolve, reject) => {
    let buffer = '';
    const timeout = setTimeout(() => {
      child.stdout.off('data', onData);
      reject(new Error(`MCP request timed out: ${method}`));
    }, 15000);
    function onData(chunk) {
      buffer += chunk.toString();
      const lines = buffer.split('\n');
      buffer = lines.pop();
      for (const line of lines) {
        if (!line.trim()) continue;
        let response;
        try { response = JSON.parse(line); } catch (_) { continue; }
        if (response.id !== id) continue;
        clearTimeout(timeout);
        child.stdout.off('data', onData);
        resolve(response);
      }
    }
    child.stdout.on('data', onData);
    child.stdin.write(`${JSON.stringify({ jsonrpc: '2.0', id, method, params })}\n`);
  });
}

async function mcpText(child, id, argumentsObject) {
  const response = await sendMcp(child, id, 'tools/call', {
    name: 'fo', arguments: argumentsObject
  });
  assert.ok(response.result, JSON.stringify(response));
  const text = response.result.content.find(item => item.type === 'text').text;
  return JSON.parse(text);
}

async function main() {
  const sameRoot = project('work_modes_simultaneous');
  const sameEvents = path.join(scratch, 'simultaneous.jsonl');
  const sameGate = path.join(scratch, 'simultaneous-gate');
  const sameFile = saveTasks('simultaneous', [
    task(sameEvents, 'one-owner', { gate: sameGate })]);
  const sameArgs = ['work', 'start', '--mode', 'parallel', '--max-workers', '1',
    '--tasks', sameFile, '--dir', sameRoot];
  activeRoots.add(sameRoot);
  const sameStarts = await Promise.all(Array.from({ length: 6 }, () =>
    runAsync(sameArgs, sameRoot)));
  assert.ok(sameStarts.every(result => result.status === 0),
    JSON.stringify(sameStarts));
  const sameIds = sameStarts.map(result => JSON.parse(result.stdout).work_id);
  assert.equal(new Set(sameIds).size, 1, 'simultaneous starts attach to one owner');
  await until(() => eventsFor(sameEvents).length === 1,
    'one adapter to start for simultaneous requests');
  fs.writeFileSync(sameGate, 'go');
  await waitTasks(sameRoot, ['succeeded']);
  assert.equal(eventsFor(sameEvents).filter(event => event.phase === 'start').length, 1);
  await cancel(sameRoot);

  const differentRoot = project('work_modes_incompatible');
  const differentEvents = path.join(scratch, 'incompatible.jsonl');
  const differentGate = path.join(scratch, 'incompatible-gate');
  const differentArgs = ['left', 'right'].map(id => ['work', 'start',
    '--mode', 'parallel', '--max-workers', '1', '--tasks', saveTasks(id, [
      task(differentEvents, id, { gate: differentGate })]), '--dir', differentRoot]);
  activeRoots.add(differentRoot);
  const differentStarts = await Promise.all(differentArgs.map(args =>
    runAsync(args, differentRoot)));
  assert.deepEqual(differentStarts.map(result => result.status).sort(), [0, 2]);
  assert.match(differentStarts.find(result => result.status === 2).stdout,
    /incompatible request/);
  await until(() => eventsFor(differentEvents).length === 1,
    'only the selected policy to launch a worker');
  fs.writeFileSync(differentGate, 'go');
  await waitTasks(differentRoot, ['succeeded']);
  assert.equal(eventsFor(differentEvents).filter(event => event.phase === 'start').length, 1);
  await cancel(differentRoot);

  const root = project('work_modes_probe');
  const events = path.join(scratch, 'events.jsonl');
  const gateB = path.join(scratch, 'gate-b');
  const gateC = path.join(scratch, 'gate-c');
  const literal = `space ; $(touch ${path.join(scratch, 'shell-ran')})`;
  const tasks = [
    task(events, 'alpha', { args: [literal] }),
    task(events, 'beta', { gate: gateB }),
    task(events, 'gamma', { gate: gateC })
  ];
  const taskFile = saveTasks('concurrency', tasks);
  const started = json(['work', 'start', '--mode', 'parallel', '--max-workers', '2',
    '--tasks', taskFile, '--dir', root], root);
  activeRoots.add(root);
  assert.equal(started.mode, 'parallel');
  assert.equal(started.max_workers, 2);
  assert.equal(started.may_promote, false);
  const attached = json(['work', 'start', '--mode', 'parallel', '--max-workers', '2',
    '--tasks', taskFile, '--dir', root], root);
  assert.equal(attached.work_id, started.work_id, 'same request attaches to one owner');
  await until(() => eventsFor(events).filter(event => event.phase === 'start').length === 2,
    'two independent workers to fill configured capacity');
  assert.deepEqual(eventsFor(events).filter(event => event.phase === 'start')
    .map(event => event.id).sort(), ['alpha', 'beta']);
  await until(() => eventsFor(events).some(event => event.phase === 'start' && event.id === 'gamma'),
    'third task to start after a worker slot is released');
  assert.ok(eventsFor(events).some(event => event.id === 'beta' && event.phase === 'start'));
  assert.ok(!eventsFor(events).some(event => event.id === 'beta' && event.phase === 'end'),
    'finishing alpha must leave beta alive while gamma starts');
  fs.writeFileSync(gateB, 'go');
  fs.writeFileSync(gateC, 'go');
  const allDone = await waitTasks(root, ['succeeded']);
  assert.equal(allDone.spawned_workers, 3);
  const specialEvent = eventsFor(events).find(event => event.id === 'alpha' && event.phase === 'start');
  assert.equal(specialEvent.args[0], literal, 'adapter argv tokens must remain unchanged');
  assert.equal(fs.existsSync(path.join(scratch, 'shell-ran')), false,
    'adapter arguments must never be interpolated by a shell');
  const startsAndEnds = eventsFor(events);
  assert.ok(startsAndEnds.findIndex(event => event.id === 'gamma' &&
    event.phase === 'start') > startsAndEnds.findIndex(event => event.id === 'alpha' &&
    event.phase === 'end'), 'third task starts after a worker slot is released');
  let active = 0, peak = 0;
  for (const event of startsAndEnds) {
    active += event.phase === 'start' ? 1 : -1;
    peak = Math.max(peak, active);
  }
  assert.equal(peak, 2, 'global/session worker leases must cap simultaneous adapters');
  await cancel(root);

  const laneA = project('work_modes_lane_a');
  const laneB = project('work_modes_lane_b');
  const laneEvents = path.join(scratch, 'lane.jsonl');
  const laneGateA = path.join(scratch, 'lane-gate-a');
  const laneGateB = path.join(scratch, 'lane-gate-b');
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '1', '--tasks',
    saveTasks('lane-a', [task(laneEvents, 'lane-a', { gate: laneGateA })]),
    '--dir', laneA], laneA);
  activeRoots.add(laneA);
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '1', '--tasks',
    saveTasks('lane-b', [task(laneEvents, 'lane-b', { gate: laneGateB })]),
    '--dir', laneB], laneB);
  activeRoots.add(laneB);
  await until(() => eventsFor(laneEvents).filter(event => event.phase === 'start').length === 2,
    'independent project workers to start');
  await cancel(laneA);
  assert.equal(json(['work', 'status', '--dir', laneB], laneB).tasks[0].state,
    'running', 'cancelling one project leaves the other worker running');
  assert.ok(!eventsFor(laneEvents).some(event => event.id === 'lane-b' &&
    event.phase === 'end'));
  fs.writeFileSync(laneGateB, 'go');
  await waitTasks(laneB, ['succeeded']);
  await cancel(laneB);

  const conflictRoot = project('work_modes_conflicts');
  const conflictEvents = path.join(scratch, 'conflicts.jsonl');
  const conflictGate = path.join(scratch, 'conflict-gate');
  const conflictTasks = [
    task(conflictEvents, 'owner', { gate: conflictGate, files: ['src/shared.f90'],
      apis: ['shared_api'], abis: ['shared_abi'] }),
    task(conflictEvents, 'conflict', { gate: conflictGate, files: ['src/shared.f90'] }),
    task(conflictEvents, 'independent', { gate: conflictGate, files: ['src/other.f90'] }),
    task(conflictEvents, 'dependent', { gate: conflictGate, depends_on: ['owner'] })
  ];
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '4', '--tasks',
    saveTasks('conflicts', conflictTasks), '--dir', conflictRoot], conflictRoot);
  activeRoots.add(conflictRoot);
  await until(() => eventsFor(conflictEvents).filter(event => event.phase === 'start').length === 2,
    'independent tasks to start while overlap/dependency tasks wait');
  let current = await until(() => {
    const status = json(['work', 'status', '--dir', conflictRoot], conflictRoot);
    if (status.tasks.find(t => t.id === 'conflict').blocked_reason !==
        'ownership_conflict') return null;
    if (status.tasks.find(t => t.id === 'dependent').blocked_reason !==
        'waiting_for_dependencies') return null;
    return status;
  }, 'ownership and dependency blockers to be published');
  assert.equal(current.tasks.find(t => t.id === 'conflict').blocked_reason,
    'ownership_conflict');
  assert.equal(current.tasks.find(t => t.id === 'dependent').blocked_reason,
    'waiting_for_dependencies');
  assert.ok(!eventsFor(conflictEvents).some(event => event.id === 'conflict' && event.phase === 'start'));
  fs.writeFileSync(conflictGate, 'go');
  current = await waitTasks(conflictRoot, ['succeeded']);
  assert.equal(current.tasks.length, 4);
  const timeline = eventsFor(conflictEvents);
  const ownerEnd = timeline.findIndex(event => event.id === 'owner' && event.phase === 'end');
  const conflictStart = timeline.findIndex(event => event.id === 'conflict' && event.phase === 'start');
  const dependentStart = timeline.findIndex(event => event.id === 'dependent' && event.phase === 'start');
  assert.ok(conflictStart > ownerEnd, 'conflicting file owners must run sequentially');
  assert.ok(dependentStart > ownerEnd, 'dependents start only after successful prerequisites');
  await cancel(conflictRoot);

  const resourceRoot = project('work_modes_resource');
  const resourceEvents = path.join(scratch, 'resource.jsonl');
  const resourceGate = path.join(scratch, 'resource-gate');
  const resourceTasks = [
    task(resourceEvents, 'test-one', { gate: resourceGate, resources: ['test'] }),
    task(resourceEvents, 'test-two', { gate: resourceGate, resources: ['test'] })
  ];
  const resourceEnv = { ...env, FO_WORK_TEST_CAPACITY: '2' };
  fs.writeFileSync(path.join(scratch, 'resource.json'), JSON.stringify(resourceTasks));
  const resourceStart = spawnSync(fo, ['work', 'start', '--mode', 'parallel',
    '--max-workers', '2', '--tasks', path.join(scratch, 'resource.json'),
    '--dir', resourceRoot], { cwd: resourceRoot, env: resourceEnv, encoding: 'utf8', timeout: 30000 });
  assert.equal(resourceStart.status, 0, resourceStart.stdout + resourceStart.stderr);
  activeRoots.add(resourceRoot);
  await until(() => eventsFor(resourceEvents).filter(event => event.phase === 'start').length === 1,
    'campaign lease to leave one declared test slot');
  await wait(150);
  assert.equal(eventsFor(resourceEvents).filter(event => event.phase === 'start').length, 1,
    'shared test-resource lease must hold the second task');
  fs.writeFileSync(resourceGate, 'go');
  await waitTasks(resourceRoot, ['succeeded']);
  await cancel(resourceRoot);

  const capOneRoot = project('work_modes_test_capacity_one');
  const capOneEnv = { ...env, FO_GREMLIN_STATE_DIR: path.join(scratch, 'cap-one-state'),
    FO_WORK_TEST_CAPACITY: '1' };
  const capOneEvents = path.join(scratch, 'cap-one.jsonl');
  const capOneGateA = path.join(scratch, 'cap-one-a');
  const capOneGateB = path.join(scratch, 'cap-one-b');
  const capOneTasks = [
    task(capOneEvents, 'first', { gate: capOneGateA, resources: ['test'] }),
    task(capOneEvents, 'second', { gate: capOneGateB, resources: ['test'] })
  ];
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '2', '--tasks',
    saveTasks('cap-one', capOneTasks), '--dir', capOneRoot], capOneRoot, capOneEnv);
  activeRoots.add(capOneRoot);
  rootEnvs.set(capOneRoot, capOneEnv);
  await until(() => eventsFor(capOneEvents).some(event => event.id === 'first' &&
    event.phase === 'start'), 'test task to start with capacity one');
  await wait(150);
  assert.ok(!eventsFor(capOneEvents).some(event => event.id === 'second' &&
    event.phase === 'start'), 'second test task must wait for the sole test slot');
  fs.writeFileSync(capOneGateA, 'go');
  await until(() => eventsFor(capOneEvents).some(event => event.id === 'second' &&
    event.phase === 'start'), 'second test task to start after first finishes');
  const capOneTimeline = eventsFor(capOneEvents);
  assert.ok(capOneTimeline.findIndex(event => event.id === 'first' &&
    event.phase === 'end') < capOneTimeline.findIndex(event => event.id === 'second' &&
    event.phase === 'start'));
  fs.writeFileSync(capOneGateB, 'go');
  await waitTasks(capOneRoot, ['succeeded'], 15000, capOneEnv);
  await cancel(capOneRoot, capOneEnv);

  const resistantEnv = { ...env,
    FO_GREMLIN_STATE_DIR: path.join(scratch, 'resistant-state'),
    FO_WORK_CPU_CAPACITY: '1' };
  const resistantRoot = project('work_modes_resistant');
  const followerRoot = project('work_modes_follower');
  const resistantEvents = path.join(scratch, 'resistant.jsonl');
  const followerEvents = path.join(scratch, 'follower.jsonl');
  const followerGate = path.join(scratch, 'follower-gate');
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '1', '--tasks',
    saveTasks('resistant', [{ id: 'resistant',
      argv: [process.execPath, resistantAdapter, 'parent', resistantEvents] }]),
    '--dir', resistantRoot], resistantRoot, resistantEnv);
  activeRoots.add(resistantRoot);
  rootEnvs.set(resistantRoot, resistantEnv);
  await until(() => eventsFor(resistantEvents).filter(event =>
    event.phase === 'start').length === 2, 'resistant worker and child to start');
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '1', '--tasks',
    saveTasks('follower', [task(followerEvents, 'follower', { gate: followerGate })]),
    '--dir', followerRoot], followerRoot, resistantEnv);
  activeRoots.add(followerRoot);
  rootEnvs.set(followerRoot, resistantEnv);
  await until(() => json(['work', 'status', '--dir', followerRoot],
    followerRoot, resistantEnv).tasks[0].blocked_reason === 'cpu_capacity',
  'follower to wait for the resistant worker lease');
  assert.equal(json(['work', 'cancel', '--dir', resistantRoot], resistantRoot,
    resistantEnv).status, 'cancel_requested');
  await until(() => eventsFor(resistantEvents).filter(event =>
    event.phase === 'term').length === 2, 'both processes to ignore SIGTERM');
  assert.equal(json(['work', 'status', '--dir', resistantRoot], resistantRoot,
    resistantEnv).status, 'cancelling');
  assert.equal(eventsFor(followerEvents).length, 0,
    'follower cannot take the lease while the resistant tree is alive');
  await until(() => json(['work', 'status', '--dir', resistantRoot],
    resistantRoot, resistantEnv).status === 'cancelled',
  'resistant tree to be killed before terminal status', 15000);
  activeRoots.delete(resistantRoot);
  rootEnvs.delete(resistantRoot);
  await until(() => eventsFor(followerEvents).some(event => event.phase === 'start'),
    'follower to start after resistant tree termination');
  for (const event of eventsFor(resistantEvents).filter(item => item.phase === 'start')) {
    assert.throws(() => process.kill(event.pid, 0), { code: 'ESRCH' },
      'registered worker and child must both be dead before lease reuse');
  }
  fs.writeFileSync(followerGate, 'go');
  await waitTasks(followerRoot, ['succeeded'], 15000, resistantEnv);
  await cancel(followerRoot, resistantEnv);

  const failedRoot = project('work_modes_failure');
  const failedEvents = path.join(scratch, 'failed.jsonl');
  const failedTasks = [
    task(failedEvents, 'bad', { exit: 9 }),
    task(failedEvents, 'after-bad', { depends_on: ['bad'] }),
    task(failedEvents, 'unrelated')
  ];
  json(['work', 'start', '--mode', 'parallel', '--max-workers', '2', '--tasks',
    saveTasks('failed', failedTasks), '--dir', failedRoot], failedRoot);
  activeRoots.add(failedRoot);
  current = await waitTasks(failedRoot, ['failed', 'skipped', 'succeeded']);
  assert.equal(current.tasks.find(t => t.id === 'bad').state, 'failed');
  assert.equal(current.tasks.find(t => t.id === 'after-bad').state, 'skipped');
  assert.equal(current.tasks.find(t => t.id === 'unrelated').state, 'succeeded');
  await cancel(failedRoot);

  const serialRoot = project('work_modes_serial');
  const serial = json(['work', 'start', '--mode', 'serial', '--dir', serialRoot], serialRoot);
  activeRoots.add(serialRoot);
  assert.equal(serial.editor, 'main-session');
  assert.equal(serial.spawned_workers, 0);
  const serialStatus = json(['work', 'status', '--dir', serialRoot], serialRoot);
  assert.equal(serialStatus.mode, 'serial');
  await cancel(serialRoot);

  const mcpRoot = project('work_modes_mcp');
  const client = spawn(fo, ['mcp-server'], { cwd: mcpRoot, env,
    stdio: ['pipe', 'pipe', 'pipe'] });
  let mcpStarted = false;
  try {
    await sendMcp(client, 1, 'initialize', { protocolVersion: '2025-03-26', capabilities: {},
      clientInfo: { name: 'work-mode-verifier', version: '1' } });
    const listed = await sendMcp(client, 2, 'tools/list', {});
    const schema = listed.result.tools[0].inputSchema;
    assert.ok(schema.properties.action.enum.includes('work_start'));
    assert.ok(schema.properties.tasks);
    const mcpStart = await mcpText(client, 3, { action: 'work_start', dir: mcpRoot,
      mode: 'serial' });
    mcpStarted = true;
    activeRoots.add(mcpRoot);
    assert.equal(mcpStart.editor, 'main-session');
    const cliStatus = json(['work', 'status', '--dir', mcpRoot], mcpRoot);
    const mcpStatus = await mcpText(client, 4, { action: 'work_status', dir: mcpRoot });
    assert.equal(mcpStatus.work_id, cliStatus.work_id);
    assert.equal(mcpStatus.mode, cliStatus.mode);
    const mcpCancel = await mcpText(client, 5, { action: 'work_cancel', dir: mcpRoot });
    assert.equal(mcpCancel.status, 'cancel_requested');
    await until(() => json(['work', 'status', '--dir', mcpRoot], mcpRoot).status === 'cancelled',
      'MCP cancellation to reach the shared core owner');
    mcpStarted = false;

    const mcpParallelRoot = project('work_modes_mcp_parallel');
    const mcpEvents = path.join(scratch, 'mcp-parallel.jsonl');
    const mcpGate = path.join(scratch, 'mcp-gate');
    const mcpParallel = await mcpText(client, 6, { action: 'work_start',
      dir: mcpParallelRoot, mode: 'parallel', max_workers: 1,
      tasks: [task(mcpEvents, 'mcp-worker', { gate: mcpGate })] });
    activeRoots.add(mcpParallelRoot);
    assert.equal(mcpParallel.mode, 'parallel');
    await until(() => eventsFor(mcpEvents).some(event => event.phase === 'start'),
      'MCP parallel task to start through the shared worker adapter');
    assert.equal(json(['work', 'status', '--dir', mcpParallelRoot], mcpParallelRoot).work_id,
      mcpParallel.work_id);
    fs.writeFileSync(mcpGate, 'go');
    await waitTasks(mcpParallelRoot, ['succeeded']);
    const mcpParallelCancel = await mcpText(client, 7, {
      action: 'work_cancel', dir: mcpParallelRoot });
    assert.equal(mcpParallelCancel.status, 'cancel_requested');
    await until(() => json(['work', 'status', '--dir', mcpParallelRoot],
      mcpParallelRoot).status === 'cancelled', 'MCP parallel cancellation');
    activeRoots.delete(mcpParallelRoot);
  } finally {
    if (mcpStarted) await cancel(mcpRoot).catch(() => {});
    client.kill('SIGTERM');
  }
  await new Promise(resolve => client.once('close', resolve));
  console.log('work modes: ok');
  cleanScratch();
}

main().catch(async error => {
  console.error(error.stack || error);
  for (const root of [...activeRoots]) {
    await cancel(root, rootEnvs.get(root) || env).catch(() => {});
  }
  cleanScratch();
  process.exitCode = 1;
});
