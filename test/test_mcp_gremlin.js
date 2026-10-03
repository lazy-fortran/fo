#!/usr/bin/env node
// Exercise Gremlin through the real MCP server and shared CLI core.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-gremlin-');
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('mcp-gremlin: skipped (requires Linux async process containment)');
  process.exit(0);
}
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'gremlin-state'),
  TMPDIR: '/var/tmp', FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1',
  FO_CACHE_DIR: path.join(scratch, 'cache') };
fs.mkdirSync(env.HOME, { recursive: true });

function runFo(args, cwd) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', cwd, 'fo', ...args];
  const result = spawnSync(driver, command, {
    cwd: installed ? cwd : project, env, encoding: 'utf8',
    timeout: 120000, maxBuffer: 8 * 1024 * 1024
  });
  if (result.error) throw result.error;
  return result;
}

function writeFixture(dir, markerRoot) {
  fs.mkdirSync(path.join(dir, 'test'), { recursive: true });
  fs.writeFileSync(path.join(dir, 'fpm.toml'), 'name = "mcp_gremlin_probe"\n');
  const marker = (name, value) => [
    `open(newunit=unit, file='${path.join(markerRoot, name)}', status='unknown', position='append')`,
    `write(unit, '(a)') '${value}'`, 'close(unit)'
  ].join('\n');
  const source = (name, body, gated = false) => [
    `program ${name}`, 'implicit none', 'integer :: unit',
    ...(gated ? ['character :: gate_token'] : []),
    body, `end program ${name}`, ''
  ].join('\n');
  fs.writeFileSync(path.join(dir, 'test/test_mcp_pass.f90'), source(
    'test_mcp_pass', marker('mcp_pass.done', 'pass')));
  fs.writeFileSync(path.join(dir, 'test/test_mcp_fail.f90'), source(
    'test_mcp_fail', marker('mcp_fail.done', 'fail') + '\nerror stop 7'));
  fs.writeFileSync(path.join(dir, 'test/test_mcp_blocked.f90'), source(
    'test_mcp_blocked', [
      marker('mcp_blocked.started', 'started'),
      `open(newunit=unit, file='${path.join(markerRoot, 'mcp_blocked.gate')}', &`,
      "    status='old', access='stream', form='unformatted', action='read')",
      'read(unit) gate_token', 'close(unit)', marker('mcp_blocked.done', 'done')
    ].join('\n'), true));
}

function writeParityFixture(dir) {
  fs.mkdirSync(path.join(dir, 'test'), { recursive: true });
  fs.writeFileSync(path.join(dir, 'fpm.toml'), 'name = "mcp_gremlin_parity_probe"\n');
  for (const suffix of ['a', 'b', 'c']) {
    const name = `test_parity_${suffix}`;
    fs.writeFileSync(path.join(dir, `test/${name}.f90`), [
      `program ${name}`, 'implicit none', `end program ${name}`, ''
    ].join('\n'));
  }
}

function createGate(file) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  const result = spawnSync('mkfifo', [file], { encoding: 'utf8' });
  assert.equal(result.status, 0, result.stdout + result.stderr);
}

function releaseGate(file) {
  return new Promise((resolve, reject) => {
    const writer = spawn(process.execPath, ['-e',
      "require('node:fs').writeFileSync(process.argv[process.argv.length - 1], 'x')",
      file],
    { stdio: 'ignore' });
    const timer = setTimeout(() => {
      writer.kill('SIGKILL');
      reject(new Error(`timed out releasing MCP test gate ${file}`));
    }, 3000);
    writer.once('error', error => { clearTimeout(timer); reject(error); });
    writer.once('close', code => {
      clearTimeout(timer);
      if (code === 0) resolve();
      else reject(new Error(`MCP gate writer exited ${code}: ${file}`));
    });
  });
}

function startServer(cwd) {
  const args = installed ? ['mcp-server']
    : ['exec', '--no-build', '--cwd', cwd, 'fo', 'mcp-server'];
  const child = spawn(driver, args, { cwd: installed ? cwd : project, env });
  let buffer = '';
  let stderr = '';
  const pending = [];
  child.stderr.on('data', chunk => { stderr += chunk.toString(); });
  child.stdout.on('data', chunk => {
    buffer += chunk.toString();
    let newline;
    while ((newline = buffer.indexOf('\n')) >= 0 && pending.length > 0) {
      const body = buffer.slice(0, newline);
      buffer = buffer.slice(newline + 1);
      const waiter = pending.shift();
      clearTimeout(waiter.timer);
      try { waiter.resolve(JSON.parse(body)); } catch (error) { waiter.reject(error); }
    }
  });
  function rpc(id, method, params = {}) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id, method, params }) + '\n');
    });
  }
  function call(id, args) {
    return rpc(id, 'tools/call', { name: 'fo', arguments: args });
  }
  return { child, call, rpc, stderr: () => stderr };
}

function waitForChildClose(child, timeoutMs) {
  return new Promise((resolve, reject) => {
    if (child.exitCode !== null || child.signalCode !== null) return resolve();
    const timer = setTimeout(() => reject(new Error('MCP server did not exit')), timeoutMs);
    child.once('close', () => { clearTimeout(timer); resolve(); });
  });
}

function payload(response) {
  assert.equal(response.error, undefined, JSON.stringify(response));
  assert.ok(response.result, 'MCP returns a tool result');
  const text = response.result.content[0].text;
  return { body: JSON.parse(text), isError: response.result.isError };
}

async function waitFor(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`timed out waiting for ${file}`);
}

async function waitForInitialSample(readStatus, seed, expectedCount, label) {
  const deadline = Date.now() + 30000;
  let latest = {};
  while (Date.now() < deadline) {
    latest = await readStatus();
    const events = Array.isArray(latest.events) ? latest.events : [];
    const sample = events.filter(event => event.seed === seed);
    if (latest.seed !== seed && sample.length >= expectedCount) return sample;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error(`timed out waiting for the completed seeded sample from ${label}: `
    + JSON.stringify(latest));
}

async function waitForStopped(cwd, lane, sessionId, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const result = runFo(['gremlin', 'status', '--lane', lane, '--session',
      sessionId, '--dir', cwd, '--json'], cwd);
    if (result.status === 0) {
      try {
        if (JSON.parse(result.stdout.trim()).state === 'stopped') return;
      } catch (_) { /* status may be between owner publication and reconnect */ }
    }
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error(`Gremlin owner ${sessionId} did not stop`);
}

function gremlinOwnerPids(dir, lane) {
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
          dirAt >= 0 && args[dirAt + 1] === dir) owners.push(Number(entry));
    } catch (_) { /* process exited during scan */ }
  }
  return owners;
}

function ownerProcessGroup(pid) {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
  const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
  return { group: Number(fields[2]), session: Number(fields[3]) };
}

async function waitForOwnerExit(dir, lane, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (gremlinOwnerPids(dir, lane).length === 0) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`Gremlin owner remained after cleanup: ${lane}`);
}

async function forceStopOwnedGremlinGroup(dir, lane) {
  const signalGroups = signal => {
    for (const pid of gremlinOwnerPids(dir, lane)) {
      const { group, session } = ownerProcessGroup(pid);
      assert.equal(session, pid, `refusing to signal non-owner session ${pid}`);
      assert.equal(group, pid, `refusing to signal non-owner group ${pid}`);
      try { process.kill(-group, signal); }
      catch (error) { if (error.code !== 'ESRCH') throw error; }
    }
  };
  signalGroups('SIGTERM');
  try { await waitForOwnerExit(dir, lane, 1500); return; }
  catch (_) { /* escalate only within the verified Gremlin owner group */ }
  signalGroups('SIGKILL');
  await waitForOwnerExit(dir, lane, 5000);
}

async function cleanupOwner(server, cwd, lane, knownId, mcpOwned, requestId) {
  let ownerId = knownId;
  if (!ownerId) {
    try {
      const current = runFo(['gremlin', 'status', '--lane', lane,
        '--dir', cwd, '--json'], cwd);
      if (current.status === 0) ownerId = JSON.parse(current.stdout.trim()).session_id;
    } catch (_) { /* continue to exact process-group cleanup */ }
  }
  if (!ownerId) {
    const liveOwners = gremlinOwnerPids(cwd, lane);
    if (liveOwners.length > 0) {
      await forceStopOwnedGremlinGroup(cwd, lane);
      throw new Error(`no session ID was recoverable; terminated exact owned process group: ${lane}`);
    }
    return;
  }

  let lastError;
  for (let attempt = 0; attempt < 3; attempt++) {
    let current;
    try {
      current = runFo(['gremlin', 'status', '--lane', lane,
        '--session', ownerId, '--dir', cwd, '--json'], cwd);
    } catch (_) { /* stop and exact process-group fallback still have a chance */ }
    if (current && current.status === 0) {
      try {
        if (JSON.parse(current.stdout.trim()).state === 'stopped') {
          await waitForOwnerExit(cwd, lane, 5000);
          return;
        }
      } catch (_) { /* the owner may be between state publications */ }
    }
    try {
      if (mcpOwned) {
        const response = payload(await server.call(requestId.value++, {
          action: 'gremlin_stop', dir: cwd, lane_id: lane, session_id: ownerId
        }));
        assert.equal(response.isError, false, JSON.stringify(response.body));
      } else {
        const stopped = runFo(['gremlin', 'stop', '--lane', lane,
          '--session', ownerId, '--dir', cwd, '--json'], cwd);
        assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
      }
      await waitForStopped(cwd, lane, ownerId, 3000);
      await waitForOwnerExit(cwd, lane, 5000);
      return;
    } catch (error) { lastError = error; }
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  const liveOwners = gremlinOwnerPids(cwd, lane);
  if (liveOwners.length > 0) {
    await forceStopOwnedGremlinGroup(cwd, lane);
    throw new Error(`API stop failed; exact owned process group was terminated: ${lane}; `
      + String(lastError));
  }
  try {
    await waitForStopped(cwd, lane, ownerId, 3000);
    await waitForOwnerExit(cwd, lane, 5000);
  } catch (verificationError) {
    throw new AggregateError([lastError, verificationError],
      `stop retries ended without verifiable owner termination: ${lane}`);
  }
}

async function main() {
  const fixture = path.join(scratch, 'project');
  const parityFixture = path.join(scratch, 'parity-project');
  const markerRoot = path.join(scratch, 'markers');
  const blockedGate = path.join(markerRoot, 'mcp_blocked.gate');
  createGate(blockedGate);
  writeFixture(fixture, markerRoot);
  writeParityFixture(parityFixture);
  let sessionId = '';
  let parityMcpSessionId = '';
  let parityCliSessionId = '';
  let primaryError;
  if (!installed) {
    const built = spawnSync(driver, ['build'], { cwd: project, env, encoding: 'utf8',
      timeout: 120000, maxBuffer: 8 * 1024 * 1024 });
    assert.equal(built.status, 0, built.stdout + built.stderr);
  }

  const server = startServer(fixture);
  try {
    const init = await server.rpc(1, 'initialize', {
      protocolVersion: '2025-11-25', capabilities: {}
    });
    assert.equal(init.result.protocolVersion, '2025-11-25');
    // Use the MCP tool list to verify the Gremlin actions are discoverable.
    const listed = await server.rpc(2, 'tools/list');
    const actions = listed.result.tools[0].inputSchema.properties.action.enum;
    for (const action of ['gremlin_start', 'gremlin_status', 'gremlin_wait',
      'gremlin_events', 'gremlin_failures', 'gremlin_reproduce', 'gremlin_stop']) {
      assert.ok(actions.includes(action), `tools/list advertises ${action}`);
    }

    const startedAt = Date.now();
    const startResult = payload(await server.call(3, {
      action: 'gremlin_start', dir: fixture, lane_id: 'mcp-probe',
      targets: ['test_mcp_pass', 'test_mcp_fail', 'test_mcp_blocked'],
      seed: 1729, timeout_seconds: 5, campaign_seconds: 60
    }));
    assert.equal(startResult.isError, false, JSON.stringify(startResult.body));
    assert.ok(Date.now() - startedAt < 3000, 'background start returns promptly');
    assert.ok(['running', 'testing'].includes(startResult.body.state),
      `start returns an active session, got ${startResult.body.state}`);
    assert.ok(startResult.body.session_id, 'start returns a reconnectable session id');
    sessionId = startResult.body.session_id;
    const duplicate = runFo(['gremlin', 'start', '--lane', 'mcp-probe',
      '--dir', fixture, '--target', 'test_mcp_pass', '--target', 'test_mcp_fail',
      '--target', 'test_mcp_blocked', '--campaign-seconds', '60',
      '--seed', '1729', '--timeout-seconds', '5'], fixture);
    assert.equal(duplicate.status, 0, duplicate.stdout + duplicate.stderr);
    const duplicateState = JSON.parse(duplicate.stdout.trim());
    assert.equal(duplicateState.session_id, sessionId,
      'CLI start attaches to the MCP-owned session');
    assert.equal(duplicateState.state, 'attached');

    const invalidInputs = [
      { name: 'random_count upper bound', cli: ['--random', '33'],
        mcp: { random_count: 33 } },
      { name: 'unknown property', cli: ['--not-a-field', 'value'],
        mcp: { not_a_field: 'value' } }
    ];
    let invalidId = 20;
    for (const invalid of invalidInputs) {
      const invalidLane = `invalid-${invalidId}`;
      const cli = runFo(['gremlin', 'start', '--dir', fixture,
        '--lane', invalidLane, ...invalid.cli], fixture);
      assert.notEqual(cli.status, 0, `${invalid.name} fails through CLI`);
      const cliBody = JSON.parse(cli.stdout.trim());
      const mcpResponse = await server.call(invalidId++, {
        action: 'gremlin_start', dir: fixture, lane_id: invalidLane,
        ...invalid.mcp
      });
      const mcpResult = payload(mcpResponse);
      assert.equal(mcpResult.isError, true,
        `${invalid.name} is returned as an MCP tool error`);
      const mcpBody = mcpResult.body;
      assert.deepEqual(mcpBody, cliBody,
        `${invalid.name} returns the same shared-core error through MCP and CLI`);
    }

    // No MCP requests are sent while these markers appear; the child must progress
    // independently of the idle client.
    await waitFor(path.join(markerRoot, 'mcp_pass.done'), 30000);
    await waitFor(path.join(markerRoot, 'mcp_fail.done'), 30000);
    await waitFor(path.join(markerRoot, 'mcp_blocked.started'), 30000);
    assert.equal(fs.existsSync(path.join(markerRoot, 'mcp_blocked.done')), false,
      'the third case is still running while the client is idle');

    const identity = { dir: fixture, lane_id: 'mcp-probe',
      session_id: startResult.body.session_id };
    const cliStatus = runFo(['gremlin', 'status', '--lane', 'mcp-probe',
      '--session', startResult.body.session_id, '--dir', fixture, '--json'], fixture);
    assert.equal(cliStatus.status, 0, cliStatus.stdout + cliStatus.stderr);
    const cliState = JSON.parse(cliStatus.stdout.trim());
    const mcpStatus = payload(await server.call(4, { action: 'gremlin_status',
      ...identity })).body;
    assert.deepEqual(cliState, mcpStatus, 'CLI and MCP status share the same core response');

    const waited = payload(await server.call(5, { action: 'gremlin_wait', ...identity,
      wait_ms: 100, cursor: 0, max_records: 8, max_bytes: 8192 })).body;
    assert.equal(waited.session_id, sessionId,
      'MCP wait reconnects to the detached owner by session ID');
    assert.equal(waited.state, 'testing',
      'wait observes the blocked test while the client is idle');

    const eventPage = payload(await server.call(6, { action: 'gremlin_events', ...identity,
      cursor: 0, max_records: 8, max_bytes: 8192 })).body;
    assert.ok(Array.isArray(eventPage.events), 'events action returns bounded receipts');
    const failureEvent = eventPage.events.find(event =>
      event.case_id === 'test_mcp_fail' && event.status === 'FAIL');
    assert.ok(failureEvent, 'events includes the known failing case');

    const failures = payload(await server.call(7, { action: 'gremlin_failures', ...identity,
      cursor: 0, max_records: 8, max_bytes: 8192 })).body;
    assert.ok(failures.events.some(event => event.case_id === 'test_mcp_fail' &&
      event.generation === failureEvent.generation && event.status === 'FAIL'),
    'failures returns the known failed receipt');

    const page = payload(await server.call(8, { action: 'gremlin_status', ...identity,
      cursor: 0, max_records: 8, max_bytes: 8192 })).body;
    assert.ok(Array.isArray(page.events), 'status returns bounded completion events');
    const verdicts = page.events.map(event => [event.case_id, event.status]);
    assert.ok(verdicts.some(([name, outcome]) => name === 'test_mcp_pass' && outcome === 'PASS'));
    assert.ok(verdicts.some(([name, outcome]) => name === 'test_mcp_fail' && outcome === 'FAIL'));
    assert.ok(!verdicts.some(([name]) => name === 'test_mcp_blocked'),
      'blocked case has no invented completion');

    const stopped = payload(await server.call(9, { action: 'gremlin_stop', ...identity }));
    assert.equal(stopped.isError, false, JSON.stringify(stopped.body));
    assert.equal(stopped.body.state, 'stopping');
    await waitForStopped(fixture, 'mcp-probe', sessionId, 10000);
    const reconnected = payload(await server.call(10, { action: 'gremlin_wait', ...identity,
      wait_ms: 100, cursor: 0, max_records: 8, max_bytes: 8192 })).body;
    assert.equal(reconnected.session_id, sessionId,
      'MCP wait reconnects after the original start request has returned');
    assert.equal(reconnected.state, 'stopped', 'MCP observes the completed stop');
    const failureMarker = path.join(markerRoot, 'mcp_fail.done');
    const failedRunsBefore = fs.existsSync(failureMarker)
      ? fs.readFileSync(failureMarker, 'utf8').trim().split('\n').filter(Boolean) : [];
    assert.ok(failedRunsBefore.length > 0,
      'the initial failing receipt came from a real test execution');
    const reproduced = payload(await server.call(11, {
      action: 'gremlin_reproduce', ...identity, case_id: failureEvent.case_id,
      generation_id: failureEvent.generation, timeout_seconds: 5
    }));
    assert.equal(reproduced.body.session_id, sessionId);
    assert.equal(reproduced.body.state, 'FAIL',
      'reproduce reruns the known failed test on its captured generation');
    const failedRunsAfter = fs.readFileSync(failureMarker, 'utf8').trim()
      .split('\n').filter(Boolean);
    assert.ok(failedRunsAfter.length > failedRunsBefore.length,
      'reproduce launches a new failing test execution instead of echoing its stored receipt');

    // Compare valid random-count and seed mapping across both adapters by the
    // exact cases each independently selects from the same frozen source set.
    const parityMcpStart = payload(await server.call(12, {
      action: 'gremlin_start', dir: parityFixture, lane_id: 'mcp-parity',
      random_count: 2, seed: 1729,
      timeout_seconds: 5, campaign_seconds: 1
    }));
    assert.equal(parityMcpStart.isError, false, JSON.stringify(parityMcpStart.body));
    parityMcpSessionId = parityMcpStart.body.session_id;
    assert.ok(parityMcpSessionId);
    let nextParityStatusId = 13;
    const parityCliStart = runFo(['gremlin', 'start', '--dir', parityFixture,
      '--lane', 'cli-parity', '--random', '2', '--seed', '1729',
      '--timeout-seconds', '5', '--campaign-seconds', '1'], parityFixture);
    assert.equal(parityCliStart.status, 0,
      parityCliStart.stdout + parityCliStart.stderr);
    parityCliSessionId = JSON.parse(parityCliStart.stdout.trim()).session_id;
    assert.ok(parityCliSessionId);
    const mcpParitySample = await waitForInitialSample(async () => {
      const result = payload(await server.call(nextParityStatusId++, {
        action: 'gremlin_status', dir: parityFixture, lane_id: 'mcp-parity',
        session_id: parityMcpSessionId, cursor: 0, max_records: 8, max_bytes: 8192
      }));
      assert.equal(result.isError, false, JSON.stringify(result.body));
      return result.body;
    }, 1729, 2, 'MCP random selection');
    const cliParitySample = await waitForInitialSample(() => {
      const result = runFo(['gremlin', 'status', '--dir', parityFixture,
        '--lane', 'cli-parity', '--session', parityCliSessionId,
        '--cursor', '0', '--max-records', '8', '--max-bytes', '8192', '--json'],
      parityFixture);
      assert.equal(result.status, 0, result.stdout + result.stderr);
      return JSON.parse(result.stdout.trim());
    }, 1729, 2, 'CLI random selection');
    assert.equal(mcpParitySample.length, 2,
      'MCP completed exactly the requested number of first-sample cases');
    assert.equal(cliParitySample.length, 2,
      'CLI completed exactly the requested number of first-sample cases');
    assert.ok(mcpParitySample.every(event => event.status === 'PASS'),
      'all MCP first-sample cases passed');
    assert.ok(cliParitySample.every(event => event.status === 'PASS'),
      'all CLI first-sample cases passed');
    const selectedCases = events => events.map(event => event.case_id).sort();
    assert.equal(new Set(selectedCases(mcpParitySample)).size, 2,
      'MCP first sample contains two distinct test cases');
    assert.equal(new Set(selectedCases(cliParitySample)).size, 2,
      'CLI first sample contains two distinct test cases');
    assert.deepEqual(selectedCases(mcpParitySample), selectedCases(cliParitySample),
      'MCP and CLI map the same valid random count and seed to the same cases');

    const parityStop = payload(await server.call(nextParityStatusId++, {
      action: 'gremlin_stop', dir: parityFixture, lane_id: 'mcp-parity',
      session_id: parityMcpSessionId
    }));
    assert.equal(parityStop.isError, false, JSON.stringify(parityStop.body));
    await waitForStopped(parityFixture, 'mcp-parity', parityMcpSessionId, 10000);
    const cliParityStop = runFo(['gremlin', 'stop', '--dir', parityFixture,
      '--lane', 'cli-parity', '--session', parityCliSessionId, '--json'], parityFixture);
    assert.equal(cliParityStop.status, 0, cliParityStop.stdout + cliParityStop.stderr);
    await waitForStopped(parityFixture, 'cli-parity', parityCliSessionId, 10000);
    console.log('mcp-gremlin: discoverable actions, prompt start, idle progress, '
      + 'shared CLI errors/status, events/failures/reproduce, wait reconnect and scoped stop passed');
  } catch (error) {
    primaryError = error;
  } finally {
    const cleanupErrors = [];
    if (fs.existsSync(path.join(markerRoot, 'mcp_blocked.started'))) {
      let active = true;
      if (sessionId) {
        const current = runFo(['gremlin', 'status', '--lane', 'mcp-probe',
          '--session', sessionId, '--dir', fixture, '--json'], fixture);
        if (current.status === 0) {
          try { active = JSON.parse(current.stdout.trim()).state !== 'stopped'; }
          catch (_) { /* the owner may be between state publications */ }
        }
      }
      if (active) {
        try { await releaseGate(blockedGate); } catch (_) { /* owner may be gone */ }
      }
    }
    const requestId = { value: 100 };
    for (const [cwd, lane, knownId, mcpOwned] of [
      [fixture, 'mcp-probe', sessionId, true],
      [parityFixture, 'mcp-parity', parityMcpSessionId, true],
      [parityFixture, 'cli-parity', parityCliSessionId, false]
    ]) {
      try { await cleanupOwner(server, cwd, lane, knownId, mcpOwned, requestId); }
      catch (error) { cleanupErrors.push(error); }
    }
    try {
      await server.rpc(99, 'shutdown');
      await waitForChildClose(server.child, 5000);
    } catch (_) {
      server.child.kill();
      await waitForChildClose(server.child, 5000);
    }
    if (cleanupErrors.length) {
      if (primaryError) cleanupErrors.unshift(primaryError);
      throw new AggregateError(cleanupErrors,
        primaryError ? 'Gremlin MCP fixture and cleanup failed'
          : 'Gremlin MCP fixture cleanup failed');
    }
  }
  if (primaryError) throw primaryError;
}

main().then(() => fs.rmSync(scratch, { recursive: true, force: true }))
  .catch(error => {
    console.error(error);
    console.error(`scratch preserved for diagnosis: ${scratch}`);
    process.exitCode = 1;
  });
