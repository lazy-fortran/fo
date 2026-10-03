#!/usr/bin/env node
// Keep an exact-base MCP process alive while a new CLI runs Gremlin.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const driver = process.argv[2] || process.env.FO;
const base = 'e4fc19309315ca003e7b2a704d3f8cc7dde15d32';
if (!driver) throw new Error('pass the newly built fo path as the first argument');
if (process.platform !== 'linux' || !fs.existsSync('/proc')) {
  console.log('mcp-gremlin-stale: skipped (requires Linux async process containment)');
  process.exit(0);
}
const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-stale-mcp-');
const home = path.join(scratch, 'home');
const env = { ...process.env, HOME: home, TMPDIR: '/var/tmp',
  FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1',
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_CACHE_DIR: path.join(scratch, 'cache'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg-cache'),
  FO_GREMLIN_STATE_DIR: path.join(scratch, 'gremlin-state') };
fs.mkdirSync(home, { recursive: true });

function makeStaleBinary() {
  const source = path.join(scratch, 'old-source');
  fs.mkdirSync(source);
  const archive = spawnSync('git', ['archive', base], { cwd: project,
    maxBuffer: 32 * 1024 * 1024 });
  assert.equal(archive.status, 0, archive.stderr.toString());
  const unpack = spawnSync('tar', ['-x', '-C', source], { input: archive.stdout,
    encoding: 'utf8' });
  assert.equal(unpack.status, 0, unpack.stderr);
  const built = spawnSync(driver, ['build'], { cwd: source, env, encoding: 'utf8',
    timeout: 180000, maxBuffer: 8 * 1024 * 1024 });
  assert.equal(built.status, 0, built.stdout + built.stderr);
  const buildRoot = path.join(source, 'build');
  const dirs = fs.existsSync(buildRoot) ? fs.readdirSync(buildRoot) : [];
  const binary = dirs.map(dir => path.join(buildRoot, dir, 'app', 'fo'))
    .find(file => fs.existsSync(file));
  assert.ok(binary, 'exact-base MCP binary was built in isolated scratch');
  return binary;
}

function startMcp(binary, cwd) {
  const child = spawn(binary, ['mcp-server'], { cwd, env,
    stdio: ['pipe', 'pipe', 'pipe'] });
  let buffer = '';
  let stderr = '';
  const pending = [];
  child.stdout.on('data', chunk => {
    buffer += chunk.toString();
    let newline;
    while (pending.length && (newline = buffer.indexOf('\n')) >= 0) {
      const body = buffer.slice(0, newline);
      buffer = buffer.slice(newline + 1);
      const waiter = pending.shift();
      clearTimeout(waiter.timer);
      try { waiter.resolve(JSON.parse(body)); } catch (error) { waiter.reject(error); }
    }
  });
  child.stderr.on('data', chunk => { stderr += chunk.toString(); });
  function rpc(id, method, params = {}) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id, method, params }) + '\n');
    });
  }
  function waitForExit(timeoutMs = 5000) {
    if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve();
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('stale MCP did not exit')), timeoutMs);
      child.once('exit', () => { clearTimeout(timer); resolve(); });
    });
  }
  return { child, rpc, waitForExit };
}

function cli(args, cwd) {
  return spawnSync(driver, args, { cwd, env, encoding: 'utf8', timeout: 30000,
    maxBuffer: 8 * 1024 * 1024 });
}

function writeFixture(dir, markerRoot) {
  fs.mkdirSync(path.join(dir, 'test'), { recursive: true });
  fs.mkdirSync(markerRoot, { recursive: true });
  fs.writeFileSync(path.join(dir, 'fpm.toml'), 'name = "gremlin_stale_probe"\n');
  fs.writeFileSync(path.join(dir, 'test/test_stale_pass.f90'), [
    'program test_stale_pass', 'implicit none', 'integer :: unit',
    `open(newunit=unit,file='${path.join(markerRoot, 'pass.done')}',status='replace')`,
    "write(unit,'(a)') 'pass'", 'close(unit)', 'end program test_stale_pass', ''
  ].join('\n'));
  fs.writeFileSync(path.join(dir, 'test/test_stale_blocked.f90'), [
    'program test_stale_blocked', 'implicit none', 'integer :: unit, status',
    'character(len=512) :: command',
    `command = "sh -c 'sleep 60 & echo $! > ${path.join(markerRoot, 'child.pid')}; wait'"`,
    'call execute_command_line(trim(command), exitstat=status)',
    'end program test_stale_blocked', ''
  ].join('\n'));
}

async function waitForFile(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`timed out waiting for ${file}`);
}

async function waitForPidMarker(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(file)) {
      const value = fs.readFileSync(file, 'utf8').trim();
      if (/^[1-9]\d*$/.test(value)) return Number(value);
    }
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`timed out waiting for process identity in ${file}`);
}

function processIdentity(pid) {
  try {
    const stat = fs.readFileSync(`/proc/${pid}/stat`, 'utf8');
    const fields = stat.slice(stat.lastIndexOf(')') + 2).trim().split(/\s+/);
    return { pid, state: fields[0], group: Number(fields[2]),
      session: Number(fields[3]), startTime: fields[19] };
  } catch (_) {
    return null;
  }
}

function pidAlive(identityOrPid) {
  const identity = typeof identityOrPid === 'object'
    ? identityOrPid : { pid: identityOrPid };
  const current = processIdentity(identity.pid);
  return !!current && (!identity.startTime || current.startTime === identity.startTime) &&
    !['Z', 'X'].includes(current.state);
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
    } catch (_) { /* process exited during the scan */ }
  }
  return owners;
}

async function waitForStopped(dir, lane, sessionId, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const result = cli(['gremlin', 'status', '--lane', lane, '--session', sessionId,
      '--dir', dir], dir);
    if (result.status === 0) {
      try {
        if (JSON.parse(result.stdout.trim()).state === 'stopped') return;
      } catch (_) { /* the owner may be between state publications */ }
    }
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error(`Gremlin owner ${sessionId} did not stop`);
}

async function waitForOwnerExit(dir, lane, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (gremlinOwnerPids(dir, lane).length === 0) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(`Gremlin owner process remained after stop: ${lane}`);
}

function ownerProcessGroup(pid) {
  const identity = processIdentity(pid);
  if (!identity) throw new Error(`process ${pid} disappeared during identity check`);
  return identity;
}

async function forceStopOwnedGremlinGroup(dir, lane) {
  const signalOwners = signal => {
    for (const pid of gremlinOwnerPids(dir, lane)) {
      const { group, session } = ownerProcessGroup(pid);
      assert.equal(group, pid, `refusing to signal non-owner process group ${pid}`);
      assert.equal(session, pid, `refusing to signal non-owner session ${pid}`);
      try { process.kill(-group, signal); }
      catch (error) { if (error.code !== 'ESRCH') throw error; }
    }
  };
  signalOwners('SIGTERM');
  try { await waitForOwnerExit(dir, lane, 1500); return; }
  catch (_) { /* escalate only within the verified owner group */ }
  signalOwners('SIGKILL');
  await waitForOwnerExit(dir, lane, 5000);
}

function trackedSleepGroup(identity) {
  const { pid } = identity;
  const args = fs.readFileSync(`/proc/${pid}/cmdline`).toString().split('\0')
    .filter(Boolean);
  assert.equal(path.basename(args[0] || ''), 'sleep',
    `refusing to signal unrecognized tracked child ${pid}`);
  assert.ok(args.includes('60'), `refusing to signal reused child PID ${pid}`);
  const current = ownerProcessGroup(pid);
  assert.equal(current.startTime, identity.startTime,
    `refusing to signal reused child PID ${pid}`);
  return current;
}

async function forceStopTrackedSleep(identity) {
  const { pid } = identity;
  const signalSleep = signal => {
    if (!pidAlive(identity)) return;
    const { group, session } = trackedSleepGroup(identity);
    const target = group > 1 && group === session ? -group : pid;
    try { process.kill(target, signal); }
    catch (error) { if (error.code !== 'ESRCH') throw error; }
  };
  signalSleep('SIGTERM');
  const deadline = Date.now() + 1500;
  while (pidAlive(identity) && Date.now() < deadline) {
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  if (pidAlive(identity)) {
    signalSleep('SIGKILL');
    const killDeadline = Date.now() + 5000;
    while (pidAlive(identity) && Date.now() < killDeadline) {
      await new Promise(resolve => setTimeout(resolve, 50));
    }
  }
  assert.equal(pidAlive(identity), false, `tracked sleeper ${pid} survived cleanup`);
}

async function main() {
  const staleBinary = makeStaleBinary();
  const fixture = path.join(scratch, 'project');
  const markerRoot = path.join(scratch, 'markers');
  writeFixture(fixture, markerRoot);
  const server = startMcp(staleBinary, fixture);
  const lane = 'stale-mcp-cli';
  let sessionId = '';
  let childPid = 0;
  let childIdentity = null;
  let ownerStopped = false;
  let primaryError;
  try {
    const init = await server.rpc(1, 'initialize', {
      protocolVersion: '2025-11-25', capabilities: {}
    });
    assert.equal(init.result.protocolVersion, '2025-11-25');
    const listed = await server.rpc(2, 'tools/list');
    const actions = listed.result.tools[0].inputSchema.properties.action.enum;
    assert.equal(actions.some(action => action.startsWith('gremlin_')), false,
      'the live exact-base MCP server has no Gremlin action');

    const startedAt = Date.now();
    const started = cli(['gremlin', 'start', '--dir', fixture, '--lane', lane,
      '--target', 'test_stale_pass', '--target', 'test_stale_blocked',
      '--timeout-seconds', '5'], fixture);
    assert.equal(started.status, 0, started.stdout + started.stderr);
    assert.ok(Date.now() - startedAt < 3000, 'new CLI start returns promptly');
    const startedJson = JSON.parse(started.stdout.trim());
    assert.equal(startedJson.state, 'running');
    sessionId = startedJson.session_id;
    assert.ok(sessionId, 'new CLI returns a stable session id');

    await waitForFile(path.join(markerRoot, 'pass.done'), 30000);
    childPid = await waitForPidMarker(path.join(markerRoot, 'child.pid'), 30000);
    assert.equal(server.child.exitCode, null,
      'stale MCP stays alive while the new CLI operates');
    childIdentity = processIdentity(childPid);
    assert.ok(childPid > 0 && childIdentity && pidAlive(childIdentity),
      'blocked test child is running with its recorded process identity');

    const status = cli(['gremlin', 'status', '--dir', fixture, '--lane', lane,
      '--session', sessionId, '--cursor', '0', '--max-records', '8',
      '--max-bytes', '8192'], fixture);
    assert.equal(status.status, 0, status.stdout + status.stderr);
    const snapshot = JSON.parse(status.stdout.trim());
    assert.ok(snapshot.events.some(event => event.case_id === 'test_stale_pass' &&
      event.status === 'PASS'), 'new CLI reports the completed pass');
    assert.ok(!snapshot.events.some(event => event.case_id === 'test_stale_blocked'),
      'new CLI leaves the in-flight case unknown');

    const stopped = cli(['gremlin', 'stop', '--dir', fixture, '--lane', lane,
      '--session', sessionId], fixture);
    assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
    await waitForStopped(fixture, lane, sessionId, 10000);
    await waitForOwnerExit(fixture, lane, 5000);
    ownerStopped = true;
    const childDeadline = Date.now() + 5000;
    while (pidAlive(childIdentity) && Date.now() < childDeadline) {
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    assert.equal(pidAlive(childIdentity), false, 'stop kills the owned test process tree');
    assert.equal(server.child.exitCode, null,
      'the stale MCP process remains alive through CLI stop');
    console.log('mcp-gremlin-stale: exact-base MCP remained live while new CLI '
      + 'started, reported a pass, preserved an in-flight case, and stopped its tree');
  } catch (error) {
    primaryError = error;
  } finally {
    const cleanupErrors = [];
    if (!sessionId) {
      try {
        const current = cli(['gremlin', 'status', '--dir', fixture, '--lane', lane,
          '--json'], fixture);
        if (current.status === 0) {
          sessionId = JSON.parse(current.stdout.trim()).session_id;
        }
      } catch (error) { cleanupErrors.push(error); }
    }
    if (sessionId && !ownerStopped) {
      try {
        const status = cli(['gremlin', 'status', '--dir', fixture, '--lane', lane,
          '--session', sessionId, '--json'], fixture);
        let stopped = false;
        if (status.status === 0) {
          try { stopped = JSON.parse(status.stdout.trim()).state === 'stopped'; }
          catch (_) { /* state may be between publications */ }
        }
        if (!stopped) {
          const stop = cli(['gremlin', 'stop', '--dir', fixture, '--lane', lane,
            '--session', sessionId, '--json'], fixture);
          if (stop.status !== 0) {
            const laneStop = cli(['gremlin', 'stop', '--dir', fixture,
              '--lane', lane, '--json'], fixture);
            assert.equal(laneStop.status, 0, laneStop.stdout + laneStop.stderr);
          }
          await waitForStopped(fixture, lane, sessionId, 10000);
        }
        await waitForOwnerExit(fixture, lane, 5000);
      } catch (error) { cleanupErrors.push(error); }
    }
    if (!ownerStopped) {
      try {
        if (gremlinOwnerPids(fixture, lane).length > 0) {
          await forceStopOwnedGremlinGroup(fixture, lane);
          cleanupErrors.push(new Error('API stop failed; exact stale owner group was terminated'));
        } else await waitForOwnerExit(fixture, lane, 1000);
        ownerStopped = true;
      } catch (error) { cleanupErrors.push(error); }
    }
    if (childIdentity) {
      try {
        const childDeadline = Date.now() + 5000;
        while (pidAlive(childIdentity) && Date.now() < childDeadline) {
          await new Promise(resolve => setTimeout(resolve, 50));
        }
        if (pidAlive(childIdentity)) await forceStopTrackedSleep(childIdentity);
        assert.equal(pidAlive(childIdentity), false,
          `owned sleeper ${childPid} survived cleanup`);
      } catch (error) { cleanupErrors.push(error); }
    }
    try {
      await server.rpc(3, 'shutdown');
      await server.waitForExit();
    } catch (error) {
      server.child.kill('SIGTERM');
      try { await server.waitForExit(); }
      catch (waitError) { cleanupErrors.push(waitError); }
    }
    if (cleanupErrors.length) {
      if (primaryError) cleanupErrors.unshift(primaryError);
      throw new AggregateError(cleanupErrors,
        primaryError ? 'stale-MCP fixture and cleanup failed'
          : 'stale-MCP fixture cleanup failed');
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
