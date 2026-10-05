#!/usr/bin/env node
// Black-box cancellation test for the async process session used by MCP.
// Run: TMPDIR=/var/tmp node test/test_async_process_lifecycle.js [/path/to/fo]

'use strict';

const { spawn, spawnSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const FO = process.argv[2] || path.join(os.homedir(), '.local', 'bin', 'fo');
let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    passed++;
    process.stdout.write('  ok: ' + message + '\n');
  } else {
    failed++;
    process.stdout.write('  FAIL: ' + message + '\n');
  }
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function countLines(file) {
  try { return fs.readFileSync(file, 'utf8').trim().split('\n').filter(Boolean).length; }
  catch (_) { return 0; }
}

function fixturePids(file) {
  try {
    return fs.readFileSync(file, 'utf8').split('\n').flatMap(line => {
      const match = line.match(/^(?:parent|grandchild|escape-attempt|escaped|compat-attempt|compat-(?:i386|x32)):(\d+):/);
      return match ? [Number(match[1])] : [];
    });
  } catch (_) { return []; }
}

const compatCode = {
  i386: { setsid: 'b842000000cd80c3', setpgid: '53b83900000031db31c9cd805bc3' },
  x32: { setsid: '48b870000040000000000f05c3',
    setpgid: '48b86d0000400000000031ff31f60f05c3' }
};

function probeCompatAbi(abi, syscall) {
  if (process.arch !== 'x64') {
    return { supported: false, reason: 'test process is not x86-64' };
  }
  const script = [
    'import ctypes, mmap, os, sys',
    'code = bytes.fromhex(sys.argv[1])',
    'memory = mmap.mmap(-1, len(code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)',
    'memory.write(code)',
    'address = ctypes.addressof(ctypes.c_char.from_buffer(memory))',
    'call = ctypes.CFUNCTYPE(ctypes.c_int32)(address)',
    'value = call()',
    'expected = os.getpid() if sys.argv[2] == "setsid" else 0',
    'actual_group = os.getsid(0) if sys.argv[2] == "setsid" else os.getpgid(0)',
    'if value != expected or actual_group != os.getpid():',
    '    raise SystemExit("syscall returned %d; group is %d; expected %d" % (value, actual_group, expected))'
  ].join('\n');
  const result = spawnSync('python3', ['-c', script, compatCode[abi][syscall], syscall], {
    encoding: 'utf8', timeout: 5000
  });
  if (result.error) return { supported: false, reason: result.error.message };
  if (result.status !== 0) {
    return { supported: false, reason: result.signal ||
      (result.stderr || 'compat syscall unavailable').trim() };
  }
  return { supported: true };
}

async function runCompatScenario(root, abi, syscall, servers, cleanupPids) {
  const probe = probeCompatAbi(abi, syscall);
  if (!probe.supported) {
    process.stdout.write('  skip: ' + abi + ' ' + syscall +
      ' unavailable in unfiltered child (' + probe.reason + ')\n');
    return;
  }
  process.stdout.write('  probe: ' + abi + ' ' + syscall + ' succeeded unfiltered\n');
  const file = path.join(root, 'compat-' + abi + '-' + syscall + '.log');
  const server = startServer(root, {
    PATH: root + path.delimiter + process.env.PATH,
    FO_LIFECYCLE_HEARTBEAT: file,
    FO_LIFECYCLE_COMPAT_ABI: abi,
    FO_LIFECYCLE_COMPAT_SYSCALL: syscall,
    FO_LIFECYCLE_COMPAT_CODE: compatCode[abi][syscall]
  });
  servers.push(server);
  await initialize(server);
  const runId = await startRun(server, root);
  const label = 'compat-' + abi + ':';
  await waitFor(() => fs.existsSync(file) &&
    fs.readFileSync(file, 'utf8').includes(label) &&
    fs.readFileSync(file, 'utf8').includes('compat-result:' + syscall + ':') &&
    new Set(fixturePids(file)).size >= 2, 5000, abi + ' ' + syscall + ' heartbeat');
  assert(fs.readFileSync(file, 'utf8').includes('compat-result:' + syscall + ':-1'),
    abi + ' ' + syscall + ' is denied with EPERM in the filtered child');
  const pids = fixturePids(file);
  cleanupPids.push(...fs.readFileSync(file, 'utf8').split('\n').flatMap(line => {
    const match = line.match(/^compat-(?:i386|x32):(\d+):/);
    return match ? [Number(match[1])] : [];
  }));
  const result = await cancel(server, runId);
  const stoppedAtReturn = countLines(file);
  assert(result.value && result.value.cancelled === true,
    abi + ' cancellation succeeds after the compat ' + syscall + ' attempt');
  assert(allProcessesStopped(pids),
    abi + ' ' + syscall + ' descendant is gone before cancellation returns');
  await delay(300);
  assert(countLines(file) === stoppedAtReturn,
    abi + ' ' + syscall + ' heartbeat does not survive cancellation');
}

function processIsRunning(pid) {
  try { process.kill(pid, 0); return true; }
  catch (error) { return error.code !== 'ESRCH'; }
}

function allProcessesStopped(pids) {
  return pids.length >= 2 && pids.every(pid => !processIsRunning(pid));
}

function waitFor(predicate, timeoutMs, label) {
  const end = Date.now() + timeoutMs;
  return (async () => {
    while (Date.now() < end) {
      if (await predicate()) return true;
      await delay(40);
    }
    throw new Error('timed out waiting for ' + label);
  })();
}

function startServer(root, env) {
  const proc = spawn(FO, ['mcp-server'], {
    cwd: root,
    env: Object.assign({}, process.env, env),
    stdio: ['pipe', 'pipe', 'pipe']
  });
  let stdout = Buffer.alloc(0);
  let stderr = '';
  const pending = new Map();
  proc.stdout.on('data', chunk => {
    stdout = Buffer.concat([stdout, chunk]);
    for (;;) {
      const split = stdout.indexOf('\r\n\r\n');
      if (split < 0) break;
      const headers = stdout.slice(0, split).toString();
      const match = headers.match(/Content-Length:\s*(\d+)/i);
      if (!match) throw new Error('bad MCP response header: ' + headers);
      const size = Number(match[1]);
      const start = split + 4;
      if (stdout.length < start + size) break;
      const body = JSON.parse(stdout.slice(start, start + size).toString());
      stdout = stdout.slice(start + size);
      const resolve = pending.get(body.id);
      if (resolve) {
        pending.delete(body.id);
        resolve(body);
      }
    }
  });
  proc.stderr.on('data', chunk => { stderr += chunk.toString(); });
  let sequence = 0;
  function request(method, params) {
    const id = ++sequence;
    const body = JSON.stringify({ jsonrpc: '2.0', id, method, params });
    const frame = 'Content-Length: ' + Buffer.byteLength(body) + '\r\n\r\n' + body;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        pending.delete(id);
        reject(new Error('MCP request timed out: ' + method + '\n' + stderr));
      }, 15000);
      pending.set(id, response => { clearTimeout(timer); resolve(response); });
      proc.stdin.write(frame);
    });
  }
  async function call(args) {
    const response = await request('tools/call', { name: 'fo', arguments: args });
    if (response.error) return response;
    if (response.result && response.result.run_id !== undefined) {
      return { response, value: response.result };
    }
    if (!response.result || !response.result.content || !response.result.content[0]) {
      return { response, value: null };
    }
    const text = response.result.content[0].text;
    let value = text;
    try { value = JSON.parse(text); } catch (_) { /* plain status text */ }
    return { response, value };
  }
  return { proc, request, call, getStderr: () => stderr };
}

function makeFixture(root) {
  const fakeFo = path.join(root, 'fo');
  fs.writeFileSync(fakeFo, `#!/usr/bin/env node
const fs = require('fs');
const { spawn } = require('child_process');
const file = process.env.FO_LIFECYCLE_HEARTBEAT;
const mode = process.env.FO_LIFECYCLE_MODE || 'run';
if (mode === 'early') {
  fs.appendFileSync(file, 'early\\n');
  process.stdout.write('completed-result\\n');
  process.exit(0);
}
function beat(label) { fs.appendFileSync(file, label + ':' + process.pid + ':' + Date.now() + '\\n'); }
if (process.env.FO_LIFECYCLE_IGNORE === '1') process.on('SIGTERM', () => {});
beat('parent');
const child = spawn(process.execPath, ['-e',
  "const fs=require('fs');const f=process.env.FO_LIFECYCLE_HEARTBEAT;" +
  "const beat=()=>fs.appendFileSync(f,'grandchild:'+process.pid+':'+Date.now()+'\\\\n');" +
  "if(process.env.FO_LIFECYCLE_IGNORE==='1')process.on('SIGTERM',()=>{});" +
  "beat();setInterval(beat,60);"], { stdio: 'ignore' });
if (process.env.FO_LIFECYCLE_ESCAPE === '1') {
  const escaped = "const fs=require('fs');const f=process.env.FO_LIFECYCLE_HEARTBEAT;" +
    "const beat=()=>fs.appendFileSync(f,'escaped:'+process.pid+':'+Date.now()+'\\\\n');" +
    "beat();setInterval(beat,60);";
  const attempt = spawn('setsid', [process.execPath, '-e', escaped], {
    stdio: ['ignore', 'ignore', 'pipe'],
    env: Object.assign({}, process.env, { LC_ALL: 'C' })
  });
  let setsidError = '';
  attempt.stderr.on('data', chunk => { setsidError += chunk.toString(); });
  fs.appendFileSync(file, 'escape-attempt:' + attempt.pid + ':' + Date.now() + '\\n');
  attempt.on('close', code => {
    if (code !== 0 && setsidError.includes('Operation not permitted')) {
      fs.appendFileSync(file, 'setsid-blocked:EPERM\\n');
    }
  });
}
if (process.env.FO_LIFECYCLE_COMPAT_ABI) {
  const abi = process.env.FO_LIFECYCLE_COMPAT_ABI;
  const syscall = process.env.FO_LIFECYCLE_COMPAT_SYSCALL;
  const program = [
    'import ctypes, mmap, os, sys, time',
    'code = bytes.fromhex(sys.argv[1])',
    'memory = mmap.mmap(-1, len(code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)',
    'memory.write(code)',
    'address = ctypes.addressof(ctypes.c_char.from_buffer(memory))',
    'value = ctypes.CFUNCTYPE(ctypes.c_int32)(address)()',
    'path = os.environ["FO_LIFECYCLE_HEARTBEAT"]',
    'with open(path, "a") as output: output.write("compat-result:" + sys.argv[2] + ":" + str(value) + "\\\\n")',
    'def beat():',
    '    with open(path, "a") as output: output.write("compat-' + abi +
      ':" + str(os.getpid()) + ":" + str(time.time_ns()) + "\\\\n")',
    'beat()',
    'while True:',
    '    time.sleep(0.06)',
    '    beat()'
  ].join('\\n');
  const attempt = spawn('python3', ['-c', program,
    process.env.FO_LIFECYCLE_COMPAT_CODE, syscall], { stdio: 'ignore' });
  fs.appendFileSync(file, 'compat-attempt:' + attempt.pid + ':' + Date.now() + '\\n');
}
setInterval(() => beat('parent'), 60);
`);
  fs.chmodSync(fakeFo, 0o755);
}

async function initialize(server) {
  const result = await server.request('initialize', {
    protocolVersion: '2025-11-25', capabilities: {},
    clientInfo: { name: 'lifecycle-test', version: '1' }
  });
  if (!result.result) throw new Error('MCP initialization failed');
}

async function startRun(server, root) {
  const result = await server.call({ action: 'check', mode: 'start', root });
  if (!result.value || !result.value.run_id) {
    throw new Error('async start failed: ' + JSON.stringify(result));
  }
  return result.value.run_id;
}

async function cancel(server, runId) {
  return server.call({ action: 'cancel', run_id: runId });
}

async function stopServer(server) {
  if (server.proc.exitCode !== null) return;
  try { await server.request('shutdown', {}); } catch (_) { server.proc.kill('SIGKILL'); }
  await Promise.race([
    new Promise(resolve => server.proc.once('exit', resolve)),
    delay(3000).then(() => server.proc.kill('SIGKILL'))
  ]);
}

async function main() {
  if (!fs.existsSync(FO)) throw new Error('fo executable not found: ' + FO);
  const root = fs.mkdtempSync(path.join(process.env.TMPDIR || os.tmpdir(),
    'fo-async-lifecycle-'));
  const unrelated = path.join(root, 'sentinel.log');
  let sentinel;
  let escapedPidsToClean = [];
  const servers = [];
  try {
    makeFixture(root);
    fs.writeFileSync(path.join(root, 'fpm.toml'), 'name = "lifecycle_fixture"\n');
    sentinel = spawn(process.execPath, ['-e',
      "const fs=require('fs');const f=process.argv[1];" +
      "setInterval(()=>fs.appendFileSync(f,Date.now()+'\\n'),60);", unrelated],
    { stdio: 'ignore' });

    process.stdout.write('\n--- graceful tree cancellation and stale run ---\n');
    const gracefulFile = path.join(root, 'graceful.log');
    const first = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: gracefulFile
    });
    servers.push(first);
    await initialize(first);
    const firstRun = await startRun(first, root);
    await waitFor(() => new Set(fixturePids(gracefulFile)).size >= 2, 5000,
      'parent and grandchild heartbeats');
    const firstPids = fixturePids(gracefulFile);
    const before = Date.now();
    const canceled = await cancel(first, firstRun);
    const duration = Date.now() - before;
    const stoppedAt = countLines(gracefulFile);
    await delay(300);
    assert(canceled.value && canceled.value.cancelled === true, 'cancel reports owned run cancelled');
    assert(duration < 3000, 'graceful cancellation returns within its bound');
    assert(countLines(gracefulFile) === stoppedAt, 'parent and grandchild heartbeats stop');
    assert(allProcessesStopped(firstPids), 'cancel reaps the owned parent and grandchild');
    const stale = await cancel(first, firstRun);
    assert(stale.error || (stale.response && stale.response.result &&
      stale.response.result.isError), 'repeated stale run ID returns an error');
    const nextRun = await startRun(first, root);
    await waitFor(() => countLines(gracefulFile) >= stoppedAt + 2, 5000,
      'a new run after cancellation');
    const beforeStale = countLines(gracefulFile);
    const staleDuringRun = await cancel(first, firstRun);
    await delay(200);
    assert(staleDuringRun.error || (staleDuringRun.response &&
      staleDuringRun.response.result && staleDuringRun.response.result.isError),
    'an old run ID cannot cancel the current run');
    assert(countLines(gracefulFile) > beforeStale,
      'the current run remains active after a stale cancellation');
    await cancel(first, nextRun);
    assert(allProcessesStopped(fixturePids(gracefulFile)),
      'the current run also reaps its parent and grandchild');
    const duplicate = await cancel(first, nextRun);
    assert(duplicate.error || (duplicate.response && duplicate.response.result &&
      duplicate.response.result.isError), 'duplicate cancellation reports a stale run');
    assert(countLines(unrelated) >= 3, 'unrelated sentinel survives cancellation');

    process.stdout.write('\n--- descendant session escape attempt ---\n');
    const escapeFile = path.join(root, 'escape.log');
    const escaping = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: escapeFile,
      FO_LIFECYCLE_ESCAPE: '1'
    });
    servers.push(escaping);
    await initialize(escaping);
    const escapeRun = await startRun(escaping, root);
    await waitFor(() => {
      const text = fs.existsSync(escapeFile) ? fs.readFileSync(escapeFile, 'utf8') : '';
      return /escaped:/.test(text) &&
        new Set(fixturePids(escapeFile)).size >= 2;
    }, 5000, 'setsid descendant starts in an independent session');
    const escapePids = fixturePids(escapeFile);
    escapedPidsToClean = fs.readFileSync(escapeFile, 'utf8').split('\n').flatMap(line => {
      const match = line.match(/^escaped:(\d+):/);
      return match ? [Number(match[1])] : [];
    });
    assert(escapedPidsToClean.length > 0,
      'the native setsid escape actually runs before cancellation');
    const escapeCancel = await cancel(escaping, escapeRun);
    const stoppedAtReturn = countLines(escapeFile);
    assert(escapeCancel.value && escapeCancel.value.cancelled === true,
      'owned cancellation succeeds after the setsid attempt');
    assert(allProcessesStopped(escapePids),
      'the parent and setsid descendant are gone before cancellation returns');
    await delay(300);
    assert(countLines(escapeFile) === stoppedAtReturn,
      'a descendant heartbeat cannot continue after cancellation succeeds');

    process.stdout.write('\n--- compat ABI session escape attempts ---\n');
    for (const abi of ['i386', 'x32']) {
      for (const syscall of ['setsid', 'setpgid']) {
        await runCompatScenario(root, abi, syscall, servers, escapedPidsToClean);
      }
    }

    process.stdout.write('\n--- TERM-ignoring tree and completed output ---\n');
    const ignoreFile = path.join(root, 'ignore.log');
    const stubborn = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: ignoreFile,
      FO_LIFECYCLE_IGNORE: '1'
    });
    servers.push(stubborn);
    await initialize(stubborn);
    const stubbornRun = await startRun(stubborn, root);
    await waitFor(() => new Set(fixturePids(ignoreFile)).size >= 2, 5000,
      'TERM-ignoring descendant heartbeats');
    const stubbornPids = fixturePids(ignoreFile);
    const killStart = Date.now();
    await cancel(stubborn, stubbornRun);
    const killDuration = Date.now() - killStart;
    const ignoreStoppedAt = countLines(ignoreFile);
    await delay(300);
    assert(killDuration >= 1500 && killDuration < 3500,
      'TERM-ignoring group receives bounded grace then KILL');
    assert(countLines(ignoreFile) === ignoreStoppedAt, 'KILL stops TERM-ignoring parent and grandchild');
    assert(allProcessesStopped(stubbornPids), 'KILL reaps the TERM-ignoring parent and grandchild');

    const earlyFile = path.join(root, 'early.log');
    const early = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: earlyFile,
      FO_LIFECYCLE_MODE: 'early'
    });
    servers.push(early);
    await initialize(early);
    const earlyRun = await startRun(early, root);
    let status;
    await waitFor(async () => {
      status = await early.call({ action: 'status' });
      return status.value && status.value.state === 'finished';
    }, 5000, 'early child exit');
    const diagnostics = await early.call({ action: 'diagnostics', run_id: earlyRun });
    assert(status.value.exitcode === 0, 'early exit publishes its successful status');
    assert(String(diagnostics.value).includes('completed-result'),
      'completed output remains available after process reaping');

    process.stdout.write('\n--- concurrent independent jobs ---\n');
    const laneAFile = path.join(root, 'lane-a.log');
    const laneBFile = path.join(root, 'lane-b.log');
    const laneA = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: laneAFile
    });
    const laneB = startServer(root, {
      PATH: root + path.delimiter + process.env.PATH,
      FO_LIFECYCLE_HEARTBEAT: laneBFile
    });
    servers.push(laneA, laneB);
    await Promise.all([initialize(laneA), initialize(laneB)]);
    const [runA, runB] = await Promise.all([startRun(laneA, root), startRun(laneB, root)]);
    await waitFor(() => new Set(fixturePids(laneAFile)).size >= 2 &&
      new Set(fixturePids(laneBFile)).size >= 2,
      5000, 'both concurrent lane heartbeats');
    const pidsA = fixturePids(laneAFile);
    const pidsB = fixturePids(laneBFile);
    const laneBStartedAt = countLines(laneBFile);
    await cancel(laneA, runA);
    assert(allProcessesStopped(pidsA), 'cancelling lane A reaps only lane A processes');
    const laneBStoppedAt = countLines(laneBFile);
    await delay(250);
    assert(laneBStoppedAt >= laneBStartedAt && countLines(laneBFile) > laneBStoppedAt,
      'cancelling one concurrent job leaves the other job running');
    await cancel(laneB, runB);
    assert(allProcessesStopped(pidsB), 'lane B is reaped when its own cancellation arrives');
    assert(countLines(unrelated) >= 6, 'sentinel remains alive through concurrent cancellation');
  } finally {
    if (sentinel) sentinel.kill('SIGKILL');
    for (const pid of escapedPidsToClean) {
      if (processIsRunning(pid)) {
        try { process.kill(pid, 'SIGKILL'); } catch (_) { /* already gone */ }
      }
    }
    await Promise.all(servers.map(stopServer));
    fs.rmSync(root, { recursive: true, force: true });
  }
  process.stdout.write('\n' + passed + ' passed, ' + failed + ' failed\n');
  if (failed) process.exitCode = 1;
}

main().catch(error => {
  process.stderr.write(error.stack + '\n');
  process.exitCode = 1;
});
