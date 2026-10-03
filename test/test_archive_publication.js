#!/usr/bin/env node
// Exercise atomic archive, shared-library, and executable publication through fo.
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const { execFileSync, spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const driver = process.argv[2] ? path.resolve(process.argv[2]) :
  process.env.FO || 'fo';
const realAr = execFileSync('which', ['ar'], { encoding: 'utf8' }).trim();
const realFc = execFileSync('which', ['gfortran'], { encoding: 'utf8' }).trim();
const realGcc = execFileSync('which', ['gcc'], { encoding: 'utf8' }).trim();
const truncatedMagic = process.platform === 'darwin'
  ? Buffer.from([0xcf, 0xfa, 0xed, 0xfe])
  : Buffer.from([0x7f, 0x45, 0x4c, 0x46]);
const scratch = fs.mkdtempSync('/var/tmp/fo-archive-publication-');
const project = path.join(scratch, 'project');
const localCache = path.join(scratch, 'cache');
const fakeBin = path.join(scratch, 'bin');
const arLog = path.join(scratch, 'ar.jsonl');
const fcLog = path.join(scratch, 'fc.jsonl');
const exeLog = path.join(scratch, 'exe.jsonl');
const sharedLog = path.join(scratch, 'shared.jsonl');
const fakeAr = path.join(fakeBin, 'ar');
const fakeFc = path.join(fakeBin, 'gfortran');
const fakeGcc = path.join(fakeBin, 'gcc');
const activeFoChildren = new Map();
const baseEnv = {
  ...process.env,
  PATH: `${fakeBin}:${process.env.PATH}`,
  FO_CACHE_DIR: localCache,
  FO_JOBS: '2',
  FO_TEST_AR_LOG: arLog,
  FO_TEST_REAL_AR: realAr,
  FO_TEST_FC_LOG: fcLog,
  FO_TEST_EXE_LOG: exeLog,
  FO_TEST_REAL_FC: realFc,
  FO_TEST_SHARED_LOG: sharedLog,
  FO_TEST_REAL_GCC: realGcc,
  FO_FC: fakeFc,
};

function write(file, contents) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, contents);
}

function command(args, mode = 'normal', linkMode = 'normal', envOverrides = {}) {
  const result = spawnSync(driver, args, {
    cwd: project,
    encoding: 'utf8',
    maxBuffer: 8 * 1024 * 1024,
    env: { ...baseEnv, FO_FAKE_AR_MODE: mode, FO_FAKE_SHARED_MODE: mode,
      FO_FAKE_TEST_LINK_MODE: linkMode, ...envOverrides },
  });
  if (result.error) throw result.error;
  return result;
}

function checked(args, mode = 'normal') {
  const result = command(args, mode);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

function jsonReport(output) {
  for (const line of output.split('\n')) {
    const start = line.indexOf('{');
    if (start < 0) continue;
    try {
      return JSON.parse(line.slice(start));
    } catch {}
  }
  assert.fail('fo did not emit a JSON test report');
}

function start(args, mode, linkMode = 'normal') {
  const child = spawn(driver, args, {
    cwd: project,
    env: { ...baseEnv, FO_FAKE_AR_MODE: mode, FO_FAKE_SHARED_MODE: mode,
      FO_FAKE_TEST_LINK_MODE: linkMode },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  let output = '';
  child.stdout.on('data', chunk => { output += chunk; });
  child.stderr.on('data', chunk => { output += chunk; });
  const done = new Promise(resolve => child.on('close', (status, signal) =>
    resolve({ status, signal, output })));
  activeFoChildren.set(child, done);
  child.once('close', () => activeFoChildren.delete(child));
  return { child, done };
}

function fixtureToolPids() {
  const pids = new Set();
  for (const log of [arLog, fcLog, exeLog, sharedLog]) {
    if (!fs.existsSync(log)) continue;
    for (const line of fs.readFileSync(log, 'utf8').split('\n')) {
      if (!line) continue;
      try {
        const entry = JSON.parse(line);
        if (Number.isInteger(entry.pid) && entry.pid > 0) pids.add(entry.pid);
      } catch {}
    }
  }
  return [...pids];
}

function pidAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error.code !== 'ESRCH';
  }
}

function fixtureToolAlive(pid) {
  if (!pidAlive(pid)) return false;
  try {
    const command = process.platform === 'linux'
      ? fs.readFileSync(`/proc/${pid}/cmdline`)
      : execFileSync('ps', ['-ww', '-p', String(pid), '-o', 'command=']);
    return command.includes(fakeBin);
  } catch {
    return false;
  }
}

function fixtureStagePaths() {
  const stages = [];
  for (const dir of [path.join(project, 'build/fo/lib'),
    path.join(project, 'build/fo/bin')]) {
    if (!fs.existsSync(dir)) continue;
    for (const name of fs.readdirSync(dir)) {
      if (name.startsWith('.fo-archive.tmp-') ||
          name.startsWith('.fo-shared.tmp-') ||
          name.startsWith('.fo-link.tmp-')) {
        stages.push(path.join(dir, name));
      }
    }
  }
  return stages;
}

function cleanupFixtureStages() {
  for (const stage of fixtureStagePaths()) {
    fs.rmSync(stage, { recursive: true, force: true });
  }
  assert.deepEqual(fixtureStagePaths(), [],
    'fixture cleanup removes all remaining owned staging paths');
}

async function pause(milliseconds) {
  await new Promise(resolve => setTimeout(resolve, milliseconds));
}

async function cleanupFixtureProcesses() {
  const owners = [...activeFoChildren.entries()];
  for (const [child] of owners) {
    if (child.exitCode === null && child.signalCode === null) child.kill('SIGTERM');
  }
  await Promise.race([
    Promise.all(owners.map(([, done]) => done)), pause(1000),
  ]);
  for (const [child] of owners) {
    if (child.exitCode === null && child.signalCode === null) child.kill('SIGKILL');
  }
  await Promise.race([
    Promise.all(owners.map(([, done]) => done)), pause(1000),
  ]);

  const tools = fixtureToolPids();
  for (const pid of tools) {
    if (fixtureToolAlive(pid)) {
      try { process.kill(pid, 'SIGTERM'); } catch {}
    }
  }
  await pause(100);
  for (const pid of tools) {
    if (fixtureToolAlive(pid)) {
      try { process.kill(pid, 'SIGKILL'); } catch {}
    }
  }
  for (let attempt = 0; attempt < 100; attempt++) {
    if (tools.every(pid => !fixtureToolAlive(pid))) {
      cleanupFixtureStages();
      return;
    }
    await pause(10);
  }
  try {
    assert.fail('fixture child processes remained alive after cleanup');
  } finally {
    cleanupFixtureStages();
  }
}

function archivePaths() {
  const libDir = path.join(project, 'build/fo/lib');
  if (!fs.existsSync(libDir)) return [];
  return fs.readdirSync(libDir)
    .filter(name => /^objects_[0-9a-f]+\.a$/.test(name))
    .map(name => path.join(libDir, name));
}

function sharedPaths() {
  const libDir = path.join(project, 'build/fo/lib');
  if (!fs.existsSync(libDir)) return [];
  return fs.readdirSync(libDir)
    .filter(name => /^libproject_[0-9a-f]+\.(so|dylib)$/.test(name))
    .map(name => path.join(libDir, name));
}

function stableTestBinaries() {
  const binDir = path.join(project, 'build/fo/bin');
  if (!fs.existsSync(binDir)) return [];
  return fs.readdirSync(binDir)
    .filter(name => name.startsWith('test_'))
    .map(name => path.join(binDir, name))
    .filter(file => fs.statSync(file).isFile())
    .sort();
}

function fileDigest(file) {
  return crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
}

function arCalls() {
  if (!fs.existsSync(arLog)) return [];
  return fs.readFileSync(arLog, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line)).filter(entry => entry.args[0] === 'rcs');
}

function sharedCalls() {
  if (!fs.existsSync(sharedLog)) return [];
  return fs.readFileSync(sharedLog, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line)).filter(entry => entry.kind === 'start');
}

function delayedExecutableCalls() {
  if (!fs.existsSync(exeLog)) return [];
  return fs.readFileSync(exeLog, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line));
}

function assertConcurrentTestLinks() {
  const events = fs.readFileSync(fcLog, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line));
  let active = 0;
  let maximum = 0;
  for (const event of events) {
    active += event.kind === 'start' ? 1 : -1;
    maximum = Math.max(maximum, active);
  }
  assert(maximum >= 2, 'two test link processes overlap as archive consumers');
}

function assertVisibleArchivesComplete() {
  for (const archive of archivePaths()) {
    const result = spawnSync(realAr, ['t', archive], { encoding: 'utf8' });
    assert.equal(result.status, 0, result.stderr || 'visible archive is incomplete');
    assert.equal(result.stdout.trim().split('\n').length, 3,
      'visible archive contains all defining objects');
  }
}

function validSharedMagic(file) {
  const magic = fs.readFileSync(file).subarray(0, 4);
  if (process.platform !== 'darwin') return magic.equals(Buffer.from([0x7f, 0x45, 0x4c, 0x46]));
  return [
    [0xce, 0xfa, 0xed, 0xfe], [0xcf, 0xfa, 0xed, 0xfe],
    [0xfe, 0xed, 0xfa, 0xce], [0xfe, 0xed, 0xfa, 0xcf],
    [0xca, 0xfe, 0xba, 0xbe], [0xbe, 0xba, 0xfe, 0xca],
    [0xca, 0xfe, 0xba, 0xbf], [0xbf, 0xba, 0xfe, 0xca],
  ].some(bytes => magic.equals(Buffer.from(bytes)));
}

function assertVisibleSharedLibrariesComplete() {
  for (const library of sharedPaths()) {
    assert(validSharedMagic(library), `visible shared library is incomplete: ${library}`);
  }
}

async function monitorArchives(children) {
  while (children.some(child => child.exitCode === null && child.signalCode === null)) {
    assertVisibleArchivesComplete();
    await new Promise(resolve => setTimeout(resolve, 10));
  }
  assertVisibleArchivesComplete();
}

async function monitorSharedLibraries(children) {
  while (children.some(child => child.exitCode === null && child.signalCode === null)) {
    assertVisibleSharedLibrariesComplete();
    await new Promise(resolve => setTimeout(resolve, 10));
  }
  assertVisibleSharedLibrariesComplete();
}

async function waitFor(predicate) {
  for (let attempt = 0; attempt < 200; attempt++) {
    if (predicate()) return;
    await new Promise(resolve => setTimeout(resolve, 10));
  }
  assert.fail('timed out waiting for the fake archiver');
}

function testSource(name, marker) {
  return [
    `program ${name}`,
    'use archive_left, only: left_value',
    'use archive_right, only: right_value',
    'implicit none',
    'if (left_value() + right_value() /= 42) stop 1',
    marker ? `print '(a)', '${marker}'` : '',
    `end program ${name}`,
    '',
  ].filter(Boolean).join('\n');
}

async function main() {
try {
  fs.mkdirSync(project, { recursive: true });
  fs.mkdirSync(fakeBin, { recursive: true });
  write(fakeAr, `#!/usr/bin/env node
const fs = require('node:fs');
const { spawnSync } = require('node:child_process');
const args = process.argv.slice(2);
const archive = args[1];
const mode = process.env.FO_FAKE_AR_MODE;
fs.appendFileSync(process.env.FO_TEST_AR_LOG,
  JSON.stringify({ args, mode, pid: process.pid, ppid: process.ppid }) + '\\n');
if (args[0] === 'rcs' && mode === 'delay') {
  fs.writeFileSync(archive, 'unfinished archive');
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 500);
  fs.rmSync(archive, { force: true });
}
if (args[0] === 'rcs' && mode === 'hold') {
  fs.writeFileSync(archive, 'cancelled partial archive');
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 60000);
}
if (args[0] === 'rcs' && mode === 'fail') {
  fs.writeFileSync(archive, 'failed partial archive');
  process.exit(37);
}
let forwarded = args;
if (args[0] === 'rcs' && mode === 'omit') forwarded = args.slice(0, 3);
const result = spawnSync(process.env.FO_TEST_REAL_AR, forwarded, { stdio: 'inherit' });
process.exit(result.status === null ? 1 : result.status);
`);
  fs.chmodSync(fakeAr, 0o755);
  write(fakeFc, `#!/usr/bin/env node
const fs = require('node:fs');
const { spawnSync } = require('node:child_process');
const args = process.argv.slice(2);
const outIndex = args.indexOf('-o');
const output = outIndex >= 0 ? args[outIndex + 1] : '';
const isTestLink = !args.includes('-c') && output.includes('/build/fo/bin/');
const isSharedLink = args.includes('-dynamiclib');
const mode = process.env.FO_FAKE_SHARED_MODE;
const testLinkMode = process.env.FO_FAKE_TEST_LINK_MODE;
const truncatedMagic = process.platform === 'darwin'
  ? Buffer.from([0xcf, 0xfa, 0xed, 0xfe])
  : Buffer.from([0x7f, 0x45, 0x4c, 0x46]);
if (isSharedLink) {
  fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
    JSON.stringify({ kind: 'start', output, mode, pid: process.pid }) + '\\n');
  if (mode === 'delay') {
    fs.writeFileSync(output, 'unfinished shared library');
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 500);
    fs.rmSync(output, { force: true });
  }
  if (mode === 'fail' || mode === 'omit') {
    fs.writeFileSync(output, 'incomplete shared library');
    fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
      JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
    process.exit(mode === 'fail' ? 39 : 0);
  }
  if (mode === 'truncated') {
    fs.writeFileSync(output, truncatedMagic);
    fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
      JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
    process.exit(0);
  }
}
if (isTestLink) {
  if (testLinkMode === 'delay') {
    fs.appendFileSync(process.env.FO_TEST_EXE_LOG,
      JSON.stringify({ output, mode: testLinkMode, pid: process.pid }) + '\\n');
    fs.writeFileSync(output, 'unfinished executable');
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 500);
    fs.rmSync(output, { force: true });
  }
  fs.appendFileSync(process.env.FO_TEST_FC_LOG,
    JSON.stringify({ kind: 'start', pid: process.pid }) + '\\n');
  if (testLinkMode === 'fail') {
    fs.writeFileSync(output, 'failed executable link');
    fs.appendFileSync(process.env.FO_TEST_FC_LOG,
      JSON.stringify({ kind: 'done', pid: process.pid }) + '\\n');
    process.exit(41);
  }
  if (testLinkMode === 'truncated') {
    fs.writeFileSync(output, truncatedMagic);
    fs.appendFileSync(process.env.FO_TEST_FC_LOG,
      JSON.stringify({ kind: 'done', pid: process.pid }) + '\\n');
    process.exit(0);
  }
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 250);
}
const result = spawnSync(process.env.FO_TEST_REAL_FC, args, { stdio: 'inherit' });
if (isSharedLink) fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
  JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
if (isTestLink) {
  fs.appendFileSync(process.env.FO_TEST_FC_LOG,
    JSON.stringify({ kind: 'done', pid: process.pid }) + '\\n');
}
process.exit(result.status === null ? 1 : result.status);
`);
  fs.chmodSync(fakeFc, 0o755);
  write(fakeGcc, `#!/usr/bin/env node
const fs = require('node:fs');
const { spawnSync } = require('node:child_process');
const args = process.argv.slice(2);
const outputIndex = args.indexOf('-o');
const output = outputIndex >= 0 ? args[outputIndex + 1] : '';
const shared = args.includes('-shared');
const mode = process.env.FO_FAKE_SHARED_MODE;
const truncatedMagic = process.platform === 'darwin'
  ? Buffer.from([0xcf, 0xfa, 0xed, 0xfe])
  : Buffer.from([0x7f, 0x45, 0x4c, 0x46]);
if (shared) {
  fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
    JSON.stringify({ kind: 'start', output, mode, pid: process.pid }) + '\\n');
  if (mode === 'delay') {
    fs.writeFileSync(output, 'unfinished shared library');
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 500);
    fs.rmSync(output, { force: true });
  }
  if (mode === 'fail' || mode === 'omit') {
    fs.writeFileSync(output, 'incomplete shared library');
    fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
      JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
    process.exit(mode === 'fail' ? 39 : 0);
  }
  if (mode === 'truncated') {
    fs.writeFileSync(output, truncatedMagic);
    fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
      JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
    process.exit(0);
  }
}
const result = spawnSync(process.env.FO_TEST_REAL_GCC, args, { stdio: 'inherit' });
if (shared) fs.appendFileSync(process.env.FO_TEST_SHARED_LOG,
  JSON.stringify({ kind: 'done', mode, pid: process.pid }) + '\\n');
process.exit(result.status === null ? 1 : result.status);
`);
  fs.chmodSync(fakeGcc, 0o755);

  write(path.join(project, 'fpm.toml'), [
    'name = "archive_publication_probe"',
    '[build]',
    'auto-executables = true',
    'auto-tests = true',
    '',
  ].join('\n'));
  write(path.join(project, 'src/left.f90'), [
    'module archive_left', 'implicit none', 'interface',
    'module function left_value() result(value)', 'integer :: value',
    'end function left_value', 'end interface', 'end module archive_left', '',
  ].join('\n'));
  write(path.join(project, 'src/left_impl.f90'), [
    'submodule (archive_left) archive_left_impl', 'implicit none', 'contains',
    'module procedure left_value', 'value = 20', 'end procedure left_value',
    'end submodule archive_left_impl', '',
  ].join('\n'));
  write(path.join(project, 'src/right.f90'), [
    'module archive_right', 'contains', 'integer function right_value()',
    'right_value = 22', 'end function right_value', 'end module archive_right', '',
  ].join('\n'));
  write(path.join(project, 'app/probe.f90'), [
    'program probe', 'use archive_left, only: left_value',
    'use archive_right, only: right_value', 'implicit none',
    "print '(i0)', left_value() + right_value()", 'end program probe', '',
  ].join('\n'));
  write(path.join(project, 'test/test_alpha.f90'), testSource('test_alpha', 'alpha'));
  write(path.join(project, 'test/test_beta.f90'), testSource('test_beta', 'beta'));

  const first = start(['build'], 'delay');
  await waitFor(() => {
    const libDir = path.join(project, 'build/fo/lib');
    if (!fs.existsSync(libDir) || arCalls().length === 0) return false;
    const stages = fs.readdirSync(libDir)
      .filter(name => name.startsWith('.fo-archive.tmp-'));
    if (stages.length !== 1) return false;
    const partial = path.join(libDir, stages[0], 'archive.tmp.a');
    return fs.existsSync(partial) &&
      fs.readFileSync(partial, 'utf8') === 'unfinished archive';
  });
  const stageDirs = fs.readdirSync(path.join(project, 'build/fo/lib'))
    .filter(name => name.startsWith('.fo-archive.tmp-'));
  assert.equal(stageDirs.length, 1, 'the archiver writes inside one owned stage');
  assert.equal(fs.readFileSync(path.join(project, 'build/fo/lib', stageDirs[0],
    'archive.tmp.a'), 'utf8'), 'unfinished archive');
  assert.equal(archivePaths().length, 0, 'no content-key archive is visible mid-write');
  const second = start(['build'], 'normal');
  await monitorArchives([first.child, second.child]);
  const firstResult = await first.done;
  const secondResult = await second.done;
  assert.equal(firstResult.status, 0, firstResult.output);
  assert.equal(secondResult.status, 0, secondResult.output);
  assert.equal(archivePaths().length, 1, 'complete archive is atomically published');
  const beforeNoop = arCalls().length;
  checked(['build']);
  assert.equal(arCalls().length, beforeNoop,
    'unchanged inputs reuse the published archive');

  const report = JSON.parse(checked(['test', '--all', '--json']));
  assert.deepEqual(report.tests.map(test => test.status), ['pass', 'pass'],
    'multiple test links consume the completed archive');
  assertConcurrentTestLinks();
  assert.equal(checked(['exec', 'probe']), '42\n', 'a real executable links and runs');

  const archive = archivePaths()[0];
  const members = execFileSync(realAr, ['t', archive], { encoding: 'utf8' })
    .trim().split('\n');
  assert.equal(members.length, 3, 'module and submodule objects are archived');

  const rightMember = members.find(member => /right/i.test(member));
  assert(rightMember, `archive contains the right implementation member: ${members}`);
  const staleDir = path.join(scratch, 'stale-object');
  const staleSource = path.join(staleDir, 'right.f90');
  const staleObject = path.join(staleDir, rightMember);
  write(staleSource, [
    'module archive_right', 'contains', 'integer function right_value()',
    'right_value = 29', 'end function right_value', 'end module archive_right', '',
  ].join('\n'));
  fs.mkdirSync(path.join(staleDir, 'mod'), { recursive: true });
  execFileSync(realFc, ['-c', '-fPIC', '-J', path.join(staleDir, 'mod'),
    '-o', staleObject, staleSource]);
  execFileSync(realAr, ['rcs', archive, staleObject]);
  assert.deepEqual(execFileSync(realAr, ['t', archive], { encoding: 'utf8' })
    .trim().split('\n'), members,
  'the stale archive remains structurally valid with the expected member names');
  const probeBinary = path.join(project, 'build/fo/bin/probe');
  fs.rmSync(probeBinary, { force: true });
  const beforeStaleRepair = arCalls().length;
  const staleRun = command(['exec', 'probe']);
  assert.equal(staleRun.status, 0, staleRun.stdout + staleRun.stderr);
  assert.equal(staleRun.stdout, '42\n',
    'archive reuse verifies member bytes and rebuilds stale contents');
  assert.equal(arCalls().length, beforeStaleRepair + 1,
    'stale archive member content triggers a rebuild');

  write(path.join(project, 'app/probe.f90'), [
    'program probe', 'use archive_left, only: left_value',
    'use archive_right, only: right_value', 'implicit none',
    "print '(i0)', left_value() + right_value()", 'end program probe',
    '! force a fresh app link without changing the archive key', '',
  ].join('\n'));
  fs.rmSync(archive);
  const beforeInterrupt = arCalls().length;
  const interrupted = start(['build'], 'hold');
  await waitFor(() => {
    const call = arCalls().slice(beforeInterrupt)
      .find(entry => entry.mode === 'hold');
    if (!call) return false;
    const stages = fs.readdirSync(path.join(project, 'build/fo/lib'))
      .filter(name => name.startsWith('.fo-archive.tmp-'));
    return stages.some(name => {
      const partial = path.join(project, 'build/fo/lib', name, 'archive.tmp.a');
      return fs.existsSync(partial) &&
        fs.readFileSync(partial, 'utf8') === 'cancelled partial archive';
    });
  });
  const producer = arCalls().slice(beforeInterrupt)
    .find(entry => entry.mode === 'hold');
  assert.equal(producer.ppid, interrupted.child.pid,
    'the fake archiver is a direct child of the fo build owner');
  const interruptedStages = fs.readdirSync(path.join(project, 'build/fo/lib'))
    .filter(name => name.startsWith('.fo-archive.tmp-'));
  process.kill(interrupted.child.pid, 'SIGTERM');
  const interruptedResult = await interrupted.done;
  assert.equal(interruptedResult.signal, 'SIGTERM',
    'the cancellation oracle terminates the fo build owner');
  assert.equal(archivePaths().length, 0,
    'terminated owner leaves no reusable cache archive');
  if (fixtureToolAlive(producer.pid)) process.kill(producer.pid, 'SIGTERM');
  await waitFor(() => !fixtureToolAlive(producer.pid));
  for (const stage of interruptedStages) {
    fs.rmSync(path.join(project, 'build/fo/lib', stage),
      { recursive: true, force: true });
  }
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-archive.tmp-')), false,
  'cancellation cleanup removes the owner stage after reaping its child');

  const beforeRetry = arCalls().length;
  checked(['build']);
  assert.equal(arCalls().length, beforeRetry + 1,
    'a later owner can republish after cancellation');
  assert.equal(checked(['exec', 'probe']), '42\n',
    'the post-cancellation archive links and runs');

  const stale = Buffer.from('partial stale archive');
  fs.writeFileSync(archive, stale);
  const orphanStage = path.join(project, 'build/fo/lib',
    '.fo-archive.tmp-abandoned');
  fs.mkdirSync(orphanStage);
  fs.writeFileSync(path.join(orphanStage, 'archive.tmp.a'), 'crashed partial');
  write(path.join(project, 'test/test_alpha.f90'), testSource('test_alpha', 'changed'));

  const beforeFailure = arCalls().length;
  const failed = command(['test', '--all', '--json'], 'fail');
  assert.notEqual(failed.status, 0, 'a nonzero archiver status fails the build');
  assert.equal(arCalls().length, beforeFailure + 1,
    'invalid cached archive is rebuilt');
  assert.deepEqual(fs.readFileSync(archive), stale,
    'failed staging does not replace or delete the stale final path');

  const mismatch = command(['test', '--all', '--json'], 'omit');
  assert.notEqual(mismatch.status, 0, 'successful ar with missing members is rejected');
  assert.deepEqual(fs.readFileSync(archive), stale,
    'membership failure leaves the previous final path untouched');

  const recovered = JSON.parse(checked(['test', '--all', '--json']));
  assert.deepEqual(recovered.tests.map(test => test.status), ['pass', 'pass'],
    'stale incomplete archive is replaced after successful validation');
  assert.equal(checked(['exec', 'probe']), '42\n',
    'recovered archive still links and runs');

  const binariesBeforeLink = stableTestBinaries();
  assert.equal(binariesBeforeLink.length, 2, 'both test executables are available');
  const digestsBeforeLink = new Map(binariesBeforeLink.map(file => [file, fileDigest(file)]));
  write(path.join(project, 'test/test_alpha.f90'), testSource('test_alpha', 'exe-staging'));
  const delayedExecutable = start(['test', '--all', '--json'], 'normal', 'delay');
  await waitFor(() => {
    const call = delayedExecutableCalls()[0];
    return call && fs.existsSync(call.output) &&
      fs.readFileSync(call.output, 'utf8') === 'unfinished executable';
  });
  const delayedExecutableCall = delayedExecutableCalls()[0];
  const changedBinariesDuringLink = binariesBeforeLink.filter(file =>
    !fs.existsSync(file) || fileDigest(file) !== digestsBeforeLink.get(file));
  const delayedExecutableResult = await delayedExecutable.done;
  assert.equal(delayedExecutableResult.status, 0, delayedExecutableResult.output);
  const delayedExecutableReport = jsonReport(delayedExecutableResult.output);
  assert.deepEqual(delayedExecutableReport.tests.map(test => test.status), ['pass', 'pass']);
  assert.deepEqual(changedBinariesDuringLink, [],
    `stable executable changed while linker wrote ${delayedExecutableCall.output}`);
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/bin'))
    .some(name => name.startsWith('.fo-link.tmp-')), false,
  'successful executable publication removes its owned staging directory');

  const binariesBeforeFailedLink = stableTestBinaries();
  const digestsBeforeFailedLink = new Map(binariesBeforeFailedLink
    .map(file => [file, fileDigest(file)]));
  write(path.join(project, 'test/test_alpha.f90'), testSource('test_alpha', 'exe-fail'));
  const failedExecutable = command(['test', '--all', '--json'], 'normal', 'fail');
  assert.notEqual(failedExecutable.status, 0,
    'a nonzero executable linker status fails the test build');
  for (const file of binariesBeforeFailedLink) {
    assert.equal(fileDigest(file), digestsBeforeFailedLink.get(file),
      'failed staged link leaves the previous executable unchanged');
  }
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/bin'))
    .some(name => name.startsWith('.fo-link.tmp-')), false,
  'failed executable publication removes only its owned staging directory');

  const binariesBeforeTruncatedLink = stableTestBinaries();
  const digestsBeforeTruncatedLink = new Map(binariesBeforeTruncatedLink
    .map(file => [file, fileDigest(file)]));
  write(path.join(project, 'test/test_alpha.f90'),
    testSource('test_alpha', 'exe-truncated'));
  const truncatedExecutable = command(['test', '--all', '--json'],
    'normal', 'truncated');
  assert.notEqual(truncatedExecutable.status, 0,
    'a linker that returns a magic-only executable is rejected');
  for (const file of binariesBeforeTruncatedLink) {
    assert.equal(fileDigest(file), digestsBeforeTruncatedLink.get(file),
      'a truncated staged executable does not replace the previous binary');
  }
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/bin'))
    .some(name => name.startsWith('.fo-link.tmp-')), false,
  'rejected executable publication removes its owned staging directory');

  write(path.join(project, 'fpm.toml'), [
    'name = "archive_publication_probe"', '[build]',
    'auto-executables = true', 'auto-tests = true', '[extra.fo]',
    'link = "shared"', 'pic = "true"', '',
  ].join('\n'));
  const sharedFirst = start(['test', '--all', '--json'], 'delay');
  await waitFor(() => {
    const call = sharedCalls().find(entry => entry.mode === 'delay');
    return call && fs.existsSync(call.output) &&
      fs.readFileSync(call.output, 'utf8') === 'unfinished shared library';
  });
  const delayedShared = sharedCalls().find(entry => entry.mode === 'delay');
  assert.equal(path.basename(delayedShared.output).startsWith('.fo-shared.tmp-'), true,
    'shared linker writes to its unique hidden sibling');
  assert.equal(sharedPaths().length, 0,
    'no content-key shared library is visible during its link');
  const sharedSecond = start(['test', '--all', '--json'], 'normal');
  await monitorSharedLibraries([sharedFirst.child, sharedSecond.child]);
  const sharedFirstResult = await sharedFirst.done;
  const sharedSecondResult = await sharedSecond.done;
  assert.equal(sharedFirstResult.status, 0, sharedFirstResult.output);
  assert.equal(sharedSecondResult.status, 0, sharedSecondResult.output);
  for (const result of [sharedFirstResult, sharedSecondResult]) {
    const report = jsonReport(result.output);
    assert.deepEqual(report.tests.map(test => test.status), ['pass', 'pass'],
      'concurrent test consumers link and run against the published shared library');
  }
  assert.equal(sharedPaths().length, 1, 'one complete keyed shared library is published');
  assertVisibleSharedLibrariesComplete();
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-shared.tmp-')), false,
  'successful shared-library publication removes its own staging markers');

  const beforeWarm = sharedCalls().length;
  write(path.join(project, 'test/test_beta.f90'), testSource('test_beta', 'shared-warm'));
  const sharedWarm = JSON.parse(checked(['test', '--all', '--json']));
  assert.deepEqual(sharedWarm.tests.map(test => test.status), ['pass', 'pass']);
  assert.equal(sharedCalls().length, beforeWarm,
    'unchanged shared-library inputs reuse the validated keyed output');

  const shared = sharedPaths()[0];
  const staleShared = Buffer.from('incomplete stale shared library');
  fs.writeFileSync(shared, staleShared);
  write(path.join(project, 'test/test_alpha.f90'), testSource('test_alpha', 'shared-fail'));
  const beforeSharedFailure = sharedCalls().length;
  const sharedFailed = command(['test', '--all', '--json'], 'fail');
  assert.notEqual(sharedFailed.status, 0,
    'a nonzero shared linker status fails the test build');
  assert.equal(sharedCalls().length, beforeSharedFailure + 1,
    'an invalid cached shared library is rebuilt');
  assert.deepEqual(fs.readFileSync(shared), staleShared,
    'failed staged shared link preserves the previous final path');
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-shared.tmp-')), false,
  'failed shared-library publication removes its owned marker directory');

  const malformedShared = command(['test', '--all', '--json'], 'omit');
  assert.notEqual(malformedShared.status, 0,
    'successful linker exit with an invalid image is rejected');
  assert.deepEqual(fs.readFileSync(shared), staleShared,
    'invalid staged shared output does not replace the previous final path');
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-shared.tmp-')), false,
  'rejected shared-library publication removes its owned marker directory');

  const sharedRecovered = JSON.parse(checked(['test', '--all', '--json']));
  assert.deepEqual(sharedRecovered.tests.map(test => test.status), ['pass', 'pass'],
    'stale incomplete shared library is replaced after successful validation');
  assertVisibleSharedLibrariesComplete();
  assert.equal(checked(['exec', 'probe']), '42\n',
    'a real executable still links and runs after shared-library recovery');

  const disabledCache = path.join(scratch, 'cache-is-a-file');
  write(disabledCache, 'cache initialization must fail');
  write(path.join(project, 'test/test_alpha.f90'),
    testSource('test_alpha', 'shared-no-cache-cleanup'));
  const beforeNoCacheShared = sharedCalls().length;
  command(['test', '--all', '--json'], 'normal', 'normal',
    { FO_CACHE_DIR: disabledCache });
  assert.equal(sharedCalls().length, beforeNoCacheShared + 1,
    'cache initialization failure reaches the process-owned shared linker');
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-shared.tmp-')), false,
  'no-cache shared consumers remove their owned marker directory');

  write(path.join(project, 'test/test_alpha.f90'),
    testSource('test_alpha', 'shared-no-cache-failure'));
  const noCacheSharedFailure = command(['test', '--all', '--json'],
    'fail', 'normal', { FO_CACHE_DIR: disabledCache });
  assert.notEqual(noCacheSharedFailure.status, 0,
    'shared link failure propagates when cache initialization is unavailable');
  assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
    .some(name => name.startsWith('.fo-shared.tmp-')), false,
  'no-cache shared link failure removes its owned marker directory');

  const sharedBackup = path.join(scratch, 'saved-shared-library');
  fs.renameSync(shared, sharedBackup);
  try {
    write(path.join(project, 'test/test_alpha.f90'),
      testSource('test_alpha', 'shared-truncated'));
    const truncatedShared = command(['test', '--all', '--json'], 'truncated');
    assert.notEqual(truncatedShared.status, 0,
      'a linker that returns a magic-only shared library is rejected');
    assert.equal(sharedPaths().length, 0,
      'a truncated shared library is never published at its content-key path');
    assert.equal(fs.readdirSync(path.join(project, 'build/fo/lib'))
      .some(name => name.startsWith('.fo-shared.tmp-')), false,
    'rejected shared-library publication removes its owned marker directory');
  } finally {
    if (fs.existsSync(sharedBackup)) fs.renameSync(sharedBackup, shared);
  }
  const sharedFinalReport = JSON.parse(checked(['test', '--all', '--json']));
  assert.deepEqual(sharedFinalReport.tests.map(test => test.status), ['pass', 'pass']);
  assert.equal(checked(['exec', 'probe']), '42\n',
    'archive and executable behavior remains valid after truncated image rejection');
  console.log('artifact-publication: atomic archives, shared libraries, and executables pass');
} finally {
  try {
    await cleanupFixtureProcesses();
  } finally {
    fs.rmSync(scratch, { recursive: true, force: true });
  }
}
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
