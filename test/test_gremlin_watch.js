#!/usr/bin/env node
// Behavioral oracle for event-gated Gremlin generation capture.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const fo = process.argv[2] || process.env.FO;
if (!fo) throw new Error('pass the isolated fo executable as the first argument');
if (!['linux', 'darwin'].includes(process.platform)) {
  console.log('Gremlin watch: skipped (requires inotify or kqueue)');
  process.exit(0);
}
const scratch = fs.realpathSync(fs.mkdtempSync('/var/tmp/fo-gremlin-watch-'));
const compilerLookup = spawnSync('which', ['gfortran'], { encoding: 'utf8' });
assert.equal(compilerLookup.status, 0, 'gfortran must be available for the fixture');
const realCompiler = compilerLookup.stdout.trim();
const project = path.join(scratch, 'project');
const dependency = path.join(scratch, 'dependency');
const gate = path.join(scratch, 'test.gate');
const startedMarker = path.join(scratch, 'test.started');
const bin = path.join(scratch, 'bin');
const calls = path.join(scratch, 'toolchain.jsonl');
const cache = path.join(scratch, 'cache');
const state = path.join(scratch, 'state');
const captureCounter = path.join(scratch, 'capture-counter.json');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  XDG_CACHE_HOME: path.join(scratch, 'xdg'), FO_PREFIX: path.join(scratch, 'prefix'),
  FO_CACHE_DIR: cache, FO_GREMLIN_STATE_DIR: state, TMPDIR: '/var/tmp',
  FO_GREMLIN_TEST_CAPTURE_COUNTER: captureCounter, FO_JOBS: '1', FO_DISABLE_SELF_REFRESH: '1', PATH: `${bin}:${process.env.PATH}` };
fs.mkdirSync(bin, { recursive: true });
fs.mkdirSync(env.HOME, { recursive: true });

function run(args, cwd = project, timeout = 30000) {
  return spawnSync(fo, args, { cwd, env, encoding: 'utf8', timeout,
    maxBuffer: 8 * 1024 * 1024 });
}
function json(args) {
  const result = run(args);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}
function wait(ms) { return new Promise(resolve => setTimeout(resolve, ms)); }
function discoveryCount() {
  if (!fs.existsSync(calls)) return 0;
  return fs.readFileSync(calls, 'utf8').trim().split('\n').filter(Boolean)
    .map(line => JSON.parse(line)).filter(item => item.cwd === project &&
      item.args.length === 1 && item.args[0] === '--version').length;
}
function captureCount() {
  return fs.existsSync(captureCounter)
    ? JSON.parse(fs.readFileSync(captureCounter, 'utf8')).count : 0;
}
async function expectOneCapture(edit) {
  const before = captureCount();
  const beforeStatus = json(['gremlin', 'status', '--dir', project, '--lane', 'watch',
    '--session', session, '--json']);
  edit();
  const deadline = Date.now() + 15000;
  let settleDeadline = 0;
  let current = beforeStatus;
  do {
    current = json(['gremlin', 'status', '--dir', project, '--lane', 'watch',
      '--session', session, '--json']);
    if (current.active_generation !== beforeStatus.active_generation &&
        captureCount() > before) {
      if (settleDeadline === 0) settleDeadline = Date.now() + 700;
    }
    if (settleDeadline > 0 && Date.now() >= settleDeadline) break;
    await wait(50);
  } while (Date.now() < deadline);
  assert.notEqual(current.active_generation, beforeStatus.active_generation,
    'relevant edits must publish a new immutable generation');
  assert.equal(captureCount(), before + 1,
    'one event burst must perform exactly one immutable capture');
  await wait(300);
}
function makeWritableTree(root) {
  if (!fs.existsSync(root)) return;
  const stat = fs.lstatSync(root);
  if (stat.isSymbolicLink()) return;
  if (stat.isDirectory()) {
    for (const entry of fs.readdirSync(root)) makeWritableTree(path.join(root, entry));
    fs.chmodSync(root, 0o755);
  } else {
    fs.chmodSync(root, 0o644);
  }
}

let session;
let sessionLane = 'watch';
async function main() {
try {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.mkdirSync(path.join(project, 'src'), { recursive: true });
  fs.mkdirSync(path.join(dependency, 'src'), { recursive: true });
  const fifo = spawnSync('mkfifo', [gate], { encoding: 'utf8' });
  assert.equal(fifo.status, 0, fifo.stdout + fifo.stderr);
  fs.writeFileSync(path.join(project, 'fpm.toml'), [
    'name = "gremlin_watch_probe"',
    '[dependencies]',
    'watch_dep = { path = "../dependency" }',
    'nested_dep = { path = "build/nested_dependency" }', ''
  ].join('\n'));
  fs.mkdirSync(path.join(project, 'build/nested_dependency/src'), { recursive: true });
  fs.writeFileSync(path.join(project, 'build/nested_dependency/fpm.toml'),
    'name = "nested_dep"\n');
  fs.writeFileSync(path.join(project, 'build/nested_dependency/src/nested.f90'),
    'module nested_dep\nend module nested_dep\n');
  fs.writeFileSync(path.join(dependency, 'fpm.toml'), 'name = "watch_dep"\n');
  fs.writeFileSync(path.join(project, 'src/oracle.data'), 'captured input\n');
  fs.writeFileSync(path.join(dependency, 'src/depmod.f90'), [
    'module depmod', 'implicit none', 'integer, parameter :: dep_value = 1',
    'end module depmod', ''
  ].join('\n'));
  fs.writeFileSync(path.join(project, 'test/test_watch.f90'), [
    'program test_watch', 'use depmod, only: dep_value', 'implicit none',
    'integer :: unit', 'character :: token',
    `open(newunit=unit, file='${startedMarker}', status='replace')`,
    "write(unit, '(a)') 'started'", 'close(unit)',
    `open(newunit=unit, file='${gate}', access='stream', form='unformatted', action='read')`,
    'read(unit) token', 'close(unit)', 'if (dep_value /= 1) error stop 1',
    'end program test_watch', ''
  ].join('\n'));
  fs.writeFileSync(path.join(bin, 'gfortran'), [
    '#!/usr/bin/env node',
    `const fs = require('node:fs'); const { spawnSync } = require('node:child_process');`,
    `const args = process.argv.slice(2); if (args.includes('--version')) fs.appendFileSync(${JSON.stringify(calls)}, JSON.stringify({ cwd: process.cwd(), args }) + String.fromCharCode(10));`,
    `const child = spawnSync(${JSON.stringify(realCompiler)}, args, { stdio: 'inherit' }); process.exit(child.status === null ? 1 : child.status);`, ''
  ].join('\n'));
  fs.chmodSync(path.join(bin, 'gfortran'), 0o755);

  const started = json(['gremlin', 'start', '--dir', project, '--lane', 'watch',
    '--target', 'test_watch', '--campaign-seconds', '60', '--timeout-seconds', '5']);
  session = started.session_id;
  assert.ok(session, JSON.stringify(started));

  const startedEnd = Date.now() + 15000;
  while (!fs.existsSync(startedMarker) && Date.now() < startedEnd) await wait(50);
  assert.ok(fs.existsSync(startedMarker), 'initial test did not reach its blocking gate');
  const initialStatus = json(['gremlin', 'status', '--dir', project, '--lane', 'watch',
    '--session', session, '--json']);
  const initialGeneration = initialStatus.active_generation;
  const idleBaseline = discoveryCount();
  const idleCaptures = captureCount();
  await wait(1500);
  assert.equal(discoveryCount(), idleBaseline, 'idle sessions must not rediscover the compiler');
  assert.equal(captureCount(), idleCaptures, 'idle sessions must not recapture');
  assert.ok(fs.statSync(captureCounter).size < 256, 'test counter must remain one bounded record');
  const idleStatus = json(['gremlin', 'status', '--dir', project, '--lane', 'watch',
    '--session', session, '--json']);
  assert.equal(idleStatus.active_generation, initialGeneration,
    'idle sessions must not publish duplicate generations');

  await expectOneCapture(() => {
    fs.writeFileSync(path.join(project, 'test/test_watch.f90'),
      fs.readFileSync(path.join(project, 'test/test_watch.f90'), 'utf8') + '\n! source edit\n');
  });
  await expectOneCapture(() => {
    const file = path.join(project, 'test/test_watch.f90');
    fs.appendFileSync(file, '! burst a\n');
    fs.appendFileSync(file, '! burst b\n');
    fs.appendFileSync(file, '! burst c\n');
  });
  await expectOneCapture(() => fs.renameSync(path.join(project, 'src/oracle.data'),
    path.join(project, 'src/oracle.renamed')));
  await expectOneCapture(() => fs.unlinkSync(path.join(project, 'src/oracle.renamed')));
  await expectOneCapture(() => fs.appendFileSync(path.join(project, 'fpm.toml'), '# manifest edit\n'));
  await expectOneCapture(() => fs.appendFileSync(path.join(dependency, 'src/depmod.f90'),
    '! path dependency edit\n'));
  await expectOneCapture(() => fs.appendFileSync(
    path.join(project, 'build/nested_dependency/src/nested.f90'), '! nested edit\n'));
  await expectOneCapture(() => {
    const retired = path.join(scratch, 'dependency-retired');
    fs.renameSync(dependency, retired);
    fs.mkdirSync(path.join(dependency, 'src'), { recursive: true });
    fs.writeFileSync(path.join(dependency, 'fpm.toml'), 'name = "watch_dep"\n');
    fs.writeFileSync(path.join(dependency, 'src/depmod.f90'),
      'module depmod\nimplicit none\ninteger, parameter :: dep_value = 1\nend module depmod\n');
  });
  await expectOneCapture(() => {
    fs.mkdirSync(path.join(project, 'src/new/deep'), { recursive: true });
    fs.writeFileSync(path.join(project, 'src/new/deep/input.data'), 'before subscription\n');
  });
  await expectOneCapture(() => fs.appendFileSync(
    path.join(project, 'src/new/deep/input.data'), 'after subscription\n'));
  await expectOneCapture(() => fs.writeFileSync(path.join(project, 'fpm.toml'),
    fs.readFileSync(path.join(project, 'fpm.toml'), 'utf8')
      .replace('nested_dep = { path = "build/nested_dependency" }\n', '')));
  const retiredBaseline = captureCount();
  fs.appendFileSync(path.join(project, 'build/nested_dependency/src/nested.f90'),
    '! retired dependency edit\n');
  await wait(800);
  assert.equal(captureCount(), retiredBaseline,
    'retired closure roots must not trigger capture');
  const beforeGit = discoveryCount();
  for (const metadata of ['.git', '.bzr']) {
    fs.mkdirSync(path.join(project, metadata), { recursive: true });
    fs.appendFileSync(path.join(project, metadata, 'ignored'), 'ignored\n');
  }
  await wait(800);
  assert.equal(discoveryCount(), beforeGit, '.git and .bzr edits must not trigger capture');

  const beforeUnrelated = discoveryCount();
  fs.writeFileSync(path.join(scratch, 'outside.txt'), 'outside closure');
  await wait(800);
  assert.equal(discoveryCount(), beforeUnrelated, 'outside files must not trigger capture');
  const stopStarted = Date.now();
  const stopped = run(['gremlin', 'stop', '--dir', project, '--lane', 'watch',
    '--session', session]);
  assert.equal(stopped.status, 0, stopped.stdout + stopped.stderr);
  assert.ok(Date.now() - stopStarted < 5000, 'Gremlin stop must be bounded');

  const failedObserver = path.join(scratch, 'missing-observer-parent/counter.json');
  const observerStart = spawnSync(fo, ['gremlin', 'start', '--dir', project,
    '--lane', 'observer-error', '--target', 'test_watch', '--campaign-seconds', '60',
    '--timeout-seconds', '5'], { cwd: project,
    env: { ...env, FO_GREMLIN_TEST_CAPTURE_COUNTER: failedObserver },
    encoding: 'utf8', timeout: 30000 });
  assert.equal(observerStart.status, 0, observerStart.stdout + observerStart.stderr);
  session = JSON.parse(observerStart.stdout).session_id;
  sessionLane = 'observer-error';
  const observerDeadline = Date.now() + 15000;
  let observerStatus;
  do {
    observerStatus = json(['gremlin', 'status', '--dir', project, '--lane', 'observer-error',
      '--session', session, '--json']);
    if (observerStatus.active_generation) break;
    await wait(50);
  } while (Date.now() < observerDeadline);
  assert.ok(observerStatus.active_generation, 'failed test observer must not fail capture');
  assert.ok(!fs.existsSync(failedObserver), 'the observer failure was actually induced');
  const observerStop = run(['gremlin', 'stop', '--dir', project, '--lane', 'observer-error',
    '--session', session]);
  assert.equal(observerStop.status, 0, observerStop.stdout + observerStop.stderr);
  session = undefined;
  const watchProject = path.join(scratch, 'watch-project');
  const checkMarker = path.join(scratch, 'watch-checks');
  fs.mkdirSync(path.join(watchProject, 'src'), { recursive: true });
  fs.writeFileSync(path.join(watchProject, 'fpm.toml'), 'name = "watch_cli_probe"\n');
  fs.writeFileSync(path.join(watchProject, 'src/sample.f90'), 'module sample\nend module sample\n');
  fs.writeFileSync(path.join(watchProject, 'src/second.f90'), 'module second\nend module second\n');
  fs.writeFileSync(path.join(bin, 'fo'), [
    '#!/usr/bin/env node',
    `require('node:fs').appendFileSync(${JSON.stringify(checkMarker)}, 'check' + String.fromCharCode(10));`, ''
  ].join('\n'));
  fs.chmodSync(path.join(bin, 'fo'), 0o755);
  const watch = spawn(fo, ['watch', '--fmt'], { cwd: watchProject, env,
    detached: true, stdio: ['ignore', 'pipe', 'pipe'] });
  let watchOutput = '';
  watch.stdout.setEncoding('utf8').on('data', chunk => { watchOutput += chunk; });
  const checkCount = () => fs.existsSync(checkMarker)
    ? fs.readFileSync(checkMarker, 'utf8').trim().split('\n').length : 0;
  const waitForChecks = async expected => {
    const end = Date.now() + 5000;
    while (Date.now() < end && checkCount() < expected) await wait(50);
    assert.equal(checkCount(), expected, `watch did not run ${expected} checks: ${watchOutput}`);
  };
  try {
    await wait(300);
    fs.appendFileSync(path.join(watchProject, 'src/sample.f90'), '! first edit\n');
    await waitForChecks(1);
    // Publish each complete edit atomically while the formatter is resident.
    for (const name of ['sample', 'second']) {
      const temporary = path.join(scratch, `${name}.save`);
      fs.writeFileSync(temporary,
        `module ${name}\ninteger :: ${name}_value\nend module ${name}\n`);
      fs.renameSync(temporary, path.join(watchProject, `src/${name}.f90`));
    }
    await waitForChecks(2);
    for (const name of ['sample', 'second']) {
      assert.ok(fs.readFileSync(path.join(watchProject, `src/${name}.f90`), 'utf8')
        .includes(`    integer :: ${name}_value`), 'watch --fmt must format every edited file');
    }
    await wait(700);
    assert.equal(checkCount(), 2, 'fo watch must debounce one burst to one check');
    fs.mkdirSync(path.join(watchProject, 'src/created/deep'), { recursive: true });
    const populated = path.join(watchProject, 'src/created/deep/populated.f90');
    fs.writeFileSync(populated,
      'module populated\ninteger :: populated_value\nend module populated\n');
    await waitForChecks(3);
    assert.ok(fs.readFileSync(populated, 'utf8').includes('    integer :: populated_value'),
      'watch --fmt must catch up files populated before a directory subscription');
    await wait(700);
    assert.equal(checkCount(), 3, 'format reconciliation must not trigger duplicate checks');
  } finally {
    const closed = new Promise(resolve => watch.once('close', resolve));
    process.kill(-watch.pid, 'SIGTERM');
    await Promise.race([closed, wait(2000).then(() => {
      try { process.kill(-watch.pid, 'SIGKILL'); } catch (error) {
        if (error.code !== 'ESRCH') throw error;
      }
    })]);
    await Promise.race([closed, wait(1000)]);
    watch.stdout.destroy();
    watch.stderr.destroy();
  }
  console.log('Gremlin and fo watch: idle, burst debounce, manifest, dependency and scope verified');
} finally {
  if (session) run(['gremlin', 'stop', '--dir', project, '--lane', sessionLane,
    '--session', session]);
  makeWritableTree(scratch);
  fs.rmSync(scratch, { recursive: true, force: true });
}
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
