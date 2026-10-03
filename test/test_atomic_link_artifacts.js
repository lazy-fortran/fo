#!/usr/bin/env node
// Run: TMPDIR=/var/tmp node test/test_atomic_link_artifacts.js
// Exercise published archives through the real fo build, link and exec paths.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const crypto = require('node:crypto');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const driver = process.env.FO || 'fo';
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'fo-atomic-link-'));
const fixture = path.join(scratch, 'fixture');
const archiver = path.join(scratch, 'bin', 'ar');
const realAr = spawnSync('sh', ['-c', 'command -v ar'], {
  encoding: 'utf8'
}).stdout.trim();
const env = { ...process.env, FO_JOBS: '1' };

function write(file, text) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, text);
}

function run(args, options = {}) {
  const result = spawnSync(driver, args, {
    cwd: fixture,
    encoding: 'utf8',
    maxBuffer: 8 * 1024 * 1024,
    env: { ...env, ...options.env },
    ...options
  });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, `${args.join(' ')}: ${result.stderr}`);
  return result;
}

function source(value, child) {
  write(path.join(fixture, 'src/model.f90'), [
    `! ${path.basename(scratch)}`,
    'module artifact_model', '    interface',
    '        module integer function root_value()',
    '        end function root_value',
    '        module integer function leaf_value()',
    '        end function leaf_value',
    '    end interface', 'end module artifact_model', ''
  ].join('\n'));
  write(path.join(fixture, 'src/10_root.f90'), [
    `! ${path.basename(scratch)}`,
    'submodule (artifact_model) root_impl', 'contains',
    '    module procedure root_value', `        root_value = ${value}`,
    '    end procedure root_value', 'end submodule root_impl', ''
  ].join('\n'));
  write(path.join(fixture, 'src/20_leaf.f90'), [
    `! ${path.basename(scratch)}`,
    'submodule (artifact_model:root_impl) leaf_impl', 'contains',
    '    module procedure leaf_value', `        leaf_value = ${child}`,
    '    end procedure leaf_value', 'end submodule leaf_impl', ''
  ].join('\n'));
}

function app(comment = '') {
  write(path.join(fixture, 'app/main.f90'), [
    'program artifact_consumer',
    '    use artifact_model, only: root_value, leaf_value',
    '    implicit none', `    ! ${comment}`,
    "    print '(i0)', root_value() + leaf_value()",
    'end program artifact_consumer', ''
  ].join('\n'));
}

function archives() {
  return fs.readdirSync(path.join(fixture, 'build/fo/lib'))
    .filter(name => /^objects_.*\.a$/.test(name))
    .map(name => path.join(fixture, 'build/fo/lib', name)).sort();
}

function digest(file) {
  return crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
}

function build() {
  const result = run(['build']);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result;
}

function checkOutput() {
  const result = run(['exec', '--no-build', 'atomic_link_fixture']);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  assert.equal(result.stdout.trim(), '42', 'the linked module/submodule consumer runs');
}

function arWrapper(body) {
  write(archiver, `#!/bin/sh\n${body}\n`);
  fs.chmodSync(archiver, 0o755);
}

function withArchiver(extraEnv = {}) {
  return {
    ...env,
    PATH: `${path.dirname(archiver)}:${process.env.PATH}`,
    REAL_AR: realAr,
    ...extraEnv
  };
}

function startBuild(options = {}) {
  const child = spawn(driver, options.args || ['build'], {
    cwd: fixture,
    env: { ...env, ...options.env },
    encoding: 'utf8',
    detached: options.detached === true,
    stdio: ['ignore', 'pipe', 'pipe']
  });
  let stdout = '';
  let stderr = '';
  child.stdout.setEncoding('utf8').on('data', data => { stdout += data; });
  child.stderr.setEncoding('utf8').on('data', data => { stderr += data; });
  const done = new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('close', (status, signal) => resolve({ status, signal, stdout, stderr }));
  });
  return { child, done };
}

async function waitFor(file, timeoutMs = 30000) {
  const until = Date.now() + timeoutMs;
  while (Date.now() < until) {
    if (fs.existsSync(file)) return;
    await new Promise(resolve => setTimeout(resolve, 25));
  }
  throw new Error(`timed out waiting for ${file}`);
}

async function concurrentPublication() {
  const callDir = path.join(scratch, 'ar-concurrent');
  fs.mkdirSync(callDir);
  arWrapper([
    'if [ "$1" = rcs ]; then',
    '  touch "$CALL_DIR/started.$$"',
    '  sleep 0.5',
    'fi',
    'exec "$REAL_AR" "$@"'
  ].join('\n'));
  const opts = { env: withArchiver({ CALL_DIR: callDir }) };
  const a = startBuild({ ...opts, args: ['exec', 'atomic_link_fixture'] });
  const b = startBuild({ ...opts, args: ['exec', 'atomic_link_fixture'] });
  const [ra, rb] = await Promise.all([a.done, b.done]);
  assert.equal(ra.status, 0, ra.stdout + ra.stderr);
  assert.equal(rb.status, 0, rb.stdout + rb.stderr);
  assert.match(ra.stdout, /^42$/m, 'first consumer executes the complete archive');
  assert.match(rb.stdout, /^42$/m, 'second consumer executes the complete archive');
  assert.ok(fs.readdirSync(callDir).length >= 1,
    'at least one concurrent request produced a complete archive');
  assert.equal(archives().length, 2, 'new linked inputs have a separate keyed archive');
  checkOutput();
}

async function interruptedPublication() {
  const marker = path.join(scratch, 'ar-interrupted');
  arWrapper([
    'if [ "$1" = rcs ]; then',
    '  printf "partial archive" > "$2"',
    '  : > "$MARKER"',
    '  exec sleep 60',
    'fi',
    'exec "$REAL_AR" "$@"'
  ].join('\n'));
  const before = archives().map(file => [file, digest(file)]);
  const task = startBuild({ detached: true, env: withArchiver({ MARKER: marker }) });
  await waitFor(marker);
  try {
    process.kill(-task.child.pid, 'SIGTERM');
  } catch (error) {
    if (error.code !== 'ESRCH') throw error;
  }
  const cancelled = await task.done;
  assert.equal(cancelled.signal, 'SIGTERM', 'the staged producer was interrupted');
  assert.deepEqual(archives().map(file => [file, digest(file)]), before,
    'cancelled partial output never replaces or adds a reusable archive');
}

async function main() {
  write(path.join(fixture, 'fpm.toml'), 'name = "atomic_link_fixture"\n');
  source(40, 2);
  app('initial');
  build();
  checkOutput();

  const original = archives();
  assert.equal(original.length, 1, 'one keyed archive was published');
  const originalStat = fs.statSync(original[0], { bigint: true });
  build();
  const reusedStat = fs.statSync(original[0], { bigint: true });
  assert.equal(reusedStat.mtimeNs, originalStat.mtimeNs,
    'unchanged inputs reuse the completed archive');

  fs.writeFileSync(original[0], 'truncated archive');
  const corruptDigest = digest(original[0]);
  app('force validation of existing archive');
  build();
  assert.notEqual(digest(original[0]), corruptDigest,
    'the corrupt archive was replaced');
  checkOutput();

  source(39, 3);
  await concurrentPublication();

  source(38, 4);
  await interruptedPublication();
  fs.rmSync(archiver, { force: true });
  build();
  checkOutput();
  console.log('atomic-link-artifacts: concurrent, interrupted, corrupt and warm paths pass');
}

main().finally(() => fs.rmSync(scratch, { recursive: true, force: true }));
