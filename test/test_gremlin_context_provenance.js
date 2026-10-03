#!/usr/bin/env node
// Native probes must execute Git independently of the compiler argv/output.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');
const fo = path.resolve(process.argv[2]);
const scratch = fs.mkdtempSync('/var/tmp/fo-gremlin-provenance-');
const project = path.join(scratch, 'project');
const state = path.join(scratch, 'state');
const counter = path.join(scratch, 'capture.json');
const bin = path.join(scratch, 'bin');
const calls = path.join(scratch, 'compiler-calls.jsonl');
const realCompiler = spawnSync('which', ['gfortran'], { encoding: 'utf8' }).stdout.trim();
assert.ok(realCompiler, 'fixture needs gfortran');
const env = { ...process.env, HOME: path.join(scratch, 'home'),
  FO_PREFIX: path.join(scratch, 'prefix'), XDG_CACHE_HOME: path.join(scratch, 'xdg'),
  FO_CACHE_DIR: path.join(scratch, 'cache'), FO_GREMLIN_STATE_DIR: state,
  FO_GREMLIN_TEST_CAPTURE_COUNTER: counter, FO_JOBS: '1',
  FO_DISABLE_SELF_REFRESH: '1', TMPDIR: scratch, PATH: `${bin}:${process.env.PATH}` };
for (const dir of [bin, env.HOME, path.join(project, 'test')]) {
  fs.mkdirSync(dir, { recursive: true });
}
fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "provenance_oracle"\n');
fs.writeFileSync(path.join(project, 'test/test_provenance.f90'),
  'program test_provenance\nimplicit none\nend program test_provenance\n');
fs.writeFileSync(path.join(bin, 'gfortran'), [
  '#!/usr/bin/env node',
  "const fs = require('node:fs'); const { spawnSync } = require('node:child_process');",
  `const args = process.argv.slice(2); fs.appendFileSync(${JSON.stringify(calls)},`,
  '  JSON.stringify({ cwd: process.cwd(), args }) + String.fromCharCode(10));',
  "if (args[0] === '--version') { console.log('PROVENANCE_COMPILER_ORACLE');",
  '  process.exit(0); }',
  `const result = spawnSync(${JSON.stringify(realCompiler)}, args, { stdio: 'inherit' });`,
  'process.exit(result.status === null ? 1 : result.status);', ''
].join('\n'));
fs.chmodSync(path.join(bin, 'gfortran'), 0o755);
function git(args) {
  const result = spawnSync('git', args, { cwd: project, env, encoding: 'utf8' });
  assert.equal(result.status, 0, result.stderr);
  return result.stdout.trim();
}
git(['init', '-q']);
git(['config', 'user.name', 'Provenance Oracle']);
git(['config', 'user.email', 'oracle@example.invalid']);
git(['add', 'fpm.toml', 'test']);
git(['commit', '-qm', 'initial fixture']);
const initialHead = git(['rev-parse', 'HEAD']);
const lane = 'provenance';
let session;
function command(args) {
  const result = spawnSync(fo, args, { cwd: project, env, encoding: 'utf8',
    timeout: 30000, maxBuffer: 8 * 1024 * 1024 });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout.trim());
}
function captured() {
  return fs.existsSync(counter) ? JSON.parse(fs.readFileSync(counter, 'utf8')) : null;
}
function identity(generation) {
  const file = path.join(state, 'fo/gremlin/generations', generation, 'identity.txt');
  return Object.fromEntries(fs.readFileSync(file, 'utf8').trim().split('\n')
    .map(line => { const equal = line.indexOf('=');
      return [line.slice(0, equal), line.slice(equal + 1)]; }));
}
const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
async function waitFor(predicate, description) {
  const deadline = Date.now() + 15000;
  while (Date.now() < deadline) {
    const value = predicate();
    if (value) return value;
    await wait(30);
  }
  throw new Error(`timed out waiting for ${description}`);
}
async function captureMetadata() {
  const before = captured().count;
  const now = new Date();
  fs.utimesSync(project, now, now);
  return waitFor(() => { const record = captured();
    return record?.count > before ? record : null; }, 'directory metadata capture');
}
async function main() {
  try {
    session = command(['gremlin', 'start', '--dir', project, '--lane', lane,
      '--target', 'test_provenance', '--campaign-seconds', '60']).session_id;
    const first = await waitFor(captured, 'initial capture');
    const original = identity(first.generation);
    assert.equal(original.base_commit, initialHead, 'base commit matches independent Git');
    assert.ok(original.toolchain.endsWith(':PROVENANCE_COMPILER_ORACLE'),
      'toolchain records only the compiler version probe');
    assert.equal(git(['diff', '--binary', 'HEAD']), '', 'fixture starts with an empty diff');
    for (let i = 0; i < 2; i++) {
      const next = await captureMetadata();
      assert.equal(next.generation, first.generation,
        'unchanged inputs retain the same generation across metadata events');
      assert.equal(identity(next.generation).patch_digest, original.patch_digest,
        'unchanged Git diff retains its digest');
    }
    git(['commit', '--allow-empty', '-qm', 'metadata-only commit']);
    const newHead = git(['rev-parse', 'HEAD']);
    assert.notEqual(newHead, initialHead, 'independent Git observes the new commit');
    const latest = await captureMetadata();
    const refreshed = identity(latest.generation);
    assert.equal(refreshed.base_commit, newHead, 'fresh probe records the new Git HEAD');
    assert.equal(refreshed.patch_digest, original.patch_digest,
      'empty metadata commit leaves the source diff digest unchanged');
    const probes = fs.readFileSync(calls, 'utf8').trim().split('\n').map(JSON.parse)
      .filter(call => call.cwd === project);
    assert.ok(probes.length >= latest.count, 'each capture probes the compiler');
    assert.ok(probes.every(call => JSON.stringify(call.args) === '["--version"]'),
      'Git arguments never leak into compiler execution');
    console.log('Gremlin context provenance: correct Git HEAD; stable repeated captures');
  } finally {
    if (session) command(['gremlin', 'stop', '--dir', project, '--lane', lane,
      '--session', session, '--json']);
  }
}
main().catch(error => {
  console.error(error.stack || error);
  console.error(`fixture retained at ${scratch}`);
  process.exitCode = 1;
});
