#!/usr/bin/env node
// Run: node test/test_exec_app.js [/path/to/installed/fo]
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'fo-exec-app-'));
const options = { encoding: 'utf8', maxBuffer: 8 * 1024 * 1024 };

function run(cwd, args) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', cwd, 'fo', ...args];
  const result = spawnSync(driver, command, { ...options, cwd: installed ? cwd : project });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  return result;
}

function write(file, text) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, text);
}

function fixture(name, manifest, source, target, poison = true, extraSources = {}) {
  const dir = path.join(scratch, name);
  write(path.join(dir, 'fpm.toml'), manifest);
  for (const [file, text] of Object.entries(extraSources)) write(path.join(dir, file), text);
  if (poison) {
    write(path.join(dir, 'test/broken.f90'), [
      'program broken', 'implicit none', 'print *, missing_symbol', 'end program broken', ''
    ].join('\n'));
  }
  const file = path.join(dir, source);
  let previous;
  for (const marker of ['first', 'first', 'updated_longer_value']) {
    if (marker !== previous) {
      write(file, [
        'program internal_name', 'implicit none', `print '(a)', '${marker}'`,
        'end program internal_name', ''
      ].join('\n'));
      previous = marker;
    }
    const result = run(dir, ['exec', target]);
    assert.equal(result.status, 0, `${name}: ${result.stdout}${result.stderr}`);
    assert.equal(result.stdout.trim(), marker, `${name}: executes current source`);
  }
  console.log(`exec-app: ${name} cold, warm, and changed source pass`);
  return dir;
}

try {
  if (!installed) {
    const build = spawnSync(driver, ['build'], { ...options, cwd: project });
    assert.equal(build.status, 0, build.stdout + build.stderr);
  }
  const broken = fixture('package_main', 'name = "test_package"\n',
    'app/main.f90', 'test_package');
  fixture('discovered_app', 'name = "discovery"\n',
    'app/test_auto.f90', 'test_auto');
  fixture('explicit_app', [
    'name = "explicit_app"', '[build]', 'auto-executables = false',
    '[[executable]]', 'name = "test_public_app"', 'source-dir = "app"',
    'main = "launcher.f90"', ''
  ].join('\n'), 'app/launcher.f90', 'test_public_app');
  fixture('example_main', 'name = "different_package"\n',
    'example/main.f90', 'main');
  fixture('nested_example', 'name = "nested_package"\n',
    'example/demo/demo.f90', 'demo');
  fixture('explicit_test', [
    'name = "explicit_test"', '[[test]]', 'name = "smoke"',
    'source-dir = "test"', 'main = "test_check.f90"', ''
  ].join('\n'), 'test/test_check.f90', 'smoke', false);
  const application = [
    'program application', 'implicit none', "print '(a)', 'application'",
    'end program application', ''
  ].join('\n');
  fixture('explicit_collision', [
    'name = "collision"', '[[executable]]', 'name = "shared"',
    'source-dir = "app"', 'main = "launcher.f90"', '[[test]]', 'name = "shared"',
    'source-dir = "test"', 'main = "check.f90"', ''
  ].join('\n'), 'test/check.f90', 'shared', false, { 'app/launcher.f90': application });
  fixture('discovered_collision', 'name = "collision"\n',
    'test/shared.f90', 'shared', false, { 'app/shared.f90': application });

  const failed = run(broken, ['test', 'broken']);
  assert.equal(failed.status, 1, 'unrelated test must actually fail compilation');
  console.log('exec-app: explicitly requested broken test fails');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
