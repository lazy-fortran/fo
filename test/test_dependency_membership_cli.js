#!/usr/bin/env node
// New uncommitted source modules in a symlinked path dependency are build inputs.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-dependency-membership-');
const consumer = path.join(scratch, 'consumer');
const dependency = path.join(scratch, 'dependency-real');
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '4' }
};

function write(file, contents) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, contents);
}

function run(args) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', consumer, 'fo', ...args];
  const result = spawnSync(driver, command, { ...options, cwd: installed ? consumer : project });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

function provider(value, useHelper = false) {
  return ['module provider', useHelper ? 'use added_context, only: base' : '',
    'implicit none', 'contains', 'integer function current_value()',
    `current_value = ${useHelper ? 'base()' : value}`, 'end function current_value',
    'end module provider', ''].join('\n');
}

function expectValue(expected) {
  assert.equal(run(['exec', 'probe']), `${expected}\n`, 'application reads current dependency behavior');
  fs.rmSync(path.join(consumer, 'probe.receipt'), { force: true });
  const report = JSON.parse(run(['test', '--all', '--json']));
  assert.deepEqual(report.tests.map(test => test.name), ['test_probe']);
  assert.equal(report.tests[0].status, 'pass');
  assert.equal(fs.readFileSync(path.join(consumer, 'probe.receipt'), 'utf8'), `${expected}\n`);
}

try {
  if (!installed) {
    const built = spawnSync(driver, ['build'], { ...options, cwd: project });
    assert.equal(built.status, 0, built.stdout + built.stderr);
  }
  fs.mkdirSync(dependency);
  fs.symlinkSync(dependency, path.join(scratch, 'dependency'), 'dir');
  write(path.join(dependency, 'fpm.toml'), 'name = "provider_library"\n');
  write(path.join(dependency, 'src/provider.f90'), provider(2));
  write(path.join(consumer, 'fpm.toml'), [
    'name = "membership_probe"', '[dependencies]',
    'provider_library = { path = "../dependency" }', '[extra.fo]',
    'link = "shared"', 'pic = "true"', ''
  ].join('\n'));
  write(path.join(consumer, 'app/probe.f90'), [
    'program probe', 'use provider, only: current_value', 'implicit none',
    "print '(i0)', current_value()", 'end program probe', ''
  ].join('\n'));
  write(path.join(consumer, 'test/test_probe.f90'), [
    'program test_probe', 'use provider, only: current_value',
    'implicit none', 'integer :: unit',
    "open(newunit=unit, file='probe.receipt', status='replace')",
    "write(unit, '(i0)') current_value()", 'close(unit)', 'end program test_probe', ''
  ].join('\n'));
  expectValue(2);
  expectValue(2);
  write(path.join(dependency, 'src/semantic/analyzers/added_context.f90'), [
    'module added_context', 'implicit none', 'contains',
    'integer function base()', 'base = 5', 'end function base',
    'end module added_context', ''
  ].join('\n'));
  write(path.join(dependency, 'src/provider.f90'), provider(5, true));
  expectValue(5);
  expectValue(5);
  console.log('dependency-membership-cli: uncommitted module added under symlinked dependency is built and used cold/warm');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
