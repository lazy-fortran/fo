#!/usr/bin/env node
// Distinct executable sources may legally share their private PROGRAM name.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-program-names-');
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '4' }
};

function run(args) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', scratch, 'fo', ...args];
  const result = spawnSync(driver, command, { ...options, cwd: installed ? scratch : project });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

try {
  if (!installed) {
    const built = spawnSync(driver, ['build'], { ...options, cwd: project });
    assert.equal(built.status, 0, built.stdout + built.stderr);
  }
  fs.writeFileSync(path.join(scratch, 'fpm.toml'), 'name = "program_names_probe"\n');
  fs.mkdirSync(path.join(scratch, 'test'));
  const names = ['test_first', 'test_second', 'test_third'];
  for (const [i, name] of names.entries()) {
    fs.writeFileSync(path.join(scratch, 'test', `support_${i + 1}.f90`), [
      `module support_${i + 1}`, 'implicit none', 'contains',
      'integer function value()', `value = ${i + 1}`, 'end function value',
      `end module support_${i + 1}`, ''
    ].join('\n'));
    fs.writeFileSync(path.join(scratch, 'test', `${name}.f90`), [
      'program private_name', `use support_${i + 1}, only: value`,
      'implicit none', 'integer :: unit',
      `open(newunit=unit, file='${name}.receipt', status='replace')`,
      "write(unit, '(i0)') value()", 'close(unit)', 'end program private_name', ''
    ].join('\n'));
  }
  for (let pass = 0; pass < 2; pass++) {
    for (const name of names) fs.rmSync(path.join(scratch, `${name}.receipt`), { force: true });
    const report = JSON.parse(run(['test', '--all', '--json']));
    assert.deepEqual(report.tests.map(test => test.name).sort(), [...names].sort());
    for (const test of report.tests) assert.equal(test.status, 'pass');
    for (const [i, name] of names.entries()) {
      assert.equal(fs.readFileSync(path.join(scratch, `${name}.receipt`), 'utf8'), `${i + 1}\n`);
    }
  }
  for (const name of names) {
    const report = JSON.parse(run(['test', name, '--json']));
    assert.deepEqual(report.tests.map(test => test.name), [name]);
    assert.equal(report.tests[0].status, 'pass');
  }
  fs.mkdirSync(path.join(scratch, 'src'));
  fs.mkdirSync(path.join(scratch, 'app'));
  const provider = resultType => [
    'module app_support', 'implicit none', 'contains',
    `${resultType} function value()`,
    resultType === 'integer' ? 'value = 1' : 'value = 7.0',
    'end function value', 'end module app_support', ''
  ].join('\n');
  fs.writeFileSync(path.join(scratch, 'src/app_support.f90'), provider('integer'));
  const apps = ['first_app', 'second_app'];
  for (const name of apps) {
    fs.writeFileSync(path.join(scratch, 'app', `${name}.f90`), [
      'program private_app_name', 'use app_support, only: value', 'implicit none',
      "print '(i0)', int(value())", 'end program private_app_name', ''
    ].join('\n'));
    assert.equal(run(['exec', name]), '1\n', `${name}: original provider interface`);
  }
  fs.writeFileSync(path.join(scratch, 'src/app_support.f90'), provider('real'));
  for (const name of apps) {
    assert.equal(run(['exec', name]), '7\n', `${name}: updated provider interface recompiles caller`);
  }
  console.log('program-names-cli: three distinct public tests sharing one PROGRAM name run cold, warm and named');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
