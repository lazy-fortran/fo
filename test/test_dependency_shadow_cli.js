#!/usr/bin/env node
// A root Git dependency wins over the same package reached transitively by path.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-dependency-shadow-');
const external = path.join(scratch, 'fortfront');
const shadow = path.join(scratch, 'fortfront-path-copy');
const fx = path.join(scratch, 'fx');
const consumer = path.join(scratch, 'consumer');
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: {
    ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '2',
    FO_CACHE_DIR: path.join(scratch, 'cache'), FO_SELF_REFRESH: '0',
    FO_DISABLE_SELF_REFRESH: '1'
  }
};

function write(file, contents) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, contents);
}

function command(binary, args, cwd) {
  const result = spawnSync(binary, args, { ...options, cwd });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

try {
  write(path.join(external, 'fpm.toml'), [
    'name = "fortfront_probe"', 'version = "0.1.0"', ''
  ].join('\n'));
  write(path.join(external, 'src/fortfront_probe.f90'), [
    'module fortfront_probe', 'contains', 'integer function fortfront_value()',
    'fortfront_value = 17', 'end function fortfront_value',
    'end module fortfront_probe', ''
  ].join('\n'));
  command('git', ['init', '-q', '-b', 'main'], external);
  command('git', ['add', 'fpm.toml', 'src/fortfront_probe.f90'], external);
  command('git', ['-c', 'user.name=Probe', '-c', 'user.email=probe@example.invalid',
    'commit', '-q', '-m', 'fixture'], external);

  write(path.join(shadow, 'fpm.toml'), [
    'name = "fortfront_probe"', 'version = "0.1.0"', ''
  ].join('\n'));
  write(path.join(shadow, 'src/fortfront_probe.f90'), [
    'module fortfront_probe', 'contains', 'integer function fortfront_value()',
    'fortfront_value = 99', 'end function fortfront_value',
    'end module fortfront_probe', ''
  ].join('\n'));
  write(path.join(fx, 'fpm.toml'), [
    'name = "fx_probe"', 'version = "0.1.0"', '[dependencies]',
    'fortfront_probe = { path = "../fortfront-path-copy" }', ''
  ].join('\n'));
  write(path.join(fx, 'src/fx_probe.f90'), [
    'module fx_probe', 'use fortfront_probe, only: fortfront_value', 'contains',
    'integer function fx_value()', 'fx_value = fortfront_value()',
    'end function fx_value', 'end module fx_probe', ''
  ].join('\n'));
  const gitDep = `fortfront_probe = { git = "file://${external}", branch = "main" }`;
  write(path.join(consumer, 'fpm.toml'), [
    'name = "dependency_shadow_probe"', 'version = "0.1.0"', '[dependencies]',
    'fx_probe = { path = "../fx" }', gitDep, ''
  ].join('\n'));
  write(path.join(consumer, 'src/consumer.f90'), [
    'module consumer', 'use fx_probe, only: fx_value', 'contains',
    'integer function consumer_value()', 'consumer_value = fx_value()',
    'end function consumer_value', 'end module consumer', ''
  ].join('\n'));
  write(path.join(consumer, 'test/test_probe.f90'), [
    'program test_probe', 'use consumer, only: consumer_value',
    'implicit none', 'integer :: unit',
    "open(newunit=unit, file='consumer.receipt', status='replace')",
    "write(unit, '(i0)') consumer_value()", 'close(unit)',
    'end program test_probe', ''
  ].join('\n'));

  command('fpm', ['build'], consumer);
  command(driver, ['test', 'test_probe'], consumer);
  assert.equal(fs.readFileSync(path.join(consumer, 'consumer.receipt'), 'utf8'),
    '17\n', 'root Git provider wins over the distinct path implementation');
  console.log('dependency-shadow-cli: root Git implementation wins');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
