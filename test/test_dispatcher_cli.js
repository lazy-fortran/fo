#!/usr/bin/env node
// Run: node test/test_dispatcher_cli.js [/path/to/installed/fo]
// The fixture writes independent execution receipts and rejects every wrong argv.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-dispatcher-cli-');
const allCases = ['test_alpha', 'test_beta', 'test_legacy', 'test_plain'];
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '4' }
};

function run(args, environment = {}) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', scratch, 'fo', ...args];
  const result = spawnSync(driver, command, {
    ...options, env: { ...options.env, ...environment },
    cwd: installed ? scratch : project
  });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  return result;
}

function write(file, contents) {
  const target = path.join(scratch, file);
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.writeFileSync(target, contents);
}

function caseSource(name, value) {
  return [
    '! fo: dispatcher', `module ${name}_case`, 'implicit none', 'contains',
    'subroutine run_case()', 'integer :: unit',
    `open(newunit=unit, file='${name}.receipt', status='replace')`,
    `write(unit, '(a)') '${value}'`, 'close(unit)', 'end subroutine run_case',
    `end module ${name}_case`, ''
  ].join('\n');
}

function expectCases(args, names, receipts, environment = {}) {
  for (const name of allCases) {
    fs.rmSync(path.join(scratch, `${name}.receipt`), { force: true });
  }
  const result = run([...args, '--json'], environment);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  const report = JSON.parse(result.stdout);
  assert.deepEqual(report.tests.map(entry => entry.name).sort(), [...names].sort());
  for (const entry of report.tests) assert.equal(entry.status, 'pass', entry.name);
  assert.equal(report.summary.failed, 0);
  for (const [name, expected] of Object.entries(receipts)) {
    assert.equal(fs.readFileSync(path.join(scratch, `${name}.receipt`), 'utf8').trim(), expected);
  }
  for (const name of allCases) {
    if (!(name in receipts)) assert.equal(fs.existsSync(path.join(scratch, `${name}.receipt`)), false);
  }
}

function expectRejected(name, environment = {}) {
  const result = run(['test', name, '--json'], environment);
  assert.notEqual(result.status, 0, `${name} must not succeed with zero tests`);
  assert.match(result.stderr, new RegExp(`fo: unknown test: ${name}`));
}

function expectSlowRejected(names) {
  for (const name of allCases) {
    fs.rmSync(path.join(scratch, `${name}.receipt`), { force: true });
  }
  const result = run(['test', ...names, '--json']);
  assert.notEqual(result.status, 0, 'explicit slow selection needs --all');
  assert.match(result.stderr, /fo: slow test test_probe_slow requires --all/);
  assert.doesNotMatch(result.stderr, /unknown test/);
  for (const name of allCases) {
    assert.equal(fs.existsSync(path.join(scratch, `${name}.receipt`)), false,
      'reject the entire selection before running any case');
  }
}

function expectEmptyRandomRejected(options = []) {
  const result = run(['test', '--random', '3', '--seed', '42', ...options, '--json']);
  assert.notEqual(result.status, 0, 'an empty random selection must fail');
  assert.match(result.stderr, /fo: no eligible tests for --random/);
  if (!options.includes('--all')) assert.match(result.stderr, /use --all/);
  if (result.stdout.trim()) {
    const report = JSON.parse(result.stdout);
    assert.notEqual(report.exit_code, 0, 'no successful empty JSON report');
  }
  for (const name of allCases) {
    assert.equal(fs.existsSync(path.join(scratch, `${name}.receipt`)), false);
  }
}

try {
  if (!installed) {
    const built = spawnSync(driver, ['build'], { ...options, cwd: project });
    assert.equal(built.status, 0, built.stdout + built.stderr);
  }
  write('fpm.toml', [
    'name = "dispatcher_probe"', '[extra.fo]', 'dispatcher = "test_dispatcher"',
    'link = "shared"', 'pic = "true"',
    '[[test]]', 'name = "test_dispatcher"', 'source-dir = "test"',
    'main = "suite_entry.f90"', '[[test]]', 'name = "test_beta"',
    'source-dir = "test"', 'main = "nested/beta_source.f90"',
    '[[test]]', 'name = "test_module_only"', 'source-dir = "test"',
    'main = "support.f90"', ''
  ].join('\n'));
  write('test/test_alpha.f90', caseSource('test_alpha', 'alpha-original'));
  write('test/nested/beta_source.f90', caseSource('test_beta', 'beta-original'));
  write('test/support.f90', caseSource('test_legacy', 'legacy-dispatched')
    .replace('! fo: dispatcher\n', ''));
  write('test/test_flat_module.f90', 'module test_flat_module\nend module\n');
  write('test/test_flat_source.f90', 'module flat_helper\nend module\n');
  write('test/test_legacy.f90', [
    '! fo: dispatcher', 'program test_legacy', 'implicit none',
    'stop 89', 'end program test_legacy', ''
  ].join('\n'));
  write('test/suite_entry.f90', [
    'program suite_entry',
    'use test_alpha_case, only: alpha => run_case',
    'use test_beta_case, only: beta => run_case',
    'use test_legacy_case, only: legacy => run_case',
    'implicit none', 'character(len=128) :: name',
    'if(command_argument_count() /= 1) stop 3',
    'call get_command_argument(1, name)', 'select case(trim(name))',
    "case('test_alpha')", 'call alpha()', "case('test_beta')", 'call beta()',
    "case('test_legacy')", 'call legacy()',
    'case default', 'stop 4', 'end select', 'end program suite_entry', ''
  ].join('\n'));
  write('test/test_plain.f90', [
    'program test_plain', 'implicit none', 'integer :: unit',
    "open(newunit=unit, file='test_plain.receipt', status='replace')",
    "write(unit, '(a)') 'plain-original'", 'close(unit)', 'end program test_plain', ''
  ].join('\n'));
  const names = allCases;
  const receipts = {
    test_alpha: 'alpha-original', test_beta: 'beta-original',
    test_legacy: 'legacy-dispatched', test_plain: 'plain-original'
  };
  expectCases(['test', '--all'], names, receipts);
  expectCases(['test', '--all'], names, receipts);
  expectCases(['test', '--random', '4', '--seed', '42'], names, receipts);
  expectCases(['test', '--only-changed'], names, receipts);
  expectCases(['test', 'test_alpha'], ['test_alpha'], { test_alpha: 'alpha-original' });
  expectCases(['test', 'test_beta'], ['test_beta'], { test_beta: 'beta-original' });
  expectRejected('test_module_only');
  expectRejected('test_flat_module');
  expectRejected('test_flat_source');
  expectRejected('test_does_not_exist');
  const explicit = run(['test', 'test_dispatcher', '--json']);
  assert.notEqual(explicit.status, 0, 'explicit dispatcher runs with no implicit self argument');
  const explicitReport = JSON.parse(explicit.stdout);
  assert.equal(explicitReport.tests.length, 1);
  assert.equal(explicitReport.tests[0].name, 'test_dispatcher');
  assert.equal(explicitReport.exit_code, 3);
  write('test/test_alpha.f90', caseSource('test_alpha', 'alpha-updated'));
  expectCases(['test', 'test_alpha'], ['test_alpha'], { test_alpha: 'alpha-updated' });
  expectCases(['test', '--all'], names, { ...receipts, test_alpha: 'alpha-updated' });
  const binaries = fs.readdirSync(path.join(scratch, 'build/fo/bin')).sort();
  assert.deepEqual(binaries, ['test_dispatcher', 'test_plain'], 'one routed binary, one independent binary');
  // Regex scanning retains marker-only sources without a build-unit identity.
  // A real dispatcher must not turn such a source into a named test target.
  const markerAlias = [
    '[[test]]', 'name = "test_marker_only"', 'source-dir = "test"',
    'main = "nested/marker_only.f90"', ''
  ].join('\n');
  fs.appendFileSync(path.join(scratch, 'fpm.toml'), markerAlias);
  write('test/nested/marker_only.f90', '! fo: dispatcher\n! no build unit\n');
  const regexScan = { FO_SCAN_FALLBACK: 'regex' };
  expectRejected('test_marker_only', regexScan);
  expectCases(['test', '--all'], names, { ...receipts, test_alpha: 'alpha-updated' }, regexScan);
  expectCases(['test', '--all'], names, { ...receipts, test_alpha: 'alpha-updated' }, regexScan);
  fs.rmSync(path.join(scratch, 'test/nested/marker_only.f90'));

  // A custom test root must use the same manifest mapping and eligibility.
  fs.renameSync(path.join(scratch, 'test'), path.join(scratch, 'checks'));
  const manifest = fs.readFileSync(path.join(scratch, 'fpm.toml'), 'utf8')
    .replaceAll('source-dir = "test"', 'source-dir = "checks"');
  write('fpm.toml', `${manifest}\n[build]\ntest-dir = "checks"\n`);
  expectCases(['test', 'test_beta'], ['test_beta'], { test_beta: 'beta-original' });
  expectCases(['test', '--all'], names, { ...receipts, test_alpha: 'alpha-updated' });
  expectRejected('test_module_only');
  expectRejected('test_flat_module');
  expectRejected('test_flat_source');

  // The configured dispatcher source now exists only as a module. Its name
  // and marker cannot substitute for an actual dispatcher program.
  write('checks/suite_entry.f90', '! fo: dispatcher\nmodule suite_entry\nend module\n');
  fs.rmSync(path.join(scratch, 'checks/test_legacy.f90'));
  expectRejected('test_beta');
  expectRejected('test_alpha');
  expectRejected('test_dispatcher');
  expectCases(['test', '--all'], ['test_plain'], { test_plain: 'plain-original' });
  expectCases(['test', '--all'], ['test_plain'], { test_plain: 'plain-original' });
  expectCases(['test', '--random', '1', '--seed', '42'], ['test_plain'],
    { test_plain: 'plain-original' });
  expectCases(['test', '--only-changed'], ['test_plain'], { test_plain: 'plain-original' });
  // Also exercise an absent source, with the manifest string still configured.
  fs.rmSync(path.join(scratch, 'checks/suite_entry.f90'));
  expectRejected('test_beta');
  expectRejected('test_does_not_exist');
  // Slow classification uses the public alias, not the program/source name.
  const slow = 'test_probe_slow';
  allCases.push(slow);
  fs.appendFileSync(path.join(scratch, 'fpm.toml'), [
    '[[test]]', `name = "${slow}"`, 'source-dir = "checks"',
    'main = "nested/slow_entry.f90"', ''
  ].join('\n'));
  write('checks/nested/slow_entry.f90', [
    'program slow_entry', 'implicit none', 'integer :: unit',
    `open(newunit=unit, file='${slow}.receipt', status='replace')`,
    "write(unit, '(a)') 'slow-executed'", 'close(unit)', 'end program', ''
  ].join('\n'));
  expectSlowRejected([slow]);
  expectSlowRejected(['test_plain', slow]);
  expectSlowRejected([slow, 'test_plain']);
  expectCases(['test', '--all', slow], [slow], { [slow]: 'slow-executed' });
  expectSlowRejected([slow]);
  expectCases(['test', '--all', 'test_plain', slow], ['test_plain', slow],
    { test_plain: 'plain-original', [slow]: 'slow-executed' });
  expectCases(['test'], ['test_plain'], { test_plain: 'plain-original' });
  // Only the slow program remains runnable: random must not become an
  // unfiltered zero-name backend request when default selection excludes it.
  fs.rmSync(path.join(scratch, 'checks/test_plain.f90'));
  for (const name of allCases) {
    fs.rmSync(path.join(scratch, `${name}.receipt`), { force: true });
  }
  expectEmptyRandomRejected();
  expectCases(['test', '--all', '--random', '3', '--seed', '42'], [slow],
    { [slow]: 'slow-executed' });
  expectCases(['test', '--all', '--random', '3', '--seed', '42'], [slow],
    { [slow]: 'slow-executed' });
  fs.rmSync(path.join(scratch, 'checks/nested/slow_entry.f90'));
  fs.rmSync(path.join(scratch, `${slow}.receipt`));
  expectEmptyRandomRejected();
  expectEmptyRandomRejected(['--all']);
  console.log('dispatcher-cli: shared cold/warm all, module/program cases, public aliases, ' +
    'nested/custom roots, random/changed selection, named edits, explicit self and ' +
    'ineligible/marker-only/missing-dispatcher rejection, explicit slow and empty random gates pass');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
