#!/usr/bin/env node
// Real links must preserve dependency state and resolve external static archives.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-platform-link-');
const dependency = path.join(scratch, 'dependency');
const consumer = path.join(scratch, 'consumer');
const external = path.join(scratch, 'external');
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '4',
    LIBRARY_PATH: [external, process.env.LIBRARY_PATH].filter(Boolean).join(':') }
};

function command(executable, args, cwd) {
  const result = spawnSync(executable, args, { ...options, cwd });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

function write(file, contents) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, contents);
}

function run(args) {
  return installed ? command(driver, args, consumer) :
    command(driver, ['exec', '--no-build', '--cwd', consumer, 'fo', ...args], project);
}

try {
  if (!installed) command(driver, ['build'], project);
  write(path.join(dependency, 'fpm.toml'), 'name = "depstate"\n');
  write(path.join(dependency, 'src/dep_state.f90'), [
    'module dep_state', 'implicit none', 'integer :: stored = 7',
    'end module dep_state', ''
  ].join('\n'));
  command('git', ['init', '--quiet', '--initial-branch=main'], dependency);
  command('git', ['add', 'fpm.toml', 'src/dep_state.f90'], dependency);
  command('git', ['-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
    '-c', 'commit.gpgsign=false', 'commit', '--quiet', '-m', 'Dependency fixture'], dependency);
  write(path.join(external, 'probe.c'), 'int probe_value(void) { return 42; }\n');
  command('cc', ['-fPIC', '-c', 'probe.c', '-o', 'probe.o'], external);
  command('ar', ['rcs', 'libplatform_probe.a', 'probe.o'], external);
  write(path.join(consumer, 'src/provider.f90'), [
    'module provider', 'use dep_state, only: stored',
    'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none', 'interface',
    'integer(c_int) function probe_value() bind(c)', 'import c_int',
    'end function probe_value', 'end interface', 'contains',
    'subroutine update()', 'stored = probe_value()', 'end subroutine update',
    'end module provider', ''
  ].join('\n'));
  const body = [
    'use dep_state, only: stored', 'use provider, only: update', 'implicit none',
    'call update()', 'if (stored /= 42) stop 1', "print '(i0)', stored"
  ];
  write(path.join(consumer, 'app/probe.f90'), [
    'program probe', ...body, 'end program probe', ''
  ].join('\n'));
  write(path.join(consumer, 'test/test_probe.f90'), [
    'program test_probe', ...body, 'end program test_probe', ''
  ].join('\n'));
  for (const mode of ['static', 'shared']) {
    write(path.join(consumer, 'fpm.toml'), [
      'name = "platform_link_probe"', '[dependencies]',
      `depstate = { git = "${pathToFileURL(dependency).href}", branch = "main" }`,
      '[build]', 'link = ["platform_probe"]', '[extra.fo]',
      `link = "${mode}"`, 'pic = "true"', ''
    ].join('\n'));
    for (let warm = 0; warm < 2; warm++) {
      assert.equal(run(['exec', 'probe']), '42\n', `${mode} application uses dependency state`);
      const report = JSON.parse(run(['test', '--all', '--json']));
      assert.deepEqual(report.tests.map(test => [test.name, test.status]), [['test_probe', 'pass']]);
    }
  }
  if (process.platform === 'darwin') {
    const libraryDir = path.join(consumer, 'build/fo/lib');
    const profileImages = [];
    for (const profile of ['profile-one', 'profile-two']) {
      const marker = path.join(scratch, profile);
      const report = JSON.parse(run(['test', '--all', '--json', '--flag', `-Wl,-rpath,${marker}`]));
      assert.equal(report.tests[0].status, 'pass');
      const image = fs.readdirSync(libraryDir).filter(file => file.endsWith('.dylib')).find(file =>
        command('otool', ['-l', path.join(libraryDir, file)], consumer).includes(`path ${marker} (`));
      assert(image, 'requested profile rpath is present in the native shared image');
      profileImages.push(image);
    }
    assert.notEqual(profileImages[0], profileImages[1], 'profiles retain separate native shared images');
  }
  console.log('platform-link-cli: static/shared cold/warm links preserve dependency state and static archive calls');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
