#!/usr/bin/env node
// Sequential targeted builds must materialize each requested executable.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-targeted-materialization-');
const markerPath = `${scratch}.marker`;
const envScratch = `${scratch}.env`;
const envPaths = {
  HOME: path.join(envScratch, 'home'),
  XDG_CONFIG_HOME: path.join(envScratch, 'xdg-config'),
  XDG_CACHE_HOME: path.join(envScratch, 'xdg-cache'),
  FO_PREFIX: path.join(envScratch, 'prefix'),
  FO_CACHE_DIR: path.join(envScratch, 'fo-cache'),
  FO_GREMLIN_STATE_DIR: path.join(envScratch, 'gremlin-state')
};
for (const dir of Object.values(envPaths)) fs.mkdirSync(dir, { recursive: true });
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: {
    ...process.env, ...envPaths, TMPDIR: '/var/tmp', FO_JOBS: '2',
    FO_SELF_REFRESH: '0', FO_DISABLE_SELF_REFRESH: '1'
  }
};

function run(name) {
  const args = ['test', name];
  const result = spawnSync(driver, args, { ...options, cwd: scratch });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  assert.match(result.stdout, new RegExp(`${name}\\s+PASS`));
}

try {
  fs.writeFileSync(path.join(scratch, 'fpm.toml'), [
    'name = "targeted_materialization_probe"', 'version = "0.1.0"',
    'license = "MIT"', 'author = "test"', 'maintainer = "test"',
    'description = "target materialization probe"', '', '[build]',
    'test-dir = "test"', 'source-dir = "src"', 'app-dir = "app"', ''
  ].join('\n'));
  fs.mkdirSync(path.join(scratch, 'test'));
  fs.mkdirSync(path.join(scratch, 'src'));
  fs.mkdirSync(path.join(scratch, 'app'));
  for (const [name, marker] of [
    ['test_mcp_pass', 'first target ran'],
    ['test_mcp_fail', 'second target ran']
  ]) {
    fs.writeFileSync(path.join(scratch, 'test', `${name}.f90`), [
      `program ${name}`, 'implicit none', 'integer :: unit',
      `open(newunit=unit, file='${markerPath}', status='replace')`,
      `write(unit, '(a)') '${marker}'`, 'close(unit)',
      `end program ${name}`, ''
    ].join('\n'));
  }

  run('test_mcp_pass');
  const binDir = path.join(scratch, 'build/fo/bin');
  assert.equal(fs.existsSync(path.join(binDir, 'test_mcp_pass')), true);
  assert.equal(fs.existsSync(path.join(binDir, 'test_mcp_fail')), false);
  assert.equal(fs.readFileSync(markerPath, 'utf8').trim(), 'first target ran');
  run('test_mcp_fail');
  assert.equal(fs.readFileSync(markerPath, 'utf8').trim(),
    'second target ran');
  assert.equal(fs.existsSync(path.join(binDir, 'test_mcp_fail')), true);
  console.log('targeted-materialization-cli: sequential selected targets each build and run');
} finally {
  fs.rmSync(markerPath, { force: true });
  fs.rmSync(scratch, { recursive: true, force: true });
  fs.rmSync(envScratch, { recursive: true, force: true });
}
