#!/usr/bin/env node
// Verify self-build refresh opt-out and the default refresh behavior.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const driver = process.argv[2] || process.env.FO;
if (!driver) throw new Error('pass the newly built fo path as the first argument');
const scratch = fs.mkdtempSync('/var/tmp/fo-self-refresh-');
const home = path.join(scratch, 'home');
const installed = path.join(home, '.local', 'bin', 'fo');
fs.mkdirSync(path.dirname(installed), { recursive: true });

function build(extraEnv = {}) {
  const env = { ...process.env, HOME: home, TMPDIR: '/var/tmp',
    FO_CACHE_DIR: path.join(scratch, 'cache'), ...extraEnv };
  return spawnSync(driver, ['build'], { cwd: project, env, encoding: 'utf8',
    timeout: 180000, maxBuffer: 8 * 1024 * 1024 });
}

try {
  fs.writeFileSync(installed, 'preserve-with-opt-out');
  const disabled = build({ FO_DISABLE_SELF_REFRESH: '1' });
  assert.equal(disabled.status, 0, disabled.stdout + disabled.stderr);
  assert.equal(fs.readFileSync(installed, 'utf8'), 'preserve-with-opt-out',
    'FO_DISABLE_SELF_REFRESH=1 leaves the installed path untouched');

  fs.writeFileSync(installed, 'default-refresh-sentinel');
  const normal = build();
  assert.equal(normal.status, 0, normal.stdout + normal.stderr);
  const refreshed = fs.readFileSync(installed);
  assert.ok(refreshed.length > 100000, 'default successful self-build refreshes fo');
  assert.notEqual(refreshed.toString('utf8'), 'default-refresh-sentinel');
  console.log('gremlin-self-refresh: opt-out preserves target; default refreshes it');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
