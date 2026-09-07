#!/usr/bin/env node
// Run: node test/test_test_results_json.js
// Build fo, then exercise its CLI through fo exec with an independent JSON parser.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const driver = process.env.FO || 'fo';
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'fo-test-json-'));
const options = {
  cwd: project,
  encoding: 'utf8',
  maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, FO_TEST_TIMEOUT: '120' }
};

function run(args, inFixture = true) {
  // The source build below makes --no-build safe and avoids building other tests.
  const command = inFixture
    ? ['exec', '--no-build', '--cwd', scratch, 'fo', ...args] : args;
  const result = spawnSync(driver, command, options);
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  return result;
}

try {
  const built = run(['build'], false);
  assert.equal(built.status, 0, built.stdout + built.stderr);

  const names = Array.from({ length: 397 }, (_, i) => `test_report_${i + 1}`);
  names.push('test_quoted_"name"', 'test_backslash_\\name', 'test_late_failure');
  const cmake = [
    'cmake_minimum_required(VERSION 3.20)',
    'project(fo_json_report NONE)',
    'enable_testing()',
    ...names.map((name, i) =>
      `add_test(NAME [=[${name}]=] COMMAND "\${CMAKE_COMMAND}" -E ` +
      `${i === names.length - 1 ? 'false' : 'true'})`)
  ];
  fs.writeFileSync(path.join(scratch, 'CMakeLists.txt'), cmake.join('\n') + '\n');
  const configured = run(['build']);
  assert.equal(configured.status, 0, configured.stdout + configured.stderr);

  const result = run(['test', '--json']);
  assert.equal(result.status, 1, result.stdout + result.stderr);
  let report;
  try {
    report = JSON.parse(result.stdout);
  } catch (error) {
    throw new Error(`Invalid test JSON (${Buffer.byteLength(result.stdout)} bytes): ` +
      error.message);
  }
  assert.equal(report.tests.length, names.length, 'every result is retained');
  assert.deepEqual(report.tests.map(test => test.name).sort(), [...names].sort(),
    'all names survive, including JSON escapes and the late failure');
  for (const test of report.tests) {
    assert.equal(test.status, test.name === 'test_late_failure' ? 'fail' : 'pass');
    assert.equal(typeof test.seconds, 'number');
    assert.ok(test.seconds >= 0);
  }
  assert.equal(report.summary.passed, 399);
  assert.equal(report.summary.failed, 1);
  assert.equal(report.summary.skipped, 0);
  assert.ok(report.summary.total_seconds >= 0);
  assert.notEqual(report.exit_code, 0);
  assert.ok(Buffer.byteLength(result.stdout) > 16384, 'exercise the former limit');
  console.log(`test-results-json: ${report.tests.length} complete entries, ` +
    `${Buffer.byteLength(result.stdout)} bytes, late failure and escapes preserved`);
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
