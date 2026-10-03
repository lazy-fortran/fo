#!/usr/bin/env node
// Run: node test/test_cmake_named_cli.js [/path/to/fo]
// Prove that a CTest ID runs exactly one registered case through `fo test`.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const driver = process.argv[2] || process.env.FO || 'fo';
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'fo-cmake-named-'));
const selected = 'solovev_axis_regularity';
const neighbor = `${selected}_extra`;
const selectedReceipt = path.join(scratch, 'build', `${selected}.ran`);
const neighborReceipt = path.join(scratch, 'build', `${neighbor}.ran`);

function run(args) {
  const result = spawnSync(driver, args, {
    cwd: scratch,
    encoding: 'utf8',
    maxBuffer: 1024 * 1024,
    env: { ...process.env, FO_BACKEND: 'cmake', FO_JOBS: '2' }
  });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  return result;
}

try {
  const cmake = [
    'cmake_minimum_required(VERSION 3.20)',
    'project(fo_cmake_named_cli NONE)',
    'enable_testing()',
    `add_test(NAME ${selected} COMMAND "\${CMAKE_COMMAND}" -E touch "\${CMAKE_BINARY_DIR}/${selected}.ran")`,
    `add_test(NAME ${neighbor} COMMAND "\${CMAKE_COMMAND}" -E touch "\${CMAKE_BINARY_DIR}/${neighbor}.ran")`,
    ''
  ];
  fs.writeFileSync(path.join(scratch, 'CMakeLists.txt'), cmake.join('\n'));

  const named = run(['test', selected, '--json']);
  assert.equal(named.status, 0, named.stdout + named.stderr);
  const report = JSON.parse(named.stdout);
  assert.deepEqual(report.tests.map(test => test.name), [selected],
    'the report retains only the requested CTest ID');
  assert.equal(report.tests[0].status, 'pass');
  assert.equal(fs.existsSync(selectedReceipt), true,
    'the requested CTest case executes');
  assert.equal(fs.existsSync(neighborReceipt), false,
    'a similarly named CTest case does not execute');

  fs.rmSync(selectedReceipt, { force: true });
  const missing = run(['test', 'not_a_registered_ctest_id', '--json']);
  assert.notEqual(missing.status, 0,
    'an unknown CTest ID fails instead of succeeding with zero tests');
  assert.equal(fs.existsSync(selectedReceipt), false,
    'an exact no-match does not run a registered case');
  assert.equal(fs.existsSync(neighborReceipt), false,
    'an exact no-match does not run a similarly named case');

  console.log('cmake-named-cli: exact CTest ID ran alone; no-match failed');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
