#!/usr/bin/env node
// Named MCP selections must run an eligible case or return an error.
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const candidate = process.argv[2];
if (!candidate) throw new Error('pass the isolated fo executable as the first argument');
const executable = path.resolve(candidate);
if (executable === path.resolve('/home/ert/.local/bin/fo')) {
  throw new Error('pass the isolated candidate binary, not the global fo install');
}

const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-named-eligibility-');
const project = path.join(scratch, 'project');
const passName = 'test_mcp_named_pass';
const slowName = 'test_mcp_named_slow';
const moduleName = 'test_mcp_named_module_only';
const receipt = name => path.join(project, `${name}.receipt`);
const env = {
  ...process.env,
  HOME: path.join(scratch, 'home'),
  TMPDIR: '/var/tmp',
  FO_CACHE_DIR: path.join(scratch, 'cache'),
  FO_PREFIX: path.join(scratch, 'prefix'),
  FO_SELF_REFRESH: '0',
  FO_DISABLE_SELF_REFRESH: '1',
};

function writeTest(name, receiptPath) {
  const escapedPath = receiptPath.replaceAll("'", "''");
  fs.writeFileSync(path.join(project, 'test', `${name}.f90`), [
    `program ${name}`,
    'implicit none',
    'integer :: unit',
    `open(newunit=unit, file='${escapedPath}', status='replace', action='write')`,
    "write(unit, '(a)') 'executed'",
    'close(unit)',
    'end program',
    '',
  ].join('\n'));
}

function callNamedTest(name) {
  const requests = [
    { jsonrpc: '2.0', id: 1, method: 'initialize', params: {
      protocolVersion: '2025-11-25', capabilities: {},
    } },
    { jsonrpc: '2.0', id: 2, method: 'tools/call', params: {
      name: 'fo', arguments: { action: 'test', dir: project, args: [name], json: 'full' },
    } },
    { jsonrpc: '2.0', id: 3, method: 'shutdown' },
  ];
  const result = spawnSync(executable, ['mcp-server'], {
    cwd: project,
    env,
    encoding: 'utf8',
    input: `${requests.map(request => JSON.stringify(request)).join('\n')}\n`,
    maxBuffer: 8 * 1024 * 1024,
    timeout: 60000,
  });
  if (result.error) throw result.error;
  assert.equal(result.status, 0, result.stdout + result.stderr);
  const messages = result.stdout.trim().split('\n').map(line => JSON.parse(line));
  assert.equal(messages.length, 3, result.stdout);
  assert.equal(messages[0].result.protocolVersion, '2025-11-25');
  assert.equal(messages[2].result, null);
  assert.equal(messages[1].id, 2);
  return messages[1].result;
}

function expectRejected(name, expectedText) {
  fs.rmSync(receipt(name), { force: true });
  const response = callNamedTest(name);
  assert.equal(response.isError, true, `${name} must not succeed with zero tests`);
  assert.equal(response.content[0].text, expectedText);
  assert.equal(fs.existsSync(receipt(name)), false, `${name} must not execute`);
}

try {
  fs.mkdirSync(path.join(project, 'test'), { recursive: true });
  fs.mkdirSync(env.HOME, { recursive: true });
  fs.mkdirSync(env.FO_PREFIX, { recursive: true });
  fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "mcp_named_probe"\n');
  writeTest(passName, receipt(passName));
  writeTest(slowName, receipt(slowName));
  fs.writeFileSync(path.join(project, 'test', `${moduleName}.f90`), [
    'module mcp_named_module_support',
    'implicit none',
    'end module',
    '',
  ].join('\n'));

  const success = callNamedTest(passName);
  assert.equal(success.isError, false, success.content[0].text);
  const report = JSON.parse(success.content[0].text);
  assert.deepEqual(report.tests.map(test => test.name), [passName]);
  assert.equal(report.summary.passed, 1);
  assert.equal(report.summary.failed, 0);
  assert.equal(report.exit_code, 0);
  assert.equal(fs.readFileSync(receipt(passName), 'utf8').trim(), 'executed',
    'the positive case leaves an independent execution receipt');

  expectRejected('test_mcp_named_typo', 'fo: unknown test: test_mcp_named_typo');
  expectRejected(moduleName, `fo: unknown test: ${moduleName}`);
  expectRejected(slowName, `fo: slow test ${slowName} requires --all`);
  console.log('mcp-named-eligibility: positive execution receipt, typo, module-only, and slow rejection passed');
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
