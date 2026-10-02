#!/usr/bin/env node
// A complete test report must survive JSON escaping and both MCP framings.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-test-json-');
const env = { ...process.env, TMPDIR: '/var/tmp', FO_CACHE_DIR: path.join(scratch, 'cache') };
const names = Array.from({ length: 397 }, (_, i) => `test_report_${i + 1}_${'x'.repeat(90)}`);
names.push('test_quoted_"name"', 'test_backslash_\\name', 'test_late_failure');

function start(framed) {
  const args = installed ? ['mcp-server']
    : ['exec', '--no-build', '--cwd', scratch, 'fo', 'mcp-server'];
  const proc = spawn(driver, args, { cwd: installed ? scratch : project, env });
  let buffer = Buffer.alloc(0);
  let stderr = '';
  const pending = [];
  proc.stderr.on('data', chunk => { stderr += chunk; });
  proc.stdout.on('data', chunk => {
    buffer = Buffer.concat([buffer, chunk]);
    while (pending.length) {
      let start = 0;
      let length;
      if (framed) {
        const end = buffer.indexOf('\r\n\r\n');
        if (end < 0) return;
        const header = /Content-Length:\s*(\d+)/i.exec(buffer.subarray(0, end).toString());
        assert.ok(header, 'MCP response carries Content-Length');
        start = end + 4;
        length = Number(header[1]);
        if (buffer.length < start + length) return;
      } else {
        length = buffer.indexOf('\n');
        if (length < 0) return;
      }
      const body = buffer.subarray(start, start + length).toString();
      buffer = buffer.subarray(start + length + (framed ? 0 : 1));
      const waiter = pending.shift();
      clearTimeout(waiter.timer);
      try { waiter.resolve(JSON.parse(body)); } catch (error) { waiter.reject(error); }
    }
  });
  function call(message) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      const data = JSON.stringify(message);
      proc.stdin.write(framed ? `Content-Length: ${Buffer.byteLength(data)}\r\n\r\n${data}`
        : `${data}\n`);
    });
  }
  return { proc, call };
}

async function verify(framed) {
  const server = start(framed);
  try {
    const init = await server.call({ jsonrpc: '2.0', id: 1, method: 'initialize',
      params: { protocolVersion: '2025-11-25', capabilities: {} } });
    assert.equal(init.result.protocolVersion, '2025-11-25');
    const response = await server.call({ jsonrpc: '2.0', id: 2, method: 'tools/call',
      params: { name: 'fo', arguments: { action: 'test', dir: scratch, json: 'full' } } });
    assert.equal(response.id, 2);
    assert.equal(response.result.isError, true, 'failing test retains the MCP failure state');
    const text = response.result.content[0].text;
    assert.ok(Buffer.byteLength(text) > 32768, 'report exceeds both former MCP limits');
    const report = JSON.parse(text);
    assert.deepEqual(report.tests.map(test => test.name).sort(), [...names].sort());
    for (const test of report.tests) {
      assert.equal(test.status, test.name === 'test_late_failure' ? 'fail' : 'pass');
      assert.equal(typeof test.seconds, 'number');
    }
    assert.equal(report.summary.passed, 399);
    assert.equal(report.summary.failed, 1);
    assert.notEqual(report.exit_code, 0);
    const action = 'unknown_action';
    const error = await server.call({ jsonrpc: '2.0', id: 3, method: 'tools/call',
      params: { name: 'fo', arguments: { action } } });
    assert.equal(error.error.code, -32602);
    assert.equal(error.error.message, `unknown action: ${action}`);
    const shutdown = await server.call({ jsonrpc: '2.0', id: 4, method: 'shutdown' });
    assert.equal(shutdown.result, null);
    console.log(`mcp-test-json: ${framed ? 'framed' : 'bare'}: ${report.tests.length} entries, ` +
      `${Buffer.byteLength(text)} bytes, late failure and escaped names retained`);
  } finally {
    server.proc.kill();
  }
}

(async () => {
  try {
    if (!installed) {
      const built = spawnSync(driver, ['build'], { cwd: project, encoding: 'utf8',
        env: { ...process.env, TMPDIR: '/var/tmp' }, maxBuffer: 8 * 1024 * 1024 });
      assert.equal(built.status, 0, built.stdout + built.stderr);
    }
    fs.writeFileSync(path.join(scratch, 'CMakeLists.txt'), [
      'cmake_minimum_required(VERSION 3.20)', 'project(fo_mcp_json_report NONE)',
      'enable_testing()', ...names.map((name, i) =>
        `add_test(NAME [=[${name}]=] COMMAND "\${CMAKE_COMMAND}" -E ` +
        `${i === names.length - 1 ? 'false' : 'true'})`), ''
    ].join('\n'));
    await verify(true);
    await verify(false);
  } finally {
    fs.rmSync(scratch, { recursive: true, force: true });
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
