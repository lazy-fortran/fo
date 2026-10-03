#!/usr/bin/env node
// Ensure JSON-RPC _meta data cannot shadow params or tool arguments.
const assert = require('node:assert/strict');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const driver = process.argv[2] || process.env.FO;
if (!driver) throw new Error('pass the combined fo path as the first argument');
const executable = path.resolve(driver);
const scratch = fs.mkdtempSync('/var/tmp/fo-mcp-request-structure-');
const project = path.join(scratch, 'project');
const env = { ...process.env, HOME: path.join(scratch, 'home'), TMPDIR: '/var/tmp',
  FO_CACHE_DIR: path.join(scratch, 'cache') };
fs.mkdirSync(project, { recursive: true });
fs.mkdirSync(env.HOME, { recursive: true });
fs.writeFileSync(path.join(project, 'fpm.toml'), 'name = "mcp_request_probe"\n');

function startServer() {
  const child = spawn(executable, ['mcp-server'], { cwd: project, env,
    stdio: ['pipe', 'pipe', 'pipe'] });
  let buffer = '';
  let stderr = '';
  const pending = [];
  child.stdout.on('data', chunk => {
    buffer += chunk.toString();
    let newline;
    while (pending.length && (newline = buffer.indexOf('\n')) >= 0) {
      const raw = buffer.slice(0, newline);
      buffer = buffer.slice(newline + 1);
      const waiter = pending.shift();
      clearTimeout(waiter.timer);
      try { waiter.resolve(JSON.parse(raw)); } catch (error) { waiter.reject(error); }
    }
  });
  child.stderr.on('data', chunk => { stderr += chunk.toString(); });
  function send(message) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error(`MCP timeout: ${stderr}`)), 30000);
      pending.push({ resolve, reject, timer });
      child.stdin.write(JSON.stringify(message) + '\n');
    });
  }
  function waitForExit() {
    if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve();
    return new Promise(resolve => child.once('exit', resolve));
  }
  return { child, send, waitForExit };
}

function toolResult(response) {
  assert.ok(response.result && response.result.content, JSON.stringify(response));
  return { payload: JSON.parse(response.result.content[0].text),
    isError: response.result.isError };
}

async function main() {
  const server = startServer();
  try {
    // Put params before method and _meta before arguments. A substring scan
    // would dispatch tools/list, then gremlin_stop, instead of gremlin_status.
    const metaMethod = await server.send({ jsonrpc: '2.0', id: 1,
      params: { name: 'fo', 'arguments ': { action: 'gremlin_stop' },
        _meta: { method: 'tools/list' },
        arguments: { action: 'gremlin_status', dir: project, lane_id: 'safe' } },
      'method ': 'tools/list', method: 'tools/call' });
    assert.equal(toolResult(metaMethod).payload.action, 'status',
      'top-level method and actual arguments control dispatch');

    const metaAction = await server.send({ jsonrpc: '2.0', id: 2,
      method: 'tools/call', params: { name: 'fo', _meta: {
        action: 'gremlin_stop', arguments: { action: 'gremlin_stop',
          session_id: 'metadata-session' } },
      arguments: { action: 'gremlin_status', dir: project, lane_id: 'safe' } } });
    assert.equal(toolResult(metaAction).payload.action, 'status',
      '_meta action and arguments cannot replace params.arguments');

    const tools = await server.send({ jsonrpc: '2.0', id: 3, method: 'tools/list' });
    assert.equal(tools.result.tools[0].inputSchema.properties.background, undefined,
      'background is not advertised when the shared core rejects it');

    const escapedKey = await server.send({ jsonrpc: '2.0', id: 4,
      method: 'tools/call', params: { name: 'fo', arguments: {
        action: 'gremlin_status', dir: project, lane_id: 'safe', 'odd"key': 1,
      } } });
    const escapedResult = toolResult(escapedKey);
    assert.equal(escapedResult.isError, true);
    assert.match(escapedResult.payload.error, /unsupported Gremlin request field/,
      'escaped unknown key reaches strict core validation as valid JSON');

    const whitespaceKey = await server.send({ jsonrpc: '2.0', id: 41,
      method: 'tools/call', params: { name: 'fo', arguments: {
        action: 'gremlin_status', dir: project, 'lane_id ': 'safe',
      } } });
    const whitespaceResult = toolResult(whitespaceKey);
    assert.equal(whitespaceResult.isError, true);
    assert.match(whitespaceResult.payload.error, /field/,
      'normalization preserves trailing spaces in unknown property names');

    const paddedAction = await server.send({ jsonrpc: '2.0', id: 42,
      method: 'tools/call', params: { name: 'fo', arguments: {
        action: 'gremlin_status ', dir: project, lane_id: 'safe',
      } } });
    const paddedActionResult = toolResult(paddedAction);
    assert.equal(paddedActionResult.isError, true);
    assert.match(paddedActionResult.payload.error, /exact public name/,
      'MCP action mapping preserves exact public names');

    const falseBackground = await server.send({ jsonrpc: '2.0', id: 5,
      method: 'tools/call', params: { name: 'fo', arguments: {
        action: 'gremlin_start', dir: project, lane_id: 'safe', background: false,
      } } });
    const rejectedBackground = toolResult(falseBackground);
    assert.equal(rejectedBackground.isError, true);
    assert.match(rejectedBackground.payload.error, /background/);

    const cli = spawnSync(executable, ['gremlin', 'start', '--dir', project,
      '--lane', 'safe', '--background'], { cwd: project, env, encoding: 'utf8',
      timeout: 10000 });
    assert.notEqual(cli.status, 0, 'CLI background flag is rejected by the same core');
    console.log('mcp-request-structure: exact envelope/arguments, escaped keys, and background rejection passed');
  } finally {
    if (server.child.exitCode === null && server.child.signalCode === null) {
      try {
        await server.send({ jsonrpc: '2.0', id: 99, method: 'shutdown' });
      } catch (_) { /* process exit is checked below */ }
      const timer = setTimeout(() => server.child.kill('SIGTERM'), 5000);
      await server.waitForExit();
      clearTimeout(timer);
    }
    fs.rmSync(scratch, { recursive: true, force: true });
  }
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
