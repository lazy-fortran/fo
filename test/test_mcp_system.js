#!/usr/bin/env node
// System test for fo MCP server — exercises the full JSON-RPC/stdio protocol.
// Tests both Content-Length framing and bare JSON line framing.
// Run: node test/test_mcp_system.js [/path/to/fo]

const { spawn } = require('child_process');
const path = require('path');

const FO = process.argv[2] || path.join(__dirname, '..', 'build',
  'gfortran_4308FCC5A9F59874', 'app', 'fo');

let passed = 0;
let failed = 0;

function assert(cond, msg) {
  if (cond) { passed++; process.stdout.write('  ok: ' + msg + '\n'); }
  else { failed++; process.stdout.write('  FAIL: ' + msg + '\n'); }
}

function startServer(cwd) {
  const proc = spawn(FO, ['mcp-server'], {
    stdio: ['pipe', 'pipe', 'pipe'],
    cwd: cwd || process.cwd()
  });
  let stderr = '';
  proc.stderr.on('data', (d) => { stderr += d.toString(); });
  return { proc, getStderr: () => stderr };
}

function sendFramed(proc, obj) {
  const data = JSON.stringify(obj);
  proc.stdin.write('Content-Length: ' + Buffer.byteLength(data) + '\r\n\r\n' + data);
}

function sendBare(proc, obj) {
  proc.stdin.write(JSON.stringify(obj) + '\n');
}

function readFramedResponse(proc, timeout) {
  timeout = timeout || 10000;
  return new Promise((resolve, reject) => {
    let buf = Buffer.alloc(0);
    let contentLength = null;
    let headerEnd = -1;

    const timer = setTimeout(() => {
      proc.stdout.removeListener('data', onData);
      reject(new Error('timeout waiting for framed response'));
    }, timeout);

    function onData(chunk) {
      buf = Buffer.concat([buf, chunk]);
      if (contentLength === null) {
        headerEnd = buf.indexOf('\r\n\r\n');
        if (headerEnd === -1) return;
        const header = buf.slice(0, headerEnd).toString();
        const m = header.match(/Content-Length:\s*(\d+)/i);
        if (!m) { clearTimeout(timer); reject(new Error('bad header: ' + header)); return; }
        contentLength = parseInt(m[1]);
      }
      const bodyStart = headerEnd + 4;
      if (buf.length >= bodyStart + contentLength) {
        clearTimeout(timer);
        proc.stdout.removeListener('data', onData);
        resolve(JSON.parse(buf.slice(bodyStart, bodyStart + contentLength).toString()));
      }
    }
    proc.stdout.on('data', onData);
  });
}

function readBareResponse(proc, timeout) {
  timeout = timeout || 10000;
  return new Promise((resolve, reject) => {
    let buf = '';
    const timer = setTimeout(() => {
      proc.stdout.removeListener('data', onData);
      reject(new Error('timeout waiting for bare response'));
    }, timeout);

    function onData(chunk) {
      buf += chunk.toString();
      const nl = buf.indexOf('\n');
      if (nl >= 0) {
        clearTimeout(timer);
        proc.stdout.removeListener('data', onData);
        resolve(JSON.parse(buf.slice(0, nl)));
      }
    }
    proc.stdout.on('data', onData);
  });
}

async function runSuite(label, send, readResponse) {
  process.stdout.write('\n--- ' + label + ' ---\n');
  const srv = startServer();

  try {
    process.stdout.write('initialize:\n');
    send(srv.proc, {
      jsonrpc: '2.0', id: 1, method: 'initialize',
      params: { protocolVersion: '2025-11-25', capabilities: {},
                clientInfo: { name: 'test', version: '1.0' } }
    });
    const init = await readResponse(srv.proc);
    assert(init.jsonrpc === '2.0', 'jsonrpc version');
    assert(init.id === 1, 'id matches');
    assert(init.result.protocolVersion === '2025-11-25', 'echoes protocol version');
    assert(init.result.serverInfo.name === 'fo', 'server name');
    assert(init.result.capabilities.tools !== undefined, 'has tools capability');
    assert(init.result.capabilities.resources !== undefined, 'has resources capability');

    process.stdout.write('initialized notification:\n');
    send(srv.proc, { jsonrpc: '2.0', method: 'initialized' });
    send(srv.proc, { jsonrpc: '2.0', id: 99, method: 'tools/list', params: {} });
    const alive = await readResponse(srv.proc);
    assert(alive.id === 99, 'server alive after initialized notification');

    process.stdout.write('tools/list:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 2, method: 'tools/list', params: {} });
    const tools = await readResponse(srv.proc);
    assert(Array.isArray(tools.result.tools), 'tools is array');
    const fo = tools.result.tools.find(t => t.name === 'fo');
    assert(fo !== undefined, 'fo tool exists');
    assert(fo.inputSchema.properties.action.enum.includes('check'), 'has check action');
    assert(fo.inputSchema.properties.action.enum.includes('info'), 'has info action');
    assert(fo.inputSchema.required.includes('action'), 'action is required');

    process.stdout.write('resources/list:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 3, method: 'resources/list', params: {} });
    const res = await readResponse(srv.proc);
    assert(Array.isArray(res.result.resources), 'resources is array');
    const diag = res.result.resources.find(r => r.uri === 'fo://diagnostics');
    assert(diag !== undefined, 'diagnostics resource exists');

    process.stdout.write('tools/call info:\n');
    send(srv.proc, {
      jsonrpc: '2.0', id: 4, method: 'tools/call',
      params: { name: 'fo', arguments: { action: 'info' } }
    });
    const info = await readResponse(srv.proc);
    assert(info.result.isError === false, 'info not error');
    assert(info.result.content[0].text.includes('backend:'), 'info has backend');

    process.stdout.write('unknown method:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 5, method: 'bogus/method', params: {} });
    const unk = await readResponse(srv.proc);
    assert(unk.error !== undefined, 'unknown method returns error');
    assert(unk.error.code === -32601, 'method not found code');

    process.stdout.write('unknown action:\n');
    send(srv.proc, {
      jsonrpc: '2.0', id: 6, method: 'tools/call',
      params: { name: 'fo', arguments: { action: 'nonexistent' } }
    });
    const unkAct = await readResponse(srv.proc);
    assert(unkAct.error !== undefined, 'unknown action returns error');

    process.stdout.write('stderr:\n');
    assert(srv.getStderr() === '', 'no stderr output');

    process.stdout.write('shutdown:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 7, method: 'shutdown', params: {} });
    const shut = await readResponse(srv.proc);
    assert(shut.result === null, 'shutdown returns null');
    await new Promise((resolve) => {
      srv.proc.on('exit', (code) => {
        assert(code === 0, 'clean exit after shutdown');
        resolve();
      });
      setTimeout(() => {
        assert(false, 'server exited within timeout');
        srv.proc.kill();
        resolve();
      }, 5000);
    });
  } catch (e) {
    failed++;
    process.stdout.write('  FAIL: exception: ' + e.message + '\n');
    srv.proc.kill();
  }
}

async function run() {
  process.stdout.write('fo MCP system test\n');
  process.stdout.write('binary: ' + FO + '\n');

  await runSuite('Content-Length framing', sendFramed, readFramedResponse);
  await runSuite('bare JSON line framing', sendBare, readBareResponse);

  process.stdout.write('\n' + passed + ' passed, ' + failed + ' failed\n');
  process.exit(failed > 0 ? 1 : 0);
}

run();
