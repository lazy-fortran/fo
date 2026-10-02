#!/usr/bin/env node
// Run: node test/test_fmt_cli.js [/path/to/installed/fo]
const assert = require('node:assert/strict');
const { spawnSync } = require('node:child_process');
const { createHash } = require('node:crypto');
const fs = require('node:fs');
const path = require('node:path');

const project = path.resolve(__dirname, '..');
const installed = process.argv[2];
const driver = installed || process.env.FO || 'fo';
const scratch = fs.mkdtempSync('/var/tmp/fo-fmt-cli-');
const options = {
  encoding: 'utf8', maxBuffer: 8 * 1024 * 1024,
  env: { ...process.env, TMPDIR: '/var/tmp', FO_JOBS: '4' }
};

function run(args) {
  const command = installed ? args : ['exec', '--no-build', '--cwd', scratch, 'fo', ...args];
  const result = spawnSync(driver, command, { ...options, cwd: installed ? scratch : project });
  if (result.error) throw result.error;
  assert.equal(result.signal, null, result.stderr);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

function digest(name) {
  return createHash('md5').update(fs.readFileSync(path.join(scratch, 'build/fo/bin', name))).digest('hex');
}

try {
  if (!installed) {
    const built = spawnSync(driver, ['build'], { ...options, cwd: project });
    assert.equal(built.status, 0, built.stdout + built.stderr);
  }
  fs.writeFileSync(path.join(scratch, 'fpm.toml'), 'name = "format_probe"\n');
  fs.mkdirSync(path.join(scratch, 'app'));
  const identifiers = [
    'function', 'subroutine', 'helper_function', 'helper_subroutine',
    'myfunction', 'mysubroutine', 'a_function', 'a_subroutine',
    'value1function', 'value1subroutine', 'function_count', 'subroutine_count',
    'FUNCTION', 'SUBROUTINE', 'helper_FUNCTION', 'helper_SUBROUTINE',
    'prefix2_function', 'prefix2_subroutine', 'xfunction', 'xsubroutine'
  ];
  const rows = identifiers.map((identifier, i) => {
    const name = `probe_${i + 1}`;
    const source = [
      `program ${name}`, 'implicit none', `integer :: ${identifier}`,
      `${identifier} = ${i + 1}`, `print '(i0)', ${identifier}`, `end program ${name}`, ''
    ].join('\n');
    fs.writeFileSync(path.join(scratch, 'app', `${name}.f90`), source);
    return { name, source, expected: `${i + 1}\n`, closing: `end program ${name}` };
  });
  const typedName = 'typed_function';
  const typedSource = [
    `program ${typedName}`, 'implicit none', 'integer, parameter :: kind_function = 4',
    "print '(f4.1)', answer()", 'contains',
    'real(kind=kind_function ) function answer()', 'answer = 7.0',
    'end function answer', `end program ${typedName}`, ''
  ].join('\n');
  fs.writeFileSync(path.join(scratch, 'app', `${typedName}.f90`), typedSource);
  rows.push({ name: typedName, source: typedSource, expected: ' 7.0\n', closing: `end program ${typedName}` });
  for (let i = 1; i <= 20; i++) {
    const name = `literal_${i}`;
    const quote = i % 2 === 0 ? '"' : "'";
    const escaped = `${quote}${quote}`;
    const gap = i % 4 === 0 ? ['! between literal segments', ''] : [];
    const tail = i % 3 === 0 ? [
      `&right!case${i}&`, `&${escaped}quoted${quote}`
    ] : [`&right!case${i}${escaped}quoted${quote}`];
    const source = [
      `program ${name}`, 'implicit none', 'character(len=80) :: text',
      `text = ${quote}left &`, ...gap, ...tail,
      "print '(a)', trim(text)", `end program ${name}`, ''
    ].join('\n');
    fs.writeFileSync(path.join(scratch, 'app', `${name}.f90`), source);
    rows.push({ name, source, expected: `left right!case${i}${quote}quoted\n`,
      closing: `end program ${name}`, literal: true });
  }
  for (const row of rows) {
    assert.equal(run(['exec', row.name]), row.expected, `${row.name}: original executable`);
    row.before_md5 = digest(row.name);
  }
  run(['fmt']);
  for (const row of rows) {
    const formatted = fs.readFileSync(path.join(scratch, 'app', `${row.name}.f90`), 'utf8');
    const lines = formatted.trimEnd().split('\n');
    assert.equal(lines.at(-1), row.closing, `${row.name}: closing scope at column 1`);
    if (row.literal) {
      assert.equal(lines.at(-2), "    print '(a)', trim(text)",
        `${row.name}: literal continuation leaves following statement at one scope`);
    } else if (row.name === typedName) {
      assert.equal(lines[6], '        answer = 7.0', 'typed procedure body opens exactly one scope');
    } else {
      assert.equal(lines[4], `    ${row.source.split('\n')[4]}`, `${row.name}: assignment opens no scope`);
    }
    row.formatted = formatted;
    assert.equal(run(['exec', row.name]), row.expected, `${row.name}: formatted executable`);
    row.after_md5 = digest(row.name);
  }
  run(['fmt']);
  for (const row of rows) {
    assert.equal(fs.readFileSync(path.join(scratch, 'app', `${row.name}.f90`), 'utf8'), row.formatted,
      `${row.name}: second format is byte-identical`);
  }
  if (process.env.FO_FORMAT_REPORT) {
    fs.writeFileSync(process.env.FO_FORMAT_REPORT, JSON.stringify(rows.map(row => ({
      name: row.name, before_md5: row.before_md5, after_md5: row.after_md5,
      expected_stdout: row.expected
    })), null, 2) + '\n');
  }
  console.log(`fmt-cli: ${rows.length} executable outputs preserved; nesting and idempotence pass`);
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
