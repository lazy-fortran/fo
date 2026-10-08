#!/usr/bin/env python3
"""Native public Fo oracle; pass exact driver and new evidence directory."""
from pathlib import Path
import json
import os
import subprocess
import sys

root = Path(sys.argv[2]).resolve()
root.mkdir()
project = root / 'project'
project.mkdir()
files = {
    'fpm.toml': '''name="explicit_test_roots"
[build]
auto-tests=false
[dev-dependencies]
provider={path="provider"}
[[test]]
name="nested-alpha"
source-dir="test/alpha"
main="main.f90"
[[test]]
name="outside-beta"
source-dir="checks/beta"
main="main.f90"
''',
    'provider/fpm.toml': 'name="provider"\n',
    'provider/src/provider.f90': '''module provider
implicit none
contains
integer function provider_value()
provider_value=5
end function
end module
''',
    'src/library.f90': '''module library
implicit none
contains
integer function library_value()
library_value=3
end function
end module
''',
    'test/alpha/helper.f90': '''module helper
implicit none
contains
integer function helper_value()
helper_value=7
end function
end module
''',
    'test/alpha/unregistered.f90': '''program unregistered
implicit none
error stop "unregistered program must not run"
end program
''',
}
for label, dirname in [('alpha', 'test/alpha'), ('beta', 'checks/beta')]:
    helper_use = 'use helper, only: helper_value' if label == 'alpha' else ''
    helper_value = 'helper_value()' if label == 'alpha' else '7'
    files[dirname + '/main.f90'] = f'''program oracle_{label}
use library, only: library_value
use provider, only: provider_value
{helper_use}
implicit none
integer :: value
value=library_value()+provider_value()+{helper_value}
print *, "{label} native payload", value
if(value/=15)error stop "incorrect native payload"
end program
'''
for rel, text in files.items():
    p = project / rel
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text)
driver = str(Path(sys.argv[1]).resolve())
env = dict(os.environ, FO_BACKEND='gfortran', FO_JOBS='8',
           FO_DISABLE_SELF_REFRESH='1', TMPDIR=str(root))
rows = []
helper = project / 'test/alpha/helper.f90'
original = helper.read_bytes()
stat = helper.stat()
for phase in ['cold', 'warm', 'edit', 'restore', 'unknown']:
    if phase == 'edit':
        helper.write_bytes(original.replace(b'helper_value=7', b'helper_value=11'))
        os.utime(helper, ns=(stat.st_atime_ns, stat.st_mtime_ns))
    if phase == 'restore':
        helper.write_bytes(original)
        os.utime(helper, ns=(stat.st_atime_ns, stat.st_mtime_ns))
    names = ['missing-case'] if phase == 'unknown' else ['nested-alpha', 'outside-beta']
    command = [driver, 'test', '--json'] + names
    result = subprocess.run(command, cwd=project, env=env,
                            capture_output=True, text=True)
    output = result.stdout + result.stderr
    (root / (phase + '.log')).write_text(output)
    row = {'phase': phase, 'exit': result.returncode, 'command': command,
           'output': output, 'helper_mtime_preserved': helper.stat().st_mtime_ns == stat.st_mtime_ns}
    rows.append(row)
(root / 'result.json').write_text(json.dumps(rows, indent=2) + '\n')
print(json.dumps([{'phase': r['phase'], 'exit': r['exit']} for r in rows]))
# A caller may use a known-broken parent to retain a negative receipt.
if '--expect-parent-failure' not in sys.argv:
    assert all(r['exit'] == 0 for r in rows if r['phase'] in ['cold', 'warm', 'restore'])
    assert all(r['exit'] != 0 for r in rows if r['phase'] in ['edit', 'unknown'])
    assert all(r['helper_mtime_preserved'] for r in rows)
