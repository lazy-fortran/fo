#!/usr/bin/env python3
"""Native CMake preset/runtime oracle for Fo's resident metadata discovery."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

root = Path(sys.argv[2]).resolve()
root.mkdir()
project = root / 'project'
project.mkdir()
driver = str(Path(sys.argv[1]).resolve())
parent = '--expect-parent-failure' in sys.argv
(project / 'CMakeLists.txt').write_text("""cmake_minimum_required(VERSION 3.20)
project(preset_locator LANGUAGES Fortran)
enable_testing()
add_executable(marker main.f90)
add_test(NAME native_payload COMMAND marker)
""")
(project / 'main.f90').write_text("""program main
include 'value.inc'
if (value /= 17) error stop 'independent payload expected 17'
print *, 'payload17'
end program
""")
leaf = project / 'value.inc'
leaf.write_text('integer, parameter :: value=17\n')
(project / 'CMakePresets.json').write_text(json.dumps({
    'version': 3, 'configurePresets': [
        {'name': 'base', 'hidden': True, 'generator': 'Ninja',
         'binaryDir': '${sourceDir}/build' if '--flat' in sys.argv else
                      '${sourceDir}/build/native',
         'cacheVariables': {'CMAKE_BUILD_TYPE': 'Release'}},
        {'name': 'cpu', 'inherits': 'base'}],
    'buildPresets': [{'name': 'cpu', 'configurePreset': 'cpu'}],
    'testPresets': [{'name': 'cpu', 'configurePreset': 'cpu'}]})+'\n')
env = dict(os.environ, FO_CMAKE_CONFIGURE_PRESET='cpu',
           FO_CMAKE_BUILD_PRESET='cpu', FO_CMAKE_TEST_PRESET='cpu',
           FO_DISABLE_SELF_REFRESH='1', FO_JOBS='2',
           XDG_CACHE_HOME=str(root / 'cache'), TMPDIR=str(root))
env.pop('FO_CMAKE_BUILD_DIR', None)
rows = []

def run(args, phase):
    p = subprocess.run(args, cwd=project, env=env, capture_output=True,
                       text=True, timeout=45)
    (root / (phase + '.log')).write_text(p.stdout+p.stderr)
    return p

for args in [['cmake', '--preset', 'cpu'], ['cmake', '--build', '--preset', 'cpu'],
             ['ctest', '--preset', 'cpu', '--output-on-failure']]:
    p = run(args, 'native-'+str(len(rows)))
    rows.append({'phase': 'native', 'command': args, 'exit': p.returncode})
    assert p.returncode == 0
p = run([driver, 'gremlin', 'start', '--dir', str(project), '--lane', 'preset',
         '--target', 'native_payload', '--random', '0'], 'start')
assert p.returncode == 0
session = json.loads(p.stdout)['session_id']
base = [driver, 'gremlin', 'status', '--dir', str(project), '--lane', 'preset']
original = leaf.read_bytes()
st = leaf.stat()
generation = ''
try:
    for phase in ['cold', 'warm', 'edit', 'restore']:
        if phase == 'edit':
            leaf.write_bytes(original.replace(b'17', b'23'))
        if phase == 'restore':
            leaf.write_bytes(original)
        if phase in ['edit', 'restore']:
            os.utime(leaf, ns=(st.st_atime_ns, st.st_mtime_ns))
        deadline = time.monotonic()+60
        x = {}
        while time.monotonic() < deadline:
            p = run(base, phase+'-status')
            x = json.loads(p.stdout)
            if parent and p.returncode != 0:
                break
            failure = x.get('latest_failure') or {}
            if phase == 'edit':
                ready = (x.get('active_generation') != generation and
                         failure.get('status') == 'FAIL' and
                         failure.get('case_id') == 'native_payload' and
                         failure.get('generation') == x.get('active_generation'))
            else:
                ready = x.get('local_gate_green', False)
            if ready:
                break
            time.sleep(0.2)
        rows.append({'phase': phase, 'exit': p.returncode,
                     'status': x, 'mtime_preserved': leaf.stat().st_mtime_ns == st.st_mtime_ns})
        if parent:
            assert p.returncode != 0
            break
        assert ready, (phase, x)
        generation = x['active_generation']
finally:
    run([driver, 'gremlin', 'stop', '--dir', str(project), '--lane', 'preset',
         '--session', session], 'stop')
    leaf.write_bytes(original)
    os.utime(leaf, ns=(st.st_atime_ns, st.st_mtime_ns))
    (root / 'receipt.json').write_text(json.dumps({'driver': driver,
        'driver_sha256': hashlib.sha256(Path(driver).read_bytes()).hexdigest(),
        'session_id': session, 'rows': rows}, indent=2)+'\n')
print(json.dumps([{'phase': r['phase'], 'exit': r['exit']} for r in rows]))
