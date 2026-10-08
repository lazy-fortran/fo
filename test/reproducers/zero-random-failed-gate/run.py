#!/usr/bin/env python3
"""Independent finite failed-gate oracle through native registered CTest."""
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
(project / 'CMakeLists.txt').write_text('''cmake_minimum_required(VERSION 3.20)
project(scope_oracle LANGUAGES Fortran)
enable_testing()
add_executable(gate main.f90)
add_executable(unbuilt invalid.f90)
add_test(NAME selected_red COMMAND gate)
add_test(NAME selected_companion COMMAND "${CMAKE_COMMAND}" -E echo COMPANION17)
add_test(NAME unrelated_sentinel COMMAND "${CMAKE_COMMAND}" -E echo OUTSIDE_SCOPE_RAN)
add_test(NAME unrelated_unbuilt COMMAND unbuilt)
''')
(project / 'main.f90').write_text('''program gate
include 'value.inc'
if(value /= 17) error stop 'oracle expects integer17'
print *, 'oracle17'
end program
''')
(project / 'invalid.f90').write_text('this unselected target must never compile\n')
leaf = project / 'value.inc'
leaf.write_text('integer, parameter :: value=23\n')
(project / 'CMakePresets.json').write_text(json.dumps({
    'version': 3, 'configurePresets': [{'name': 'cpu', 'generator': 'Ninja',
    'binaryDir': '${sourceDir}/build'}],
    'buildPresets': [{'name': 'cpu', 'configurePreset': 'cpu'}],
    'testPresets': [{'name': 'cpu', 'configurePreset': 'cpu'}]})+'\n')
env = dict(os.environ, FO_CMAKE_CONFIGURE_PRESET='cpu',
    FO_CMAKE_BUILD_PRESET='cpu', FO_CMAKE_TEST_PRESET='cpu',
    FO_CMAKE_BUILD_TARGETS='gate', FO_DISABLE_SELF_REFRESH='1', FO_JOBS='2',
    XDG_CACHE_HOME=str(root / 'cache'), TMPDIR=str(root))
for k in ('FO_CMAKE_BUILD_DIR', 'FO_BACKEND', 'FO_CMAKE_ARGS'):
    env.pop(k, None)
rows = []

def run(args, label):
    p = subprocess.run(args, cwd=project, env=env, capture_output=True,
                       text=True, timeout=40)
    (root / (label+'.log')).write_text(p.stdout+p.stderr)
    return p

for i,args in enumerate([['cmake','--preset','cpu'],
    ['cmake','--build','--preset','cpu','--target','gate'],
    ['ctest','--preset','cpu','-R','^selected_red$','--output-on-failure']]):
    p = run(args, 'native-'+str(i))
    assert (p.returncode != 0) if i == 2 else (p.returncode == 0)
p = run([driver,'gremlin','start','--dir',str(project),'--lane','scope',
    '--target','selected_red','--target','selected_companion','--random','0'], 'start')
assert p.returncode == 0
session = json.loads(p.stdout)['session_id']
base = [driver,'gremlin','status','--dir',str(project),'--lane','scope']

def case_rows():
    items = []
    for f in (root/'cache').rglob(session+'-case-*.log'):
        text=f.read_text()
        try: data=json.JSONDecoder().raw_decode(text)[0]
        except ValueError: continue
        for t in data.get('tests', []):
            items.append({'name':t['name'],'status':t['status'],'log':str(f),
                          'output':t.get('output','')})
    return items

try:
    for phase in (['red'] if parent else ['red','green','restored-red']):
        if phase == 'green': leaf.write_text('integer, parameter :: value=17\n')
        if phase == 'restored-red': leaf.write_text('integer, parameter :: value=23\n')
        deadline=time.monotonic()+40
        ready=False
        while time.monotonic()<deadline:
            p=run(base, phase+'-status');x=json.loads(p.stdout)
            cases=case_rows()
            unexpected=[t for t in cases if t['name'].startswith('unrelated_')]
            if parent:
                ready=bool(unexpected)
            elif phase == 'green':
                ready=x.get('state')=='quiescent' and x.get('local_gate_green',False)
            else:
                f=x.get('latest_failure') or {}
                ready=(x.get('state')=='quiescent' and not x.get('local_gate_green')
                    and f.get('case_id')=='selected_red'
                    and f.get('status')=='FAIL'
                    and f.get('generation')==x.get('active_generation'))
            if ready: break
            time.sleep(0.1)
        rows.append({'phase':phase,'status':x,'cases':cases})
        assert ready, (phase,x,cases)
        if not parent:
            assert not unexpected, unexpected
            assert any(t['name']=='selected_companion' and t['status']=='pass'
                       for t in cases), cases
            if phase != 'green':
                assert x.get('health')=='failure', x
            # A settled RED must remain finite and retain its failure.
            time.sleep(1)
            after=json.loads(run(base,phase+'-settled').stdout)
            assert after['completed']==x['completed'], (x,after)
finally:
    run([driver,'gremlin','stop','--dir',str(project),'--lane','scope',
         '--session',session],'stop')
    (root/'receipt.json').write_text(json.dumps({'driver':driver,
        'driver_sha256':hashlib.sha256(Path(driver).read_bytes()).hexdigest(),
        'session_id':session,'rows':rows},indent=2)+'\n')
print(json.dumps([{'phase':r['phase'],'state':r['status'].get('state'),
    'cases':[t['name'] for t in r['cases']]} for r in rows]))
