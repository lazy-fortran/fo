#!/usr/bin/env python3
"""Independent signed test-dependency payload; explicit native FPM reference."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

root = Path(sys.argv[2]).resolve()
root.mkdir()
project = root/'project'
project.mkdir()
driver = str(Path(sys.argv[1]).resolve())
fpm = str(Path(sys.argv[3]).resolve())
parent = '--expect-parent-failure' in sys.argv
files = {
'fpm.toml': '''name="target_dependencies"
[build]
auto-tests=false
[[test]]
name="scoped-suite"
source-dir="checks"
main="check.f90"
[test.dependencies]
provider.path="provider"
''',
'src/library.f90': '''module library
implicit none
integer, parameter :: base=5
end module
''',
'app/main.f90': '''program main
use library, only: base
implicit none
if(base/=5)error stop 'ordinary library oracle5'
print *, 'ordinary5'
end program
''',
'checks/check.f90': '''program check
use library, only: base
use provider, only: answer
implicit none
if(base+answer/=23)error stop 'signed test dependency oracle23'
print *, 'test23'
end program
''',
'provider/fpm.toml': '''name="provider"
[dependencies]
leaf={path="../leaf"}
''',
'provider/src/provider.f90': '''module provider
use leaf, only: leaf_value
implicit none
integer, parameter :: answer=-11+leaf_value
end module
''',
'leaf/fpm.toml': 'name="leaf"\n',
'leaf/src/leaf.f90': '''module leaf
implicit none
integer, parameter :: leaf_value=29
end module
'''
}
for name,text in files.items():
    p=project/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(text)
env=dict(os.environ, FO_DISABLE_SELF_REFRESH='1',FO_JOBS='4',TMPDIR=str(root),
         XDG_CACHE_HOME=str(root/'cache'))
for k in ('FO_BACKEND','FO_CMAKE_ARGS','FO_CMAKE_CONFIGURE_PRESET',
          'FO_CMAKE_BUILD_PRESET','FO_CMAKE_TEST_PRESET'):
    env.pop(k,None)
rows=[]

def run(args,label):
    p=subprocess.run(args,cwd=project,env=env,capture_output=True,text=True,timeout=60)
    (root/(label+'.log')).write_text(p.stdout+p.stderr)
    rows.append({'phase':label,'command':args,'exit':p.returncode,
                 'output':p.stdout+p.stderr})
    return p

assert run([fpm,'run','--profile','debug'],'native-app').returncode==0
assert run([fpm,'test','--profile','debug','--target','scoped-suite'],'native-test').returncode==0
leaf=project/'leaf/src/leaf.f90'
original=leaf.read_bytes();st=leaf.stat()
for phase in ['cold','warm','edit','restore']:
    if phase=='edit':leaf.write_bytes(original.replace(b'=29',b'=31'))
    if phase=='restore':leaf.write_bytes(original)
    if phase in ('edit','restore'):os.utime(leaf,ns=(st.st_atime_ns,st.st_mtime_ns))
    if phase in ('edit','restore'):
        native=run([fpm,'test','--profile','debug','--target','scoped-suite'],'native-'+phase)
        assert (native.returncode!=0) if phase=='edit' else (native.returncode==0)
    p=run([driver,'test','--json','scoped-suite'],phase)
    if parent:
        assert p.returncode!=0
        break
    assert (p.returncode!=0) if phase=='edit' else (p.returncode==0), rows[-1]
    assert leaf.stat().st_mtime_ns==st.st_mtime_ns
if not parent:
    manifest=project/'fpm.toml'
    text=manifest.read_text()
    manifest.write_text(text.replace('provider.path="provider"','provider={path="provider"}'))
    assert run([driver,'test','--json','scoped-suite'],'inline').returncode==0
    manifest.write_text('[test.dependencies]\nprovider={path="provider"}\n')
    assert run([driver,'test','--json','scoped-suite'],'orphan-rejected').returncode!=0
    manifest.write_text(text)
(root/'receipt.json').write_text(json.dumps({'driver':driver,
    'driver_sha256':hashlib.sha256(Path(driver).read_bytes()).hexdigest(),
    'reference_fpm':fpm,'reference_fpm_sha256':hashlib.sha256(Path(fpm).read_bytes()).hexdigest(),
    'rows':rows},indent=2)+'\n')
print(json.dumps([{'phase':r['phase'],'exit':r['exit']}for r in rows]))
