import json,os,subprocess,time,hashlib,sys
from pathlib import Path
root=Path(sys.argv[2]).resolve();root.mkdir()
parent='--expect-parent-failure' in sys.argv
fo=str(Path(sys.argv[1]).resolve())
rows=[]
for case,key,rogue in [('unsupported','PAYLOAD_SOURCE',False),('supported','PAYLOAD_SOURCE_DIR',False),('sibling-rejected','PAYLOAD_SOURCE_DIR',True),('external-alias-rejected','PAYLOAD_SOURCE_DIR',False)]:
 work=root/case;project=work/'project';project.mkdir(parents=True)
 provider=work/'payload';provider.mkdir();(provider/'value.f90').write_text('module payload\ninteger,parameter :: value=19\nend module\n')
 sibling=work/'sibling';sibling.mkdir();(sibling/'unused.f90').write_text('module unused\ninteger,parameter :: value=41\nend module\n')
 (project/'main.f90').write_text('program main\nuse payload\nif(value/=19)error stop "oracle19"\nend program\n')
 extra=' "${CMAKE_CURRENT_SOURCE_DIR}/../sibling/unused.f90"' if rogue else ''
 if case=='external-alias-rejected':
  (project/'alias.f90').symlink_to('../sibling/unused.f90')
  extra=' alias.f90'
 (project/'CMakeLists.txt').write_text(f'''cmake_minimum_required(VERSION 3.24)
project(root_contract LANGUAGES Fortran)
include(CTest)
set({key} "${{CMAKE_CURRENT_SOURCE_DIR}}/../payload" CACHE PATH "Source input")
add_executable(oracle main.f90 "${{{key}}}/value.f90"{extra})
add_test(NAME oracle COMMAND oracle)
''')
 env=dict(os.environ,FO_DISABLE_SELF_REFRESH='1',FO_JOBS='1',XDG_CACHE_HOME=str(work/'cache'),TMPDIR=str(work))
 for n in list(env):
  if n=='FO_BACKEND' or n.startswith('FO_CMAKE_'):env.pop(n)
 def run(args,label):
  p=subprocess.run(args,cwd=project,env=env,capture_output=True,text=True,timeout=40)
  (work/(label+'.log')).write_text(p.stdout+p.stderr)
  return p
 for args,label in [(['cmake','-S','.','-B','build/reference'],'native-configure'),(['cmake','--build','build/reference'],'native-build'),(['ctest','--test-dir','build/reference','--output-on-failure'],'native-test')]:
  assert run(args,label).returncode==0
 lane='root-contract'
 start=run([fo,'gremlin','start','--dir',str(project),'--lane',lane,'--target','oracle','--random','0'],'start')
 assert start.returncode==0
 try:
  for i in range(80):
   status=run([fo,'gremlin','status','--dir',str(project),'--lane',lane],'status')
   j=json.loads(status.stdout)
   if j.get('state') in ('quiescent','build_failed') or status.returncode!=0:break
   time.sleep(.2)
  terminal=[]
  for p in (work/'cache/fo/gremlin/terminal').rglob('status'):
   terminal.append(json.loads(p.read_text()))
  row={'case':case,'status_exit':status.returncode,'status':j,'terminal':terminal}
  if case=='supported':
   if parent:assert j['state']=='build_failed' and not j['local_gate_green']
   else:assert j['state']=='quiescent' and j['local_gate_green']
   gen=j.get('active_generation') or j.get('candidate_generation')
   script=work/'cache/fo/gremlin/generations-v2'/gen/'bundle/project/.fo-cmake/cache.cmake'
   (work/'frozen-cache.cmake').write_bytes(script.read_bytes())
  else:
   assert status.returncode!=0
   prefix='referenced CMake input is missing or unsupported:' if case=='external-alias-rejected' else 'unsupported uncaptured external CMake input:'
   assert any(prefix in t.get('diagnostic','') for t in terminal)
   expected=provider if case=='unsupported' else (project/'alias.f90' if case=='external-alias-rejected' else sibling)
   assert any(str(expected) in t.get('diagnostic','') for t in terminal)
  rows.append(row)
 finally:run([fo,'gremlin','stop','--dir',str(project),'--lane',lane],'stop')
(root/'receipt.json').write_text(json.dumps({'driver':fo,'sha256':hashlib.sha256(Path(fo).read_bytes()).hexdigest(),'rows':rows},indent=2)+'\n')
print(json.dumps([{'case':r['case'],'status_exit':r['status_exit'],'state':r['status'].get('state')}for r in rows]))
