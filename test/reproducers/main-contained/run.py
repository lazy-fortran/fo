"""Exercise executable linking for main programs containing procedures.

Usage: python3 run.py /fixed/fo /native/fpm /new/output /parent/fo
"""
from pathlib import Path
import json
import os
import subprocess
import sys

fixed, fpm, root, parent = [Path(x).resolve() for x in sys.argv[1:5]]
root.mkdir(parents=True, exist_ok=False)
env = dict(os.environ, FO_BACKEND="fpm", FO_JOBS="1", OMP_NUM_THREADS="1")
results = []
source_templates = {
    "main_subroutine": "program main\ncall helper()\ncontains\nsubroutine helper()\ninterface\ninteger function external_payload()\nend function\nend interface\nprint *,external_payload()+DELTA\nend subroutine\nend program main\n",
    "main_function": "program main\nprint *,helper()\ncontains\ninteger function helper()\ninterface\ninteger function external_payload()\nend function\nend interface\nhelper=external_payload()+DELTA\nend function\nend program main\n",
    "implicit_main": "print *,helper()\ncontains\ninteger function helper()\ninterface\ninteger function external_payload()\nend function\nend interface\nhelper=external_payload()+DELTA\nend function\nend program\n",
    "same_line": "program main; print *,helper(); contains\ninteger function helper()\ninterface\ninteger function external_payload()\nend function\nend interface\nhelper=external_payload()+DELTA\nend function\nend program main\n",
    "named_control": "program ordinary\nprint *,helper()\ncontains\ninteger function helper()\ninterface\ninteger function external_payload()\nend function\nend interface\nhelper=external_payload()+DELTA\nend function\nend program ordinary\n",
}
test_source = '''program test_app_layout
implicit none
character(4096) :: path, tool, cmd, arg
integer :: slash, rc, u, value, expected
call get_command_argument(1,arg)
read(arg,*)expected
call get_command_argument(0,path)
slash=scan(trim(path),'/',back=.true.)
if(slash==0)error stop 'test executable path unavailable'
tool=path(:slash)//'../app/main_contained'
cmd='"'//trim(tool)//'" > layout_payload.tmp'
call execute_command_line(trim(cmd),exitstat=rc)
if(rc/=0)error stop 'sibling app invocation failed'
open(newunit=u,file='layout_payload.tmp',status='old')
read(u,*)value
close(u,status='delete')
if(value/=expected)error stop 'incorrect app payload'
print *,'payload=',value
end program
'''
for name, template in source_templates.items():
    project = root / name
    for directory in ["src", "app", "test"]:
        (project / directory).mkdir(parents=True)
    # External procedure wrappers must remain library units, including those
    # with their own internal procedures and leading comment lines.
    (project / "src" / "external.f90").write_text('''! External function control
integer function external_payload()
external_payload=23
contains
integer function unused_helper()
unused_helper=0
end function
end function
''')
    (project / "test" / "test_app_layout.f90").write_text(test_source)
    def manifest(expected):
        (project / "fpm.toml").write_text(
            'name="main_contained"\nversion="1.0.0"\n'
            '[extra.fo.test-args]\ntest_app_layout=["' + str(expected) + '"]\n')
    def run(driver, args, label):
        proc = subprocess.run([str(driver)] + args, cwd=project, env=env,
                              capture_output=True, text=True)
        (project / (label + ".log")).write_text(proc.stdout + proc.stderr)
        return proc
    manifest(23)
    (project / "app" / "main.f90").write_text(template.replace("DELTA", "0"))
    run(parent, ["build"], "parent-build")
    negative = run(parent, ["test", "test_app_layout", "--json"], "parent-test")
    for phase, delta in [("parent_cache", 0), ("warm", 0), ("edit", 8), ("restore", 0)]:
        expected = 23 + delta
        if phase in ["edit", "restore"]:
            (project / "app" / "main.f90").write_text(template.replace("DELTA", str(delta)))
            manifest(expected)
        ref_build = run(fpm, ["build", "--profile", "release"], phase + "-native-build")
        ref_test = run(fpm, ["test", "--profile", "release", "--", str(expected)],
                       phase + "-native-test")
        build = run(fixed, ["build"], phase + "-fo-build")
        test = run(fixed, ["test", "test_app_layout", "--json"], phase + "-fo-test")
        try:
            cases = json.loads(test.stdout)["tests"]
            verdict = len(cases) == 1 and cases[0]["status"] == "pass"
        except (json.JSONDecodeError, KeyError):
            verdict = False
        ok = (ref_build.returncode == ref_test.returncode == build.returncode
              == test.returncode == 0 and verdict
              and (project / "test" / "test_app_layout.f90").read_text() == test_source)
        results.append(dict(case=name, phase=phase, expected=expected,
                            parent_exit=negative.returncode, native_build=ref_build.returncode,
                            native_test=ref_test.returncode, fo_build=build.returncode,
                            fo_test=test.returncode, passed=ok))
        (root / "result.json").write_text(json.dumps(dict(fixed=str(fixed),
            parent=str(parent), fpm=str(fpm), results=results), indent=2) + "\n")
print(json.dumps(dict(passed=sum(r["passed"] for r in results), total=len(results))))
sys.exit(0 if all(r["passed"] for r in results) else 1)
