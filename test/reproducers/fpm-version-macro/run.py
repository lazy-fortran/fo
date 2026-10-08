"""Compare compiled version placeholders with a native FPM reference.

Usage: python3 run.py /absolute/path/to/fo /absolute/path/to/fpm /new/output/root
"""
from pathlib import Path
import json
import os
import subprocess
import sys

fo = Path(sys.argv[1]).resolve()
fpm = Path(sys.argv[2]).resolve()
root = Path(sys.argv[3]).resolve()
root.mkdir(parents=True, exist_ok=False)
results = []
source = """program test_version_macro
use macro_reference, only: legacy_value
implicit none
character(64) :: observed, expected
#define STRINGIFY_START(X) "&
#define STRINGIFY_END(X) &X"
observed = STRINGIFY_START(PACKAGE_VERSION)
STRINGIFY_END(PACKAGE_VERSION)
call get_command_argument(1, expected)
print *, 'compiled version: ', trim(observed)
if (trim(observed) /= trim(expected)) error stop 'incorrect compiled version'
if (UNCHANGED /= legacy_value) error stop 'unrelated macro changed'
end program
"""
environment = dict(os.environ, FO_BACKEND="fpm", FO_JOBS="1", OMP_NUM_THREADS="1")

for case, version, header in [("explicit", "0.13.0", "[preprocess.cpp]\nmacros"),
                              ("default", None, "[preprocess.cpp]\nmacros"),
                              ("dotted", "2.3.4", "[preprocess]\ncpp.macros"),
                              ("long_name", "8.9.10", "[preprocess.cpp]\nmacros")]:
    project = root / case
    (project / "test").mkdir(parents=True)
    (project / "src").mkdir()
    (project / "src" / "reference.f90").write_text(
        "module macro_reference\nimplicit none\ninteger, parameter :: legacy_value=7\nend module\n")
    macro_name = "PACKAGE_VERSION" + ("X" * 100 if case == "long_name" else "")
    case_source = source.replace("PACKAGE_VERSION", macro_name)
    test_source = project / "test" / "test_version_macro.f90"
    test_source.write_text(case_source)
    def manifest(value):
        text = 'name="version_macro"\n'
        if value is not None:
            text += 'version="' + value + '"\n'
        expected = value if value is not None else "0"
        return (text + header + ' = ["' + macro_name + '={version}", "UNCHANGED=7"]\n'
                '[extra.fo.test-args]\ntest_version_macro=["' + expected + '"]\n')
    phases = [("cold", version), ("warm", version), ("edit", "4.5.6"),
              ("restore", version)]
    for phase, value in phases:
        (project / "fpm.toml").write_text(manifest(value))
        expected = value if value is not None else "0"
        reference = subprocess.run([str(fpm), "test", "--profile", "release", "--", expected],
                                   cwd=project, env=environment, capture_output=True, text=True)
        (project / (phase + "-native.log")).write_text(reference.stdout + reference.stderr)
        candidate = subprocess.run([str(fo), "test", "test_version_macro", "--json"],
                                   cwd=project, env=environment, capture_output=True, text=True)
        (project / (phase + "-fo.log")).write_text(candidate.stdout + candidate.stderr)
        try:
            report = json.loads(candidate.stdout)
            tests = report.get("tests", [])
            verdict = len(tests) == 1 and tests[0].get("status") == "pass"
        except json.JSONDecodeError:
            verdict = False
        ok = (reference.returncode == 0 and candidate.returncode == 0 and verdict
              and test_source.read_text() == case_source)
        results.append({"case": case, "phase": phase, "expected": expected,
                        "native_exit": reference.returncode, "fo_exit": candidate.returncode,
                        "pass": ok})
        (root / "result.json").write_text(json.dumps({"fo": str(fo), "fpm": str(fpm),
                                                     "results": results}, indent=2) + "\n")
print(json.dumps({"passed": sum(r["pass"] for r in results), "total": len(results)}))
sys.exit(0 if all(r["pass"] for r in results) else 1)
