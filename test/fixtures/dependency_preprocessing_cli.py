#!/usr/bin/env python3
"""Independent package-macro ownership and manifest-only invalidation oracle."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def manifest(name, value, marker, dependencies="", dev=""):
    return (f'name = "{name}"\nversion = "1.0.0"\n'
            '[build]\nauto-executables = false\nauto-tests = true\n'
            f'[preprocess.cpp]\nmacros = ["OWNER={value}", "{marker}"]\n'
            + dependencies + dev)


def module(name, forbidden):
    return f'''module {name}
    implicit none
#ifdef OWNER
    integer, parameter :: value = OWNER
#else
    integer, parameter :: value = -1
#endif
#if defined({forbidden})
    logical, parameter :: leak = .true.
#else
    logical, parameter :: leak = .false.
#endif
end module
'''


def run(driver, root, expected, log, native=False):
    env = dict(os.environ, FO_BACKEND="fpm", FO_JOBS="1",
               OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1",
               EXPECTED_B=str(expected))
    command = [str(driver), "test", "test_macro_ownership", "--profile", "debug"]
    result = subprocess.run(command, cwd=root, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    log.write_text(result.stdout)
    if result.returncode:
        raise RuntimeError(f"{command} failed ({result.returncode}); {log}")
    if native:
        assert "PACKAGE_MACRO_ORACLE_PASS" in result.stdout, log
    else:
        assert "test_macro_ownership" in result.stdout, log
        assert "1 passed, 0 failed" in result.stdout, log
    if not native:
        commands = json.loads((root / "build/compile_commands.json").read_text())
        for entry in commands:
            filename = Path(entry["file"]).name
            values = {"root.F90": 11, "dep_a.F90": 22, "dep_b.F90": expected}
            if filename not in values:
                continue
            args = entry["arguments"]
            owners = [arg for arg in args if arg.startswith("-DOWNER=")]
            assert owners == [f"-DOWNER={values[filename]}"], (filename, owners)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fo", required=True, type=Path)
    parser.add_argument("--native-fpm", type=Path)
    parser.add_argument("--parent-fo", type=Path)
    parser.add_argument("--directory", type=Path)
    args = parser.parse_args()
    root = args.directory or Path(tempfile.mkdtemp(prefix="fo-package-macros-"))
    write(root / "fpm.toml", manifest("macro_root", 11, "ROOT_ONLY",
          '[dependencies]\ndep_a = {path = "dep_a"}\n',
          '[dev-dependencies]\ndep_dev = {path = "dep_dev"}\n'))
    write(root / "dep_a/fpm.toml", manifest("dep_a", 22, "A_ONLY",
          '[dependencies]\ndep_b = {path = "../dep_b"}\n'))
    write(root / "dep_b/fpm.toml", manifest("dep_b", 33, "B_ONLY"))
    write(root / "dep_dev/fpm.toml", manifest("dep_dev", 44, "DEV_ONLY",
          '[dependencies]\ndep_dev_child = {path = "../dep_dev_child"}\n'))
    write(root / "dep_dev_child/fpm.toml", manifest("dep_dev_child", 55, "CHILD_ONLY"))
    for name, forbidden in [("root", "A_ONLY"), ("dep_a", "ROOT_ONLY"),
                            ("dep_b", "A_ONLY"), ("dep_dev", "ROOT_ONLY"),
                            ("dep_dev_child", "DEV_ONLY")]:
        prefix = root if name == "root" else root / name
        write(prefix / f"src/{name}.F90", module(name, forbidden))
    write(root / "test/test_macro_ownership.F90", '''program test_macro_ownership
    use root, only: root_value => value, root_leak => leak
    use dep_a, only: a_value => value, a_leak => leak
    use dep_b, only: b_value => value, b_leak => leak
    use dep_dev, only: dev_value => value, dev_leak => leak
    use dep_dev_child, only: child_value => value, child_leak => leak
    implicit none
    integer :: expected_b, ios
    character(len=32) :: text
    expected_b = 33
    call get_environment_variable('EXPECTED_B', text, status=ios)
    if (ios == 0) then
        read(text, *, iostat=ios) expected_b
        if (ios /= 0) error stop 'invalid independent expected value'
    end if
    if (root_value /= 11) error stop 'root macro'
    if (a_value /= 22) error stop 'direct dependency macro'
    if (b_value /= expected_b) error stop 'transitive dependency macro'
    if (dev_value /= 44) error stop 'dev dependency macro'
    if (child_value /= 55) error stop 'dev transitive macro'
    if (root_leak .or. a_leak .or. b_leak .or. dev_leak .or. child_leak) &
        error stop 'macro crossed package boundary'
#ifdef OWNER
    if (OWNER /= 11) error stop 'test main macro'
#else
    error stop 'missing root test macro'
#endif
    print *, 'PACKAGE_MACRO_ORACLE_PASS'
end program
''')
    if args.native_fpm:
        run(args.native_fpm, root, 33, root / "native.log", native=True)
    if args.parent_fo:
        env = dict(os.environ, FO_BACKEND="fpm", FO_JOBS="1", EXPECTED_B="33")
        result = subprocess.run([str(args.parent_fo), "test", "test_macro_ownership",
                                 "--profile", "debug"], cwd=root, env=env, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (root / "parent-warm.log").write_text(result.stdout)
        assert result.returncode != 0, "parent unexpectedly satisfied ownership oracle"
        assert "direct dependency macro" in result.stdout, root / "parent-warm.log"
    run(args.fo, root, 33, root / "cold.log")
    run(args.fo, root, 33, root / "warm.log")
    dep_manifest = root / "dep_b/fpm.toml"
    stat = dep_manifest.stat()
    dep_manifest.write_text(dep_manifest.read_text().replace('OWNER=33', 'OWNER=34'))
    os.utime(dep_manifest, ns=(stat.st_atime_ns, stat.st_mtime_ns))
    run(args.fo, root, 34, root / "manifest-edit-restored-mtime.log")
    dep_manifest.write_text(dep_manifest.read_text().replace('OWNER=34', 'OWNER=33'))
    os.utime(dep_manifest, ns=(stat.st_atime_ns, stat.st_mtime_ns))
    run(args.fo, root, 33, root / "manifest-restore.log")
    print(f"PASS package macro ownership: cold, warm, manifest-only edit, restore: {root}")


if __name__ == "__main__":
    main()
