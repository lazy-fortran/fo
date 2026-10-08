# CMake resident input-capture reproducer

This CMake-only fixture has one executable and one registered behavioral test.
It records a resident Gremlin blocker observed on 2026-10-05. Public delegated
Fo test/exec commands work on the original consumer, but resident generation
capture tries to parse `fpm.toml` before choosing a CMake input path.

Use an explicitly selected Fo driver. The observed driver SHA256 was
`6e54f805af13272e859ebf6c7ec48d205a22d7fdac7c84d435fdffc922bc76ee`.

```sh
cmake -S doc/reproducers/cmake-resident-input -B /tmp/fo-cmake-input-build
cmake --build /tmp/fo-cmake-input-build
ctest --test-dir /tmp/fo-cmake-input-build --output-on-failure
"$FO_DRIVER" gremlin start --dir "$PWD/doc/reproducers/cmake-resident-input" \
    --lane cmake-input-reproducer
"$FO_DRIVER" gremlin status --dir "$PWD/doc/reproducers/cmake-resident-input" \
    --lane cmake-input-reproducer
```

Expected: capture a CMake generation and retain CTest's registered-test verdict.
Observed on the original consumer: `capture_failed`, diagnostic
`cannot parse fpm.toml for generation inputs`, no verified generation.
This historical negative is now supplemented by `test_gremlin_cmake`, an
independent delegated resident integration fixture. Run it with an explicitly
selected driver to check presets, explicit native build targets, declared
source/provider edits, registered CTest fixtures/cwd/verdicts, invalid-source
retention and referenced-input safety. Stop only the explicitly owned temporary
reproducer lane after inspection, leaving other resident lanes untouched.

The delegated route now chooses the native CMake inventory before FPM parsing.
The owning [CMake plan](../../CMAKE_GREMLIN_PLAN.md) records the supported closure
and its limits. CMake/CTest remain the configuration, build and registered-test
authorities; this fixture does not establish standalone compatibility or parity
for every project/profile.

Chris&AI
