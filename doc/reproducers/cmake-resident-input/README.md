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
This fixture is an independent reduction for the next backend repair; it is
not a passing Gremlin integration test. Stop only the explicitly owned temporary
reproducer lane after inspection, leaving other resident lanes untouched.

`src/run/fo_gremlin_context.f90` parses the FPM configuration in `capture_context`;
input declarations and frozen dependency closure also need a CMake route.
Skipping the parser alone would leave those contracts unfulfilled. The owning
[CMake plan](../../CMAKE_GREMLIN_PLAN.md) must close the entire capture/build/test
loop with positive registered-test evidence, negative edited-source evidence,
profile/dependency invalidation and last-compilable behavior.

Chris&AI
