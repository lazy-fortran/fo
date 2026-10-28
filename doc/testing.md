# Testing with fo

This page describes how `fo test` runs tests: time budgets, per-case execution,
retries and builds with non-default flags. Test selection (names, labels,
partitions, `--only-changed`) and the test report format are described in
[FO.md](FO.md#test-selection).

## Time budgets

Each test has a budget of 10 seconds of CPU time. Tests whose name ends in
`_slow` get 300 seconds and run only under `fo test --all`.

The budget counts the user CPU time of the test process from the moment it
starts. Wall time, queueing, building and system time are not counted; system
time in particular can grow by orders of magnitude on a loaded host through
kernel lock contention. A passing test therefore does not become a timeout on a
busy machine. A wall-clock cap of ten times the budget still stops a test that
hangs without using CPU. A timeout message names the limit that fired.

Override the limits per project in `fpm.toml`:

```toml
[extra.fo]
test-timeout = 20        # CPU seconds per test
slow-test-timeout = 600  # CPU seconds per *_slow test
test-wall-timeout = 900  # wall-clock cap per test
```

or per invocation with `FO_TEST_TIMEOUT`, `FO_SLOW_TEST_TIMEOUT` and
`FO_TEST_WALL_TIMEOUT`, which take precedence. CPU time is read from `/proc` on
Linux; on other systems the budget is a wall-clock limit.

## Cases

An executable that implements the fo listing protocol is run case by case.
With `FO_TEST_LIST=1` it prints one `fo-case NAME` line per case, optionally
followed by a TAB and a `;`-separated list of project sources that narrows the
case's input closure. With `FO_TEST_CASE=NAME` it runs one case.

Executables using the standard test-drive driver (`exe SUITE TEST`) are adapted
without changes; executables using other frameworks are treated as a single
case. Cases are named `exe::case` and selected like tests, for example
`fo test exe::two` or `fo test 'exe::*slow*'`. `fo test --list` shows the cases
of built executables.

## Retries and slow tests

A test that fails and then passes on a retry is reported as FLAKY, never PASS.
By default a failing test is retried once, in isolation.

```toml
[extra.fo]
test-retries = 1                                    # default
slow-timeout = { period = 60, terminate-after = 3 } # SLOW line, then TIMEOUT

[extra.fo.test-groups]
network = { tests = ["test/test_net*"], retries = 3 }
```

`fo test --retries N` overrides the retry count and `--strict-flaky` makes a
FLAKY verdict fail the run. For CMake projects the CTest properties
`FO_RETRIES`, `FO_SLOW_PERIOD` and `FO_SLOW_TERMINATE_AFTER`, and the label
`flaky` (two retries), play the same role; the CTest `TIMEOUT` property remains
the hard limit.

## Test arguments

Tests that need command-line arguments declare them without changing fpm's
target model:

```toml
[extra.fo.test-args]
test_oracle = ["test/data/reference.csv", "--strict"]
```

The key is the public test name. Each array element is passed as one argument,
including values that contain spaces.

## Profiles and flags

Builds with requested flags (`--flag`, `--profile NAME`, `--release`) keep their
executables apart from the default build, under `build/fo/profiles/<flags>/`.
`fo exec` and `fo test` choose the tree from the flags they are given, also with
`--no-build`, so a later plain `fo build` does not replace an optimised binary
that a running job is using:

```sh
fo build --release
fo exec --release --no-build solver input.nml
```

`fo exec` options go before the target name; everything after it is passed to
the program unchanged.

## Output

The default output is quieter than fpm's. `fo check --json=compact` and the MCP
check tool return one bounded JSON object with the failure, a hint, the rerun
command and the log path. Build-cache reuse is not evidence that tests passed,
and an empty `--only-changed` selection should not be read as proof that a
change is safe; run a wider set of tests when in doubt.
