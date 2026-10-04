# fo

`fo` is a fast Fortran build and test driver. It reads `fpm.toml`, builds the
module DAG with native OpenMP parallelism, caches compilation actions by
content, and reports compact diagnostics suitable for people and coding
agents. CMake projects use CMake and CTest directly.

## Scope

`fo` aims for the product boundary of a focused Fortran development tool: roughly
the build/test/run/cache role of Cargo or the Go toolchain, plus practical
formatting, lint/static diagnostics, LSP/MCP integration and continuous
affected-test feedback.

It deliberately does **not** replace fpm as the package manifest/registry, Nix
or Spack as an environment manager, containers as a sandbox, CI as repository
governance, agent frameworks as task schedulers, or FortSym/other research tools
as proof/synthesis/workflow systems. The older capsule/provenance/Nix/FortOS
experiments remain historical closed issues (#1, #3, #6, #7); `fo prove` /
synthesis (#120) and complete host toolchain/runtime capture (#157) are also
closed as out of scope.

For reproducibility, fo guarantees what a build/test driver reasonably can:
content-based cache/action identities, explicit project inputs, deterministic
selection/replay where supported, and honest compiler/configuration reporting.
Hermetic operating environments should be supplied externally when required.


## Install

```bash
fpm install --profile release --prefix ~/.local
```

## Commands

```text
fo                         static checks, build, tests, lint, format check
fo build [--profile NAME]  build applications and examples
fo test [NAME ...]         build and run tests
fo test --only-changed     run tests affected by changed modules
fo test --random 12        reproducible sample without replacement
fo test --random 12 --seed 1729
fo gremlin                continuous generation-aware build and testing
fo run [options] NAME      build and run an application or example
fo exec NAME [ARGS...]     build and run any fo executable target
fo check [--json...]       compact build and test status
fo changed                 changed modules and reverse dependents
fo graph [--dot]           module dependency graph
fo lint                    native source checks and compiler warnings
fo fmt [--check]           format or check formatting
fo clean                   remove the project build tree
fo clean --cache           also remove the shared content store
fo install [--prefix DIR]  install a release build
fo info                    backend, source, compiler, and cache information
fo mcp-server              MCP JSON-RPC server on standard input/output
fo lsp                     diagnostics-on-save language server
```

Affected-test selection is being consolidated around canonical execution-input
changes rather than compile-cache misses in [#189](https://github.com/lazy-fortran/fo/issues/189).
Until that contract is complete, use explicit known reproducers/affected targets
or conservative wider testing rather than interpreting an empty selection as
proof that a change is safe. Build-cache reuse is not evidence that tests passed.
Gremlin capabilities and remaining limitations are recorded in [PLAN.md](PLAN.md).

Each test has a budget of 10 seconds of CPU time (300 for tests named
`*_slow`, which run only under `fo test --all`). The budget counts CPU, not
wall time, so a heavily loaded host does not turn a passing test into a
timeout. A wall-clock cap of ten times the budget still stops a test that hangs
without using CPU. A timeout names the limit that fired. Override per project
in `fpm.toml`, or per invocation with `FO_TEST_TIMEOUT`,
`FO_SLOW_TEST_TIMEOUT` and `FO_TEST_WALL_TIMEOUT` (these take precedence):

```toml
[extra.fo]
test-timeout = 20        # CPU seconds per test
slow-test-timeout = 600  # CPU seconds per *_slow test
test-wall-timeout = 900  # wall-clock cap per test
```

CPU time is read from `/proc` on Linux. Elsewhere the budget stays a
wall-clock limit.

Builds with requested flags (`--flag`, `--profile NAME`, `--release`) keep
their executables apart from the default build, under
`build/fo/profiles/<flags>/`. `fo exec` and `fo test` pick the tree by the
flags they are given, also with `--no-build`, so a later plain `fo` cannot swap
an optimised binary for an unoptimised one under a running job:

```sh
fo build --release
fo exec --release --no-build solver input.nml
```

`fo exec` options go before the target; everything after it is passed to the
program unchanged.

The default output is intentionally quieter than fpm. `fo check --agent` and
MCP checks return one bounded JSON object with the failure, hint, rerun command,
and log path.

Native `ffc` mode is separate:

```text
fo build --native [-o PROGRAM] SOURCE...
fo run --native SOURCE [ARGS...]
```

It requires `ffc 0.1.0` or newer.

## fpm compatibility

`fo` implements the common fpm project contract while retaining its own cache
and compact output. Supported manifest behavior includes:

- library, application, test, and example source trees
- automatic and explicit `[[executable]]`, `[[test]]`, and `[[example]]`
  target names
- path, Git, registry, and development dependencies
- build links and compiler flags
- C preprocessing macros
- Fortran source form, implicit typing, and implicit external settings
- the OpenMP metapackage
- `build.auto-executables = false` with explicit `[[executable]]` selection
- release, debug, and sanitizer profiles

`fo run` accepts fpm-style `--target`, `--example`, `--profile`, and `--flag`
options. fo-specific commands and options remain available.

Native project artifacts live under `build/fo`. Applications and examples are
also exposed in `build/fo/app`. This stable path avoids coupling tools to fpm's
private compiler-flag hash directories. Running fpm after fo is safe because fo
does not write fpm's private digest metadata.

Git and registry dependencies are currently resolved and bootstrapped through
an installed fpm when their compiled artifacts are absent. `fo install` also
uses fpm's installation model. Project compilation and testing then run through
fo's native backend and cache.

Tests that require different command-line arguments can declare fo-specific
metadata without changing fpm's target model:

```toml
[extra.fo.test-args]
test_oracle = ["test/data/reference.csv", "--strict"]
```

The key is the public test name. Each array element is passed as one argument,
including values that contain spaces.

## CMake projects

If no `fpm.toml` exists, fo searches parent directories for `CMakeLists.txt`.
It configures with Ninja, builds with `cmake --build -j`, and runs CTest.
`FO_CMAKE_ARGS` supplies extra configure arguments. `FO_JOBS` controls build
and test fanout. A directory containing both manifests uses CMake when its
top-level file declares a CTest contract with `include(CTest)` or
`enable_testing()`; otherwise it uses `fpm.toml`. Set `FO_BACKEND=cmake` to
select CMake explicitly (`FO_BACKEND=fpm` restores the
native fpm backend explicitly).

## Cache and concurrency

Action IDs are SHA-256 hashes of source content, compiler identity, effective
flags, and dependency module payloads. Shared cache data defaults below
`~/.cache/fo`; set `FO_CACHE_DIR` to isolate it. Store schema and migration
contracts are recorded in [PLAN.md](PLAN.md).

The module DAG and selected tests run through native OpenMP loops. Each worker
has private command, log, and temporary-path state. Cache publication is
atomic, and a project lock protects build-tree materialization across
processes. `FO_JOBS=N` caps fanout and defaults to the available CPU count.
Gremlin's bootstrap capacity is currently narrower; query its supported options
rather than assuming ordinary build fanout is available in every mode.

GNU Fortran builds enable `-Warray-temporaries` by default. Disable it only
with an explicit project decision:

```bash
fo build --flag "-Wno-array-temporaries"
```

GNU Fortran compilation also uses `-pipe`, avoiding intermediate compiler-stage
files. On compatible compiler drivers, fo links with LLD when `ld.lld` is
available; if that link fails, fo transparently retries the compiler driver's
default system linker. Set `FO_LINKER=default` to disable LLD or
`FO_LINKER=lld` to request it explicitly. Linking always goes through the
Fortran compiler driver, so compiler runtime and OpenMP libraries remain
correctly selected.

## Development

Use a pinned exact fo candidate, warm caches and focused correctness tests for
each change. The lazy-fortran controller pushes small locally green increments
to main without waiting for GitHub CI, the full Gremlin epoch or unrelated work.
A confirmed current correctness regression gets repair/revert priority. Broad
`fo`/`fo test --all` correctness runs remain useful background or milestone
validation; they are not required after every mechanical edit.

Gremlin reports verification facts, not repository governance. Projects with
required human review or CI keep those requirements; this repository's fast-main
policy is not imposed on other users.

Benchmarks are **optional, non-blocking audits**, not development/main/merge
gates. [#190](https://github.com/lazy-fortran/fo/issues/190) plans real byte-edit
and edit-to-verdict measurements in independent scheduled/manual CI or explicit
slow/performance execution. [#145](https://github.com/lazy-fortran/fo/issues/145)
plans isolated pinned acquisition of ffc, fpm and other public examples. Normal
fo/test/Gremlin commands must not acquire benchmark projects or enroll timing
audits automatically. Existing ordinary project dependency bootstrap is separate.
See [bench/README.md](bench/README.md) for current tooling and limitations.

Architecture details are in [doc/FO.md](doc/FO.md); current implementation
priorities, honest capability limits and issue ownership are in
[PLAN.md](PLAN.md) and [ROADMAP.md](ROADMAP.md).
