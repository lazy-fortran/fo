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

The product target is a standalone drop-in replacement for FPM, followed by
native support for a defined CMake subset used by ITpPlasma projects. The staged
delivery order is in [PLAN.md](PLAN.md).
Path, Git and local namespaced registry dependencies use the native backend,
as does executable installation. The current CMake backend invokes CMake/CTest;
complete standalone parity remains under development in [PLAN.md](PLAN.md).

Environment management, containers, CI governance, agent scheduling and
scientific proof or synthesis systems remain outside fo's scope.

For reproducibility, fo uses content-based cache/action identities, explicit
project inputs, deterministic selection/replay where supported, and explicit
compiler/configuration reporting.
Hermetic operating environments should be supplied externally when required.


## Install

```bash
fpm install --profile release --prefix ~/.local
```

## Commands

```text
fo                         start or attach to resident Gremlin
fo verify                  static checks, build, tests, lint, format check
fo build [--profile NAME]  build applications and examples
fo build --tests           also build test programs (then fo test --no-build)
fo test [NAME ...]         build and run tests
fo test --only-changed     run tests affected by changed modules
fo test --random 12        reproducible sample without replacement
fo test --random 12 --seed 1729
fo gremlin                continuous generation-aware build and testing
fo run [options] NAME      build and run an application or example
fo exec NAME [ARGS...]     build and run any fo executable target
fo check --json=compact    compact build and test status
fo changed                 changed modules and reverse dependents
fo graph [--dot]           module dependency graph
fo lint                    native source checks and compiler warnings
fo fmt [--check]           format or check formatting
fo clean                   remove the project build tree
fo clean --cache           also remove the shared content store (no build may run)
fo install [--prefix DIR]  install a release build
fo info                    backend, source, compiler, and cache information
fo mcp-server              MCP JSON-RPC server on standard input/output
fo lsp                     diagnostics for unsaved editor buffers
```

Bare `fo` starts or attaches to the default Gremlin lane. Use explicit
`fo gremlin start --lane development` for a controller-assigned development
lane; use `fo verify` for the broad staged pipeline at a verification checkpoint.

Affected-test selection is being consolidated around canonical execution-input
changes rather than compile-cache misses in [#189](https://github.com/lazy-fortran/fo/issues/189).
Until that contract is complete, use explicit known reproducers/affected targets
or conservative wider testing rather than interpreting an empty selection as
proof that a change is safe. Build-cache reuse is not evidence that tests passed.
Gremlin capabilities and remaining limitations are recorded in [PLAN.md](PLAN.md).

Each test has a budget of 10 seconds of CPU time (300 for tests named
`*_slow`, which run only under `fo test --all`). The budget counts the user
CPU time of the test process alone, from the moment it starts: not wall time,
not queueing or building, and not system time, which a loaded host inflates
by orders of magnitude through kernel lock contention. A heavily loaded host
therefore does not turn a passing test into a timeout. A wall-clock cap of ten times the budget still stops a test that hangs
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

### Cases, retries and SLOW

An executable that implements the fo listing protocol is run case by case:
`FO_TEST_LIST=1` makes it print one `fo-case NAME` line per case (an optional
TAB and `;`-separated project sources narrow the case's input closure), and
`FO_TEST_CASE=NAME` runs one case. test-drive executables with the standard
driver (`exe SUITE TEST`) are adapted without changes; other frameworks stay one
case. Cases are named `exe::case` and selected like tests
(`fo test exe::two`, `'exe::*slow*'`); `fo test --list` shows the cases of built
executables. A test that fails and passes on a retry is FLAKY, never PASS.

```toml
[extra.fo]
test-retries = 1                                   # default: one isolated retry
slow-timeout = { period = 60, terminate-after = 3 } # SLOW line, then TIMEOUT

[extra.fo.test-groups]
network = { tests = ["test/test_net*"], retries = 3 }
```

`fo test --retries N` overrides the retries and `--strict-flaky` makes a FLAKY
verdict fail the run. For CMake, the CTest properties `FO_RETRIES`,
`FO_SLOW_PERIOD` and `FO_SLOW_TERMINATE_AFTER` and the label `flaky` (two
retries) play the same role; `TIMEOUT` stays the terminate bound.

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

The default output is intentionally quieter than fpm. `fo check --json=compact` and
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

Git dependencies are acquired with Git and built through fo's native backend.
Namespaced registry dependencies resolve from a local FPM registry tree without
FPM or network access. Fo reads `[registry] path` from
`~/.local/share/fpm/config.toml`; `FO_FPM_CONFIG_FILE` selects an absolute alternate
configuration path. A registry package lives under `path/namespace/name/version`.
An explicit `v` selects that numeric version; omitting `v` selects the latest
numeric version deterministically. Remote registries, authentication and
publishing requests fail with explicit diagnostics.

`fo install` builds release-profile app executables through the native backend
and installs them under `prefix/bin`. Example, test, library and module
installation remain unsupported. Project compilation and testing use fo's
native backend and cache.

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
top-level file unconditionally declares a CTest contract with `include(CTest)`
or `enable_testing()` (a declaration inside `if()`, `foreach()` or a function is
ignored, since it may register no tests); otherwise it uses `fpm.toml`. Set `FO_BACKEND=cmake` to
select CMake explicitly (`FO_BACKEND=fpm` restores the
native fpm backend explicitly).

## Cache and concurrency

Action IDs are SHA-256 hashes of source content, compiler identity, effective
flags, and dependency module payloads. Shared cache data defaults below
`~/.cache/fo`; set `FO_CACHE_DIR` to isolate it.

A compile action of the CMake split route is keyed by: the content of the
source and of every file its `include`/`#include "x"`/`#include <x>` lines
resolve to (an include through a macro makes the action uncacheable); the
compiler identity; the compile options in order, with output and module
directories dropped and include directories replaced by their position
(what they resolve to is keyed by content); the interface keys of the
modules it uses; for C and C++, the content of every header the compiler's
dependency file lists (a build tree without that file, such as a fresh one,
first runs the action's preprocessor to write it, so the action is keyed
before it is compiled); and the source argument passed to the compiler, because
compilers embed it in the object (gfortran in runtime error messages even
without `-g`). With debug information the compile directory is keyed too,
and `-march=native`-style options add the host CPU identity. Every part is
keyed in full; a key that cannot be complete is not used. Results therefore
stay keyed by location by default. `FO_CACHE_RELOCATABLE=1` passes sources
below the project root relative to it and, with `-g`, adds
`-fdebug-prefix-map=<root>=.` and `-gno-record-gcc-switches`, so identical
trees at different paths (worktrees, CI checkouts) share results and produce
byte-identical objects; a renamed file is still a different action.
Compiler/system modules outside the include directories (intrinsic modules,
system `.mod` files) are part of the toolchain identity, not keyed by content. Store schema and migration
contracts are recorded in [PLAN.md](PLAN.md).

The store is bounded. `FO_CACHE_MAX_SIZE` sets its cap (default `10G`;
`K`/`M`/`G`/`T` suffixes, `0` disables the cap). At most every five minutes a
publishing build checks the store size; above the cap it retires the least
recently used actions and deletes unreachable objects until the store is at
80 % of the cap. Entries used or published within the grace period
(`FO_CACHE_GRACE`, default 3600 seconds) are never evicted, so the cap is soft
while more than that is in active use. Leftover staging files of killed
builds are removed after a day.

The source scan cache has a separate cap, `FO_SCAN_CACHE_MAX_SIZE` (default
`1G`, the same suffixes, `0` disables eviction). On cache loads and saves,
at most once every five minutes, files in `scan/v1` are evicted by last use
down to 80 % of the cap. Compiler-visible CMake source scans are keyed by
content after preprocessing, so changed includes still invalidate them and
relocated sources can reuse dependency facts. Successful reads refresh their
last-use stamp at most once an hour. Temporary files older than an hour are reaped. This cap
is approximate between sweeps and while writers are publishing; concurrent
readers and evictors need no lock and a lost entry causes a rescan.

Gremlin uses a separate soft budget, `FO_GREMLIN_CACHE_MAX_SIZE` (default
`8G`, `0` disables eviction). After a finite campaign reaches quiescence,
at most once every five minutes, it counts its generation and host-local
state trees and removes the oldest inactive execution views down to 80 %
of the budget. Budgets count regular-file content bytes; filesystem block
and directory overhead are additional. Views younger than an hour, every view of a live lane, and
lanes with pending payload records or unverifiable scratch are protected.
Validated scratch of a dead lane with drained payload registries is retired
with its view. View eviction takes no lock. Each new owner creates fresh views; it never acquires a retired
one. `fo clean --gremlin` runs the same eviction pass immediately.
Generation pins/leases and durable journals retain their existing
retirement rules and may keep usage above the budget. Shared action objects
remain governed by `FO_CACHE_MAX_SIZE`; Gremlin session state uses local
process identities and must remain host-local.

The store is safe to share between hosts, including filesystems whose locks
are host-local (BeeGFS) and NFS. It takes no lock for correctness: objects
are published by exclusive link and verified by hash on every read, action
records are replaced by rename and read with a retry on `ESTALE`, a reused
object's mtime is refreshed on publication and a record's on use, and
eviction spares everything inside the grace period. A concurrent eviction or
a reader racing a replacement therefore yields a cache miss and a rebuild,
never another action's output. Keep the grace period longer than any single
build step and the clocks of sharing hosts within a few seconds.

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
[PLAN.md](PLAN.md).
