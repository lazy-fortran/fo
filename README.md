# fo

`fo` is a fast Fortran build and test driver. It reads `fpm.toml`, builds the
module DAG with native OpenMP parallelism, caches compilation actions by
content, and reports compact diagnostics suitable for people and coding
agents. CMake projects use CMake and CTest directly.

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

Tests have a 10-second hard timeout by default. Set `FO_TEST_TIMEOUT` for an
intentional override; longer tests should normally use a `_slow` suffix and
run through `fo test --all`.

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
flags, and dependency module payloads. The shared store defaults to
`~/.cache/fo/store/v1`; set `FO_CACHE_DIR` to isolate it.

The module DAG and selected tests run through native OpenMP loops. Each worker
has private command, log, and temporary-path state. Cache publication is
atomic, and a project lock protects build-tree materialization across
processes. `FO_JOBS=N` caps fanout and defaults to the available CPU count.

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

Run `fo` with no arguments before each commit. This executes the full static,
build, test, lint, and formatting-check pipeline. Architecture and compatibility
details are in [doc/FO.md](doc/FO.md).
