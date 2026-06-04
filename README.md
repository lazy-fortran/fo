# fo

Fortran build driver with module DAG, content-addressed cache, and
affected-test selection. Wraps fpm or cmake.

## Install

```bash
fpm install --prefix ~/.local
```

## Usage

Run `fo` in a directory with `fpm.toml` or `CMakeLists.txt`.

```
fo                  static -> build -> test (the default)
fo build            build only
fo build --flag -O0 fast debug build
fo test             run tests
fo test --only-changed  run only tests affected by changes
fo check            build + test, one-line status
fo check --json     build + test, JSON status for agents
fo check --json=compact  bounded JSON status for small local agents
fo check --json=full     legacy JSON plus diagnostic fields
fo check --agent    compact JSON status for opencode/Qwen
fo changed          list changed and affected modules
fo graph            module dependency graph
fo watch            rebuild on file change (inotify)
fo clean            clear global cache (~/.cache/fo)
fo info             backend, files, modules
```

Integration (AI agents, editors):

```
fo check --agent    one bounded JSON object for opencode/Qwen loops
fo check --json     stable legacy JSON for existing scripts
fo mcp-server       MCP JSON-RPC on stdin/stdout
fo lsp              LSP server (diagnostics on save)
```

`fo check --agent` writes one JSON object and no raw backend log. The object
is capped by the fixed output buffer and carries the fields an agent needs:

```
{"ok":false,"stage":"test","target":"test_x","summary":"...","hint":"...","rerun":"fo test test_x","log_path":"/tmp/fo-test.log","elapsed_s":0.12}
```

MCP `check` calls return the same compact shape by default. Request full output
only when the caller needs diagnostic arrays or log paths.

## How it works

1. Scan `.f90`/`.F90` files, parse `use`/`module` statements.
2. Build the module dependency DAG, topological sort.
3. Hash each module: `FNV-1a(source + compiler + flags + dep hashes)`.
4. Compare hashes against global cache at `~/.cache/fo/index`.
5. Delegate build to fpm or cmake. Report cache hits.
6. Compute reverse-dependency closure of changed modules.
7. Run only affected tests (skip slow tests by default).

## Backend detection

`fpm.toml` -> fpm. `CMakeLists.txt` -> cmake + ninja. fpm takes
precedence when both exist. Non-Fortran directories exit silently.

## Slow test exclusion

Tests named `*_slow` or `*_slow_*` are excluded by default.
Use `fo test --all` to include them. fpm lists targets and runs the
non-slow subset; cmake passes `-LE slow`.

## Test parallelism

`FO_JOBS=N` caps build and test fanout. Missing or invalid `FO_JOBS` falls
back to `nproc`. fpm receives the limit through `OMP_NUM_THREADS`; CMake and
CTest receive `-j N`. CMake selected tests run through one `ctest -R` expression
instead of one process per test.

## Go parity

| Go feature | fo |
|---|---|
| Global content-addressed cache | `~/.cache/fo/index`, FNV-1a |
| Cache key = hash(source + compiler + flags + dep hashes) | yes |
| Affected-test selection | `fo changed`, `fo test --only-changed` |
| Parallel builds | `FO_JOBS` or nproc for fpm, cmake, ctest |
| Flag passthrough | `fo build --flag` |
| Cache clear | `fo clean` |
| Backend autodetection | fpm.toml or CMakeLists.txt |
| Watch mode | `fo watch` (inotify) |

## Tests

142 tests: scanner (30), DAG (15), cache (8), backend (20), check (69).

## Benchmarks

Tested on 7 codebases: SIMPLE (1231 files), fortui (20), libneo (223),
GORILLA (57), NEO-RT (167), sampledex (17), fluff (178).

The current full matrix keeps `fo` in the Rust/C compile-overhead band
on nbody and ahead of CMake/C there. Go still wins the tiny clean
compile steps; `fo` wins the measured nbody test overhead in this run.
`fo` wins the 200-module bigmod clean build among the Fortran drivers;
remaining work is surrounding driver overhead, not compiler code. See
[fpm-dev issue #3](https://github.com/krystophny/fpm-dev/issues/3) for
the full cross-language comparison.

Architecture: `doc/FO.md`. Linux primitives: `doc/LINUX.md`.
