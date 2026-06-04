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
fo changed          list changed and affected modules
fo graph            module dependency graph
fo watch            rebuild on file change (inotify)
fo clean            clear global cache (~/.cache/fo)
fo info             backend, files, modules
```

Integration (AI agents, editors):

```
fo mcp-server       MCP JSON-RPC on stdin/stdout
fo lsp              LSP server (diagnostics on save)
```

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
Use `fo test --all` to include them. cmake backend passes `-LE slow`.

## Go parity

| Go feature | fo |
|---|---|
| Global content-addressed cache | `~/.cache/fo/index`, FNV-1a |
| Cache key = hash(source + compiler + flags + dep hashes) | yes |
| Affected-test selection | `fo changed`, `fo test --only-changed` |
| Parallel builds | auto nproc for cmake |
| Flag passthrough | `fo build --flag` |
| Cache clear | `fo clean` |
| Backend autodetection | fpm.toml or CMakeLists.txt |
| Watch mode | `fo watch` (inotify) |

## Tests

54 tests: scanner (27), DAG (15), cache (8), backend (4).

## Benchmarks

Tested on 7 codebases: SIMPLE (1231 files), fortui (20), libneo (223),
GORILLA (57), NEO-RT (167), sampledex (17), fluff (178).

fo beats Go on compile time (0.09s vs 0.12s nbody debug). See
[fpm-dev issue #3](https://github.com/krystophny/fpm-dev/issues/3) for
full cross-language comparison.

Architecture: `doc/FO.md`. Linux primitives: `doc/LINUX.md`.
