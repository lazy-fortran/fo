# fo

Fortran build driver. Scans module dependencies, builds the DAG, delegates
to fpm or cmake, selects affected tests.

## Install

```bash
fpm install --prefix ~/.local
```

## Commands

```
fo check              build + test, compact status with cache/changed/affected counts
fo changed            list changed and affected modules (reverse-dep closure)
fo build              build only (delegates to fpm or cmake)
fo build --flag -O0   build with flags (fpm --flag, cmake -DCMAKE_Fortran_FLAGS)
fo test               run tests only
fo graph              print module dependency graph (name -> dep)
fo info               detected backend and module count
fo clean              clear global build cache (~/.cache/fo)
fo version            print version
```

## Backend detection

fo looks for `fpm.toml` (fpm) or `CMakeLists.txt` (cmake) in the current
directory. fpm takes precedence when both exist.

## Module scanner

Parses `use` and `module` statements from `.f90`, `.F90`, `.f`, `.F` files.
Skips intrinsic modules (`iso_fortran_env`, `iso_c_binding`, `ieee_*`,
`omp_lib`, `mpi*`). Excludes `build/`, `.git/`, `node_modules/`.

## Go parity

| Go feature | fo |
|---|---|
| Global content-addressed cache | `~/.cache/fo/index`, FNV-1a hash |
| Cache key = hash(source + flags + compiler + dep hashes) | yes |
| Skip recompilation on cache hit | reports hits; delegates to fpm/cmake |
| Affected-module tracking (reverse-dep closure) | `fo changed` |
| Flag passthrough (-O0 for fast, -O2 for release) | `fo build --flag` |
| Cache clear | `fo clean` |
| Backend autodetection | fpm.toml or CMakeLists.txt |

## Status

v0.1.0. 37 tests. Tested on SIMPLE (1231 files), fortui (20), libneo
(223), GORILLA (57), NEO-RT (167), sampledex (17), fluff (178).

Planned: inotify watch mode, MCP server (single tool, action dispatch).

Architecture: `doc/FO.md`. Linux primitives: `doc/LINUX.md`.

Benchmark: https://litter.catbox.moe/npsfv2.png
