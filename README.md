# fo

Fortran build driver. Scans module dependencies, builds the DAG, delegates
to fpm or cmake, selects affected tests.

## Install

```bash
fpm install --prefix ~/.local
```

## Commands

```
fo check    build + test, compact status line
fo build    build only (delegates to fpm or cmake)
fo test     run tests only
fo graph    print module dependency graph (name -> dep)
fo info     detected backend and module count
fo version  print version
```

## Backend detection

fo looks for `fpm.toml` (fpm) or `CMakeLists.txt` (cmake) in the current
directory. fpm takes precedence when both exist.

## Module scanner

Parses `use` and `module` statements from `.f90`, `.F90`, `.f`, `.F` files.
Skips intrinsic modules (`iso_fortran_env`, `iso_c_binding`, `ieee_*`,
`omp_lib`, `mpi*`). Excludes `build/`, `.git/`, `node_modules/`.

## Status

v0.1.0. Scanner, DAG, backend detection, check command work. Tested on
SIMPLE, fortui, libneo, GORILLA, NEO-RT, sampledex, fluff.

Planned: content-addressed build cache, affected-test selection, inotify
watch mode, MCP server (single tool, action dispatch).

Architecture: `doc/FO.md`. Linux primitives: `doc/LINUX.md`.
