# fo

Fortran build cache, incremental rebuild, affected-test selection, MCP server.

## Build and Test

Use `fo` for every edit/build/test loop. Never call `fpm`, `cmake`, `make`,
or compiler commands directly except to diagnose a `fo` failure or confirm
backend parity after a `fo` fix.

```bash
fo check          # build + test, one-line status
fo check --json   # JSON status
fo build          # build only
fo test           # run tests
fo                # staged pipeline: static -> build -> test
```

If `fo` is slower than the backend or cannot handle the project, fix
`fo` first. Do not route around it.

## Structure

- `app/main.f90`: CLI entry point. Dispatches to check, build, test, graph, info, watch, mcp-server, lsp.
- `src/scan/`: module dependency scanner. Parses `use` and `module` statements.
- `src/dag/`: directed acyclic graph. Topological sort, reverse-dependency closure.
- `src/check/`: build + test runner. Calls backend, reports compact delta and structured diagnostics.
- `src/build/`: backend detection and dispatch (fpm, CMake). Argv execution via C shim.
- `src/cache/`: content-addressed module cache. FNV-1a hashing of source + compiler + flags + deps.
- `src/diag/`: log parser. Extracts file, line, column, target, hint from compiler and test output.
- `src/compiler/`: compiler capability detection (identity, OpenMP, module-output-dir, depfile).
- `src/mcp/`: MCP JSON-RPC server. Single `fo` tool with action dispatch, async check runs.
- `src/lsp/`: LSP server. Diagnostics on save.
- `src/run/`: coalescing run queue for save-triggered checks.
- `src/proc/`: C shim for fork/execvp process execution and source scanning.
- `src/watch/`: inotify-based file watcher.
- `doc/FO.md`: specification.
- `test/`: fpm tests.

## Rules

- Pure Fortran + C shim. No Python, no shell scripts in the build path.
- fpm project. No cmake for fo itself.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Modules under 500 lines, functions under 50 lines.
- Stage paths explicitly. Never `git add .`.
