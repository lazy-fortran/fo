# fo

Fortran build cache, incremental rebuild, affected-test selection, MCP server.

## Build

```bash
fpm build
fpm test
fpm run -- check
fpm run -- graph
```

## Structure

- `app/main.f90`: CLI entry point. Dispatches to check, build, test, graph.
- `src/scan/`: module dependency scanner. Parses `use` and `module` statements.
- `src/dag/`: directed acyclic graph. Topological sort, reverse-dependency closure.
- `src/check/`: build + test runner. Calls fpm, reports compact delta.
- `src/cache/`: content-addressed build cache (planned).
- `src/mcp/`: MCP server, single tool with action dispatch (planned).
- `doc/FO.md`: specification.
- `doc/LINUX.md`: Linux architecture (kernel primitives, no custom OS).
- `test/`: fpm tests.

## Rules

- Pure Fortran. No Python, no shell scripts in the build path.
- fpm project. No cmake.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Modules under 500 lines, functions under 50 lines.
- Stage paths explicitly. Never `git add .`.
