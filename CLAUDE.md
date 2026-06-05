# fo

Fortran build cache, incremental rebuild, affected-test selection, MCP server.

## Build and Test

Use `fo` for every edit/build/test loop. Never call `fpm`, `cmake`, `make`,
or compiler commands directly except to diagnose a `fo` failure or confirm
backend parity after a `fo` fix.

```bash
fo                # staged pipeline: static -> build -> test -> lint -> fmt --check
fo check          # build + test, one-line status
fo check --json   # JSON status
fo build          # build only
fo test           # run tests
fo lint           # unused imports + gfortran warnings
fo lint --json    # lint results as JSON
fo fmt            # format sources (fprettify, 88 col, 4 sp)
fo graph --dot    # module DAG in Graphviz DOT format
fo install        # fpm install --prefix ~/.local
fo install --prefix /path  # install to custom prefix
```

If `fo` is slower than the backend or cannot handle the project, fix
`fo` first. Do not route around it.

## Structure

- `app/main.f90`: CLI entry point. Dispatches to check, build, test, lint, fmt, graph, info, install, watch, mcp-server, lsp.
- `src/scan/`: module dependency scanner. Parses `use` and `module` statements.
- `src/dag/`: directed acyclic graph. Topological sort, reverse-dependency closure.
- `src/check/`: build + test runner (`fo_check`) and output formatters (`fo_check_output`).
- `src/build/`: backend detection and dispatch (fpm, CMake). Argv execution via C shim.
- `src/cache/`: content-addressed module cache. FNV-1a hashing of source + compiler + flags + deps.
- `src/lint/`: linter. Unused-import detection + gfortran compiler warnings (stack-size filtered, deduplicated).
- `src/diag/`: log parser. Extracts file, line, column, target, hint from compiler and test output.
- `src/compiler/`: compiler capability detection (identity, OpenMP, module-output-dir, depfile).
- `src/mcp/`: MCP JSON-RPC server (`fo_mcp`) and response builders (`fo_mcp_response`).
- `src/lsp/`: LSP server. Diagnostics on save.
- `src/run/`: coalescing run queue for save-triggered checks.
- `src/proc/`: C shim for fork/execvp process execution and source scanning.
- `src/watch/`: inotify-based file watcher.
- `doc/FO.md`: specification.
- `test/`: fpm tests.

## MCP Server

`fo mcp-server` exposes a single `fo` tool over JSON-RPC/stdio. Actions: `check`, `build`, `test`, `lint`, `fmt`, `info`, `graph`, `changed`, `clean`, `status`, `diagnostics`, `cancel`. Optional `dir` parameter targets a specific project directory.

Protocol: auto-detects input framing (Content-Length headers or bare JSON lines) from the first message and mirrors it. Protocol version is echoed from the client's `initialize` request.

System test: `node test/test_mcp_system.js` (or pass a binary path as arg). Tests both framing modes, protocol negotiation, tool calls, error paths, and clean shutdown.

Key source files:
- `src/mcp/fo_mcp.f90`: server loop, dispatch, async state.
- `src/mcp/fo_mcp_response.f90`: JSON-RPC response builders.
- `src/proc/fo_process.c`: `fo_c_read_jsonrpc_message` (framing auto-detect), `fo_c_get_mcp_framing`.
- `src/json/fo_json.f90`: `send_jsonrpc` (output framing follows input).

## Rules

- Pure Fortran + C shim. No Python, no shell scripts in the build path.
- fpm project. No cmake for fo itself.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Modules under 500 lines, functions under 50 lines.
- Stage paths explicitly. Never `git add .`.
