# fo

Fortran build cache, incremental rebuild, affected-test selection, MCP server.

## Gremlin mode and plans

Read [PLAN.md](PLAN.md) for the complete Gremlin provider/bootstrapping plan,
[ROADMAP.md](ROADMAP.md) for adjacent ownership, and workspace master PLAN/AGENTS
when available. The current Gremlin/storage/test-language program is explicitly
authorized for implementation and verification. Parallel mode uses a luna
coordinator and isolated
luna workers; serial mode stays in the main session without subagents. Repeated
single-task escalation uses native Sol in parallel or the GPT skill in serial.

Gremlin uses one shared CLI/MCP/background engine. Use the updated CLI after fixes
if MCP cannot reload. Fix fo workflow problems rather than bypassing them. Keep
worker worktrees/builds isolated, attach concurrent same-place starts to one owner,
preserve complete CAS keys/atomic link artifacts and bound generated disk use.
Only the controller promotes main or the globally installed tool.

Dogfood one bounded resident fo gremlin integration lane when the active storage/
execution contracts make it safe. During development, use Gremlin and focused
local behavioral oracles; never wait for GitHub CI while independent work exists.
CI is asynchronous post-submit evidence, not a Gremlin feature. Push each exact
locally green integrated increment to main; pause unrelated promotions for a
confirmed current regression.

All test programs, orchestration and assertions are Fortran. Small test-only C
OS shims are allowed. Remove JavaScript fixtures only after equivalent independent
Fortran behavioral oracles pass.

## Build and Test

Use `fo` for every edit/build/test loop. Never call `fpm`, `make`,
or compiler commands directly except to diagnose a `fo` failure.

```bash
fo                # staged pipeline: static -> build -> test -> lint -> fmt --check
fo check          # build + test, one-line status
fo check --json   # JSON status
fo build          # build only
fo test           # run tests
fo exec <t> [args] # build, then run <t> (never run build/fo/bin/* by hand: may be stale)
fo exec --release --no-build <t> [args]  # run the fo build --release binary
fo lint           # unused imports + gfortran warnings
fo lint --json    # lint results as JSON
fo fmt            # format sources (native, 88 col, 4 sp)
fo graph --dot    # module DAG in Graphviz DOT format
fo install        # fpm install --prefix ~/.local
fo install --prefix /path  # install to custom prefix
```

If `fo` cannot handle the project, fix `fo` first. Do not route around it.

## Structure

- `app/main.f90`: CLI entry point. Dispatches to check, build, test, lint, fmt, graph, info, install, watch, mcp-server, lsp.
- `src/scan/`: module dependency scanner. Parses `use` and `module` statements.
- `src/dag/`: directed acyclic graph. Topological sort, reverse-dependency closure.
- `src/check/`: build + test runner (`fo_check`) and output formatters (`fo_check_output`).
- `src/build/`: native build and test dispatch from `fpm.toml`. Argv execution via C shim.
- `src/cache/`: SHA-256 content-addressed action and binary cache.
- `src/lint/`: native linter. Unused-import detection (`fo_lint`), short-circuit
  reliance detection (`fo_lint_shortcircuit`), test programs with no failure
  path (`fo_lint_testfail`, over the project's failing helper procedures
  collected by `fo_lint_failpath`), and gfortran compiler warnings (stack-size
  filtered, deduplicated). Text-level rules match on masked code from
  `fo_lint_lex` (comments and string literals blanked), never on raw file text.
  See "Lint scope" below for what does *not* belong here.
- `src/diag/`: log parser. Extracts file, line, column, target, hint from compiler and test output.
- `src/compiler/`: compiler capability detection (identity, OpenMP, module-output-dir, depfile).
- `src/mcp/`: MCP JSON-RPC server (`fo_mcp`), including response building.
- `src/lsp/`: LSP server. Diagnostics on save.
- `src/run/`: coalescing run queue for save-triggered checks.
- `src/proc/`: C shim for fork/execvp process execution and source scanning.
- `src/watch/`: native file watcher (inotify on Linux, kqueue on macOS).
- `doc/FO.md`: specification.
- `test/`: project tests.

## MCP Server

`fo mcp-server` exposes a single `fo` tool over JSON-RPC/stdio. Actions: `check`, `build`, `test`, `lint`, `fmt`, `info`, `graph`, `changed`, `clean`, `status`, `diagnostics`, `cancel`. Optional `dir` parameter targets a specific project directory.

Protocol: auto-detects input framing (Content-Length headers or bare JSON lines) from the first message and mirrors it. Protocol version is echoed from the client's `initialize` request.

System test: `FO=/path/to/fo fo test test_mcp_system`. The standalone Fortran
client tests both framing modes, protocol negotiation, tool calls, error paths,
and clean shutdown through the public `fo mcp-server` process.

Key source files:
- `src/mcp/fo_mcp.f90`: server loop, dispatch, async state.
- `src/proc/fo_process.c`: `fo_c_read_jsonrpc_message` (framing auto-detect), `fo_c_get_mcp_framing`.
- `src/util/fo_util.f90`: `send_jsonrpc` (output framing follows input).

## Lint scope

fo and fluff split source analysis the way the Go toolchain splits `go vet`
from staticcheck, or Rust splits cargo from clippy.

**fo owns the cheap always-on tier.** Text-level checks that need no frontend,
run on every `fo` invocation, and must work when nothing else is installed:
unused imports, short-circuit reliance, test programs that cannot fail the
build, and gfortran's own warnings. This tier stays small deliberately. Adding a rule here is only correct if it needs no
parse tree.

**fluff owns everything that needs an AST.** Type-aware rules, dead-code
analysis, column-major access patterns, style rules over real syntax. fluff
depends on FortFront; fo does not, and that is the point. Reaching those rules
goes through `fo lint --deep`, which runs `fluff check --output-format json` as
a subprocess and merges its findings into fo diagnostics (#59).

Consequences, both directions:

- Do not add an AST-based rule to fo. If a rule needs to know a type, a scope,
  or a declaration, it belongs in fluff.
- Do not reimplement fo's two native rules in fluff. They must keep working
  with no fluff on the system.
- `fo build` and `fo test` never invoke fluff. Only the quality commands do, so
  the bootstrap path stays free of a FortFront dependency.

`fo fmt` follows the same shape: fo wraps fprettify rather than implementing
formatting, exactly as it would wrap fluff for deep lint.

## Rules

- Pure Fortran + C shim. No Python, no shell scripts in the build path.
- fpm project.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Keep new routines focused and split new subsystems into cohesive modules.
- Stage paths explicitly. Never `git add .`.
