# fo

Fortran build driver with module DAG, content-addressed cache, and
affected-test selection. `fo` is a self-contained, standalone build and test
tool: a drop-in replacement for `fpm` that reads the same `fpm.toml` manifest
but builds and tests your project natively through its own cache rather than
invoking `fpm` to build or test.

## Install

```bash
fpm install --prefix ~/.local
```

## Usage

Run `fo` in a directory with `fpm.toml`.

```
fo                  static -> build -> test (the default)
fo build            build only
fo build --native [-o app] dep.lf main.lf  compile in order and link with ffc
fo build --flag -O0 fast debug build
fo test             run tests
fo test --only-changed  run only tests affected by changes
fo test <name>      rebuild and run one test (never run build/fo/bin/* by hand)
fo exec <target> [args]  build, then run build/fo/bin/<target> with a fresh binary
fo run --native main.lf [args]  compile one source with ffc, then run it
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

Native mode requires `ffc 0.1.0` or newer. `fo build --native` accepts source
files in dependency order; the final source is the link unit. Earlier sources
are compiled separately and linked into the output. The default output is
`a.out`; use `-o <path>` to choose another path. `fo run <target> [args]`
remains an alias for `fo exec`; only `fo run --native` selects ffc.

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
3. Compute SHA-256 action IDs from source content, compiler identity, flags,
   and dependency `.mod` payload hashes.
4. Restore objects and module files from `~/.cache/fo/store/v1` on action hits.
5. Compile misses, then store action records and output payloads atomically.
6. Compute reverse-dependency closure of changed modules.
7. Run only affected tests; unchanged tests are cached.

## Project detection

`fo` searches the current directory and parents for `fpm.toml`. Non-Fortran
projects exit silently.

The `fpm.toml` manifest configures source directories, build targets, tests,
and path dependencies. The `fpm` tool is not invoked to build or test.

## Slow test exclusion

Tests named `*_slow` or `*_slow_*` are excluded by default.
Use `fo test --all` to include them. The native build enumerates test targets
and runs the non-slow subset.

## Test parallelism

`FO_JOBS=N` caps build and test fanout. Missing or invalid `FO_JOBS` falls
back to `nproc`. The native build parallelizes the module DAG through
`OMP_NUM_THREADS`; selected tests run by target name.

## Go parity

| Go feature | fo |
|---|---|
| Global content-addressed cache | `~/.cache/fo/store/v1`, SHA-256 |
| Cache key = hash(source + compiler + flags + `.mod` payload hashes) | yes |
| Affected-test selection | `fo changed`, `fo test --only-changed` |
| Parallel builds | `FO_JOBS` or nproc |
| Flag passthrough | `fo build --flag` |
| Cache clear | `fo clean` |
| Project detection | `fpm.toml` |
| Watch mode | `fo watch` (inotify) |

## Tests

Run `fo` for the full pipeline: static checks, build, tests, lint, and
format check.

## Benchmarks

Local regression suite against three synthetic workloads:

```bash
bench/run.sh                          # 7 reps, JSON lines to stdout
BENCH_REPS=3 bench/run.sh             # quick smoke run
BENCH_OUTPUT=out.jsonl bench/run.sh   # write to file
python3 bench/report.py out.jsonl     # medians, targets, pass/fail
```

Acceptance targets: no-op check under 100 ms on warm cache,
incremental leaf rebuild under 200 ms, diagnostic latency under 200 ms.

Cross-language comparison on 7 real codebases (SIMPLE 1231 files,
fortui 20, libneo 223, GORILLA 57, NEO-RT 167, sampledex 17,
fluff 178) at
[fpm-dev issue #3](https://github.com/krystophny/fpm-dev/issues/3).

Architecture: `doc/FO.md`. Linux primitives: `doc/LINUX.md`.
