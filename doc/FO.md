# fo

`fo` is the one command for logos. It is the Fortran equivalent of `go`: one
binary for builds, runs, and tests. The same binary manages checkpoints and the
content-addressed store. The shell is `fo shell` (alias `fsh`).

`fo` is not a second compiler. The split mirrors Go: the compiler compiles one
module; `fo` decides what to compile, reuses cached results, links the program,
and runs it. It arrives with F3 (`bootstrap/f3/SPEC.md`) and is
specified Fortran-native; it is implemented when the F5 hosted target lands
(`--std=f2003`), since the store, network, and process control need that
surface (`bootstrap/POLICY.md`, `bootstrap/FPM_SUPPORT.md`).

## The build core (priority 0)

The reason `fo` exists is speed. An AI agent edits and rebuilds constantly, so
rebuild latency dominates, and most of the win is build-driver work, not backend work
(`bootstrap/COMPILE_SPEED.md`).

```text
fo build        build to objects and an executable, via the cache and module DAG
fo run f.f90    build and run in one step; interactive in-memory lane on the host
fo test         build and run tests, caching binaries and results
fo bench        build and benchmark
fo fmt          format
fo doc          documentation
fo repl         a REPL with a persistent symbol table across lines
```

One content-addressed cache keys every artifact by
`hash(source + flags + compiler version + dependency .mod hashes)`. An unchanged
module is a cache hit and never recompiles. `.mod` is the export data: a `use` reads
the compiled interface, not the dependency's source. The module DAG compiles
independent modules in parallel. This is the Go build model, and the cache key is the
store key (next section).

### Recompile on edit, test what changed

logos bakes the loop in. On an edit, `fo` recompiles only the changed module and its
reverse-dependents, then discovers and runs the affected tests (the tests whose
dependency closure includes the edited module) and reports the result. The agent gets
instant feedback without naming which tests to run. Unchanged code is served from
cache; unchanged tests are not re-run.

## One store, not a second cache

The build cache is the store. The same content-addressed objects back the build, the
run capsules, the snapshots, and the scientific data (`bootstrap/STORAGE.md`). `fo`
exposes it:

```text
fo store ls / cat       inspect objects by hash
fo lock                 write fo.lock: the concretized input closure
fo env                  the active profile / generation
fo snapshot / restore   named state, cheap rollback
fo gc                   collect objects unreachable from live profiles
fo remote / push / pull / mirror / backup    warm dedup repo -> cold WORM mirror
```

The content index is a hash and the listing index is a tree; payloads are arrays
(`bootstrap/STRUCTURES.md`). The warm repo owns dedup; cold tiers are object-skip
and immutable, and sync is a Merkle diff (`bootstrap/STORAGE.md`).

## Capsules and the run economy

A run is a reproducible object, not a side effect:

```text
fo capsule init / build / run / repro    a reproducible computation
fo sweep params.toml                     a parameter sweep over capsules
fo compare runA runB                     what inputs, flags, or hashes differ
fo submit --slurm case.toml              run on an existing cluster
```

A capsule records the code, the toolchain, the run environment, inputs, outputs,
and lineage.
It is the unit the compute-vs-store decision works on: logos keeps an output or drops
it and regenerates it from the capsule (`bootstrap/ECONOMICS.md`).

## AI agent integration

An AI agent editing Fortran needs instant build and test feedback after every
edit without filling its context window. fo provides two feedback paths: a
Claude Code hook that runs `fo check` after every file edit, and an MCP server
that pushes diagnostic changes to any connected agent.

### Claude Code hooks (works today, zero code)

Claude Code fires `PostToolUse` hooks after every tool call. A hook on
`Edit|Write` runs `fo check --changed-only --json` and injects the result as
`additionalContext` into the conversation. Output is a delta from the last
green state: on success, one line; on failure, the failing test name,
assertion, and line number (2-5 lines). Context cost is constant per edit.

For background watch mode, a `FileChanged` hook with `asyncRewake: true` on
`*.f90` files runs `fo check` when any source file changes on disk. Claude is
re-woken with the result as a system reminder when the check completes. No
polling.

### Single MCP tool

fo exposes one MCP tool (`fo`) dispatched by an `action` argument, following the
same pattern as `sloppy_mail`. One tool keeps the MCP tool count low; the
discovery tree lists actions, not top-level tools.

```text
fo action=check                build + test, return compact delta
fo action=build                build only, return status
fo action=test                 run tests (all or affected), return results
fo action=test filter=<pat>    run matching tests
fo action=graph                return the module dependency graph
fo action=diagnostics          return current errors and warnings
fo action=watch start|stop     start or stop background file watching
fo action=status               return watch state and last result
```

The MCP server also exposes `fo://diagnostics` as a subscribable resource.
When a connected agent calls `resources/subscribe` on that URI, fo pushes
`notifications/resources/updated` on every file change that produces new
diagnostics. The agent re-reads the resource to get the current state.
The notification carries the URI only, not the payload; the payload is
small (the same compact delta `fo check` produces).

OpenAI tried exposing Codex as an MCP server for VS Code and abandoned it;
MCP semantics did not map to rich IDE interaction patterns (streaming
progress, diff emission, approval flows). They built a custom JSON-RPC
protocol instead (the Codex App Server). fo's MCP surface is simpler: the
tool returns a result, the resource pushes a URI. No streaming, no approval
flows. The MCP spec's resource subscription primitive is sufficient for
build-diagnostic push.

### What agents see

The feedback contract is a delta from the last green state:

```text
Green:    Build: OK (3 modules, 2 cached, 1 recompiled, 0.3s) Tests: 12 pass
Failure:  Build: OK Tests: 1 FAIL test_parser: line 42: expected 7 got 8
Recovery: Build: OK Tests: 12 pass (was: 1 FAIL)
```

Never more than 10 lines. The agent acts on failures immediately. On green,
the feedback is one line and carries no decision weight.

### Fortran language server gap

fortls fires diagnostics on save only and uses a regex-based parser; it
detects scope-level errors (duplicate definitions, unknown modules, unclosed
blocks) but not full semantic or type checking. A replacement based on
LFortran is under early development (v0.0.6, April 2025). fo does not depend
on either; `fo check` invokes the compiler directly and reports real build
errors, not approximations.

### What existing agents use without fo

Claude Code runs builds via shell commands (the Bash tool). Codex CLI runs
shell commands in a sandbox. Neither has built-in Fortran awareness. Without
fo, the agent runs `fpm build && fpm test` and parses the full compiler
output. With fo, the agent gets a compact, cache-aware, affected-tests-only
delta. The hook integration means the agent does not even need to call a
build command; the feedback arrives automatically after every edit.

## Surfaces

The interface is `fo` too (`bootstrap/os/UI.md`):

```text
fo shell        terminal, agent loop (alias fsh)
fo web          slopshell-style web canvas
fo agent        autonomous edit / compile / test / patch loop
fo voice        push-to-talk and dictation, STT/TTS over REST
fo mcp-server   single-tool MCP server (fo action=...)
```

Tools, skills, and resources are exposed through the discovery tree
(`bootstrap/STRUCTURES.md`): a small entry set, balanced fanout, summaries first,
schema on demand, search over enumerate. The meta-tools (`fo_list`, `fo_describe`,
`fo_search`, `fo_call`, `fo_use_skill`) are the only entry points.

## Language surface

`fo` targets standard Fortran by default. The lazy / inferred surface is defined in
the `lazy-fortran/standard` repository (`--std=lf` plus `--infer`): an implicit
program wrapper for bare statements, the walrus `:=` and first-assignment type
inference, `intent(in)` by default, and 8-byte real / 4-byte integer defaults, all
lowering to strict standard Fortran. `fo` and F3 consume that definition; this project
does not duplicate it.

## Relationship to fpm

`fo` is a standalone build and test tool, not a wrapper around `fpm`. It
conforms to the `fpm.toml` manifest standard (`bootstrap/FPM_SUPPORT.md`): it
consumes `fpm.toml` as the project descriptor (sources, targets, tests, path
dependencies) and writes `fo.lock`. It does not invoke the `fpm` tool to build or
test; it compiles the module DAG natively through its own content-addressed cache,
using `fpm.toml` as the native project configuration. The manifest format is the
shared contract; the cache store and command surfaces are `fo`'s own.

## Relationship to fortrun

`fo` supersedes the earlier fortrun project as the runner and orchestrator for Fortran builds.
Fortrun was experimental and on hold; `fo` implements the intended role of a build driver with
dependency resolution, caching, and `fpm.toml` manifest support, but does so more comprehensively and is
the active path for the lazy-fortran compiler bootstrap. Nothing from fortrun merits porting:
`fo`'s module scanner is more complete (handles submodules and external procedures), its
dependency resolver is more efficient (transitive closure with deduplication), and its caching
is stronger (content-addressed by source, compiler, flags, and `.mod` payloads vs. fortrun's
file-based locking). The fortrun repository is superseded and no longer the active path.

## Status and interim loop

The `fo` build core landed (#904: content-addressed cache, incremental rebuild,
affected-test selection) as disposable host scaffold in `tools/fo.py`. The
Fortran-native `fo` is the F5 goal; until it arrives, the fast loop is gfortran on
the host, which builds F2 (and builds F3) byte-identical to the trusted chain
(`bootstrap/COMPILE_SPEED.md` "The interim loop"). The interim host tools written in
other languages are disposable scaffolding, marked for replacement when the
Fortran-native `fo` arrives at the F5 hosted target (`bootstrap/POLICY.md`).

Refs: `bootstrap/COMPILE_SPEED.md`, `bootstrap/f3/SPEC.md`, `bootstrap/STORAGE.md`,
`bootstrap/STRUCTURES.md`, `bootstrap/LEVELS.md`, `bootstrap/COMPRESSION.md`,
`bootstrap/ECONOMICS.md`, `bootstrap/PARALLELISM.md`, `bootstrap/os/UI.md`,
`bootstrap/FPM_SUPPORT.md`.
