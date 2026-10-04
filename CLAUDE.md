# fo

Fortran build cache, incremental rebuild, affected-test selection and CLI/MCP.

## Product boundary

Keep fo a focused Fortran development tool: build, test, run, incremental/cache-aware
rebuilds, affected/continuous testing, formatting, lint/static diagnostics,
LSP/MCP convenience, dependency/bootstrap support needed by those commands, and
their correctness/performance infrastructure.

Do **not** expand fo into a Nix/Spack replacement, hermetic environment manager,
container/sandbox platform, scientific workflow/provenance/capsule system,
proof/synthesis framework, cluster scheduler, CI/promotion engine or coding-agent
orchestrator. Historical capsule/Nix/FortOS issues #1–#7 are closed;
#120 and #157 are closed not planned for the same boundary.

Record compiler/configuration identity sufficiently for cache invalidation and
diagnostics, but do not recursively capture system toolchains, sysroots, dynamic
loaders or host runtime libraries. Users needing a hermetic environment provide
one externally. Repository-internal third-party/performance audits stay under
`bench/` and CI; they are not new public environment-management commands.

## Read first and keep the development loop fast

[PLAN.md](PLAN.md) is the authoritative current provider and execution plan;
[ROADMAP.md](ROADMAP.md) maps adjacent issue ownership. Read workspace master
PLAN/AGENTS when available and preserve their host restrictions.

Implementation and focused verification are active for Gremlin, shared storage,
native tests, standalone FPM compatibility and the later defined ITpPlasma CMake
subset. Follow PLAN's stage order and continue assigned work without a new
execution confirmation. External controllers own coding agents, task DAGs, worktrees,
model selection and integration. Parallel mode uses a luna coordinator and
isolated luna workers; serial mode stays in the main session. After two
substantive failed attempts on one task, freeze its evidence and transfer only
that task to Sol, following the current workspace skill/instructions.
`fo work` is abandoned; do not recreate an agent or CI scheduler inside fo.

For lazy-fortran, push small exact integrated increments to main after their
focused local correctness gate passes and no known current regression remains.
Never wait for GitHub CI, a complete Gremlin epoch, a benchmark or a project
matrix while implementation can proceed. A confirmed current correctness
regression gets repair/revert priority; independent coding continues.

**Benchmarks are not a development/main/merge gate.** #190 owns real-edit and
edit-to-verdict measurements; #145 owns pinned automatic third-party acquisition.
They run only in independent scheduled/manual CI or an explicit slow/performance
audit, outside ordinary fo/test/default Gremlin inventories. Timing thresholds
are advisory. Small deterministic audit-harness tests are ordinary correctness;
large runs, hardware thresholds and downloads are not. Do not label a failed
measurement PASS or confuse benchmark warnings with product correctness.

## Build and test

Use the exact controller-supplied fo candidate for the edit/build/test loop.
Verify its immutable absolute path and SHA256 before validation; no PATH,
installed-tool or newest-file fallback. Keep builds/caches warm and run the
named affected correctness oracles, not a full pipeline after every edit.

```bash
fo build                    # build applications/examples
fo test NAME                # named focused correctness test
fo test --only-changed      # affected selection; see #189 completeness limits
fo test --all               # explicitly include slow correctness tests
fo                          # start/attach default resident Gremlin lane
fo verify                   # broad staged verification checkpoint
fo check --json             # structured build/test status
fo exec TARGET [ARGS...]    # resolve/build and execute through fo
fo exec --release --no-build TARGET [ARGS...]
fo lint
fo lint --json
fo fmt                      # native formatter
fo graph --dot
fo install --prefix /path   # explicit controlled installation only
```

Bare `fo` starts or attaches to the default resident Gremlin lane. Explicit
controller-assigned lanes use `fo gremlin start --lane development` (or the
assigned lane ID). `fo verify` runs the former staged pipeline at checkpoints.

Commands above use `fo` as shorthand for that selected driver. Never execute a
wildcard/newest build artifact to bypass freshness. Use the updated CLI after a
fix when MCP cannot reload; do not wait for a new agent session.

Do not bypass a broken fo path with fpm/make/direct compiler calls in the normal
development loop, except to diagnose it. Explicit bootstrap and #145 independent
upstream reference audits are different: they may run documented pinned fpm or
CMake/CTest/toolchain commands in owned isolated directories and record that
route. A reference fpm binary and fpm under test must be distinct to prevent
self-recursion. Upstream preprocessing tools such as Fypp remain declared audit
prerequisites, not fo-owned orchestration.

## Shared architecture and correctness boundaries

Ordinary fo and Gremlin must use one input/session/build/test service and the
shared fx immutable store (#165-#168). Gremlin adds watching, last-compilable
retention, supersession and finite coverage. CLI/MCP are adapters. A one-shot
check must not become an endless campaign. No second cache, database, bulk RAM
cache, metrics daemon, GitHub-polling service or promotion engine.

#175 owns canonical logical execution inputs and declarations. #189 derives
conservative affected tests from a frozen baseline/candidate delta, never from
post-build cache misses. Compile-interface, link-implementation and test/oracle
inputs are distinct. A cached build is not a cached PASS. Unknown selection
widens supported local correctness tests or reports incompleteness; it does not
silently accept one random case or download benchmark projects.

Run unresolved reproducers first, then all affected/mandatory cases, then the
finite unseen background epoch. Preserve slow affected correctness requirements
and exact scope/counts. Do not truncate requirements to a campaign chunk.
Current-generation observations and project-input/impact-model completeness are
separate facts. Repository governance remains external; hermetic host closure is
not a fo concept.

Only successfully built immutable candidates replace the active generation.
Failed candidates publish diagnostics and receive no tests; last-compilable
work continues. Keep source views immutable and use private writable execution
scratch. Each running process leases the exact project build outputs and declared project
assets it needs. Cancellation
and cleanup touch only proven-owned state; no broad pkill or shared-cache purge.
Keep documented platform containment limits honest.

Dogfood a bounded resident lane when supported storage/execution contracts make
it safe. Shared admission bounds total nested work; coding-worker count must not
multiply FO_JOBS without limit. Optional #190 measurements guide tuning, not
admission of unrelated correct changes. Only the controller may publish main
or deliberately replace a globally installed driver; worker self-builds stay
private.

## Code ownership

- `src/build/`: backend/context/DAG/compile/link/test services; #167 consolidates.
- `src/scan/`, `src/dag/`: discovery and dependency graph; #175/#186 unify policy.
- `src/cache/`: thin shared-store bindings and validated metadata memoization.
- `src/run/`: Gremlin generation, coverage, readiness, receipts and lifecycle;
  #149 removes unrelated responsibilities from the supervisor.
- `src/watch/`: shared event provider; #148 removes competing watch semantics.
- `src/proc/`: narrow OS process/ownership boundary; no duplicate source policy.
- `src/mcp/`: framing/envelopes/adapters; #185 removes transport-owned scheduling.
- `src/lsp/`: LSP behavior; #56 unsaved diagnostics remains adjacent scope.
- `src/check/`, `src/diag/`: public result assembly and diagnostics.
- `src/lint/`, `src/fmt/`: cheap native lint and formatting.
- `test/`, `test-support/`, `test-fixtures/`: independent native oracles/helpers.
- `bench/`: explicit audit tool; #190 corrects touch-only measurement semantics.

`fo mcp-server` exposes one fo tool. Query current capabilities rather than
assuming every planned field/backend is implemented. Preserve both supported
framing forms, request/error semantics, typed waits and lossless paged output.
Native clients test the public process boundary; independent parsing must not
share the production defect it is meant to detect.

## Lint scope

Keep cheap text-level checks and compiler warnings available without an AST
provider. Rules requiring type/scope/AST analysis belong in fluff/FortFront and
are explicitly opt-in through #59; build/test never automatically invokes them.
Do not reimplement a second AST analysis stack in fo to improve impact ranking.
Optional runtime coverage is a later audit experiment, not default instrumentation.

## Implementation and test rules

- Fortran with narrow C OS shims; fo-owned tests, generated fixture programs,
  assertions and benchmark orchestration remain Fortran.
- YAML/TOML and minimal workflow command glue are declarative integration.
  Upstream reference tools keep their documented dependencies.
- All arguments have intent; derived types end in `_t`; use explicit imports.
- Use `real(dp)` with `dp => real64` from iso_fortran_env where appropriate.
- Keep modules cohesive; do not split solely to satisfy a line-count target.
- Stage explicit paths, never `git add .`; preserve other workers' ownership.
- Preserve useful independent negatives during #161/#163 native migration.
  #184 removes redundant setup/meta assertions, not legitimate failures.
- Do not add tests asserting PLAN/issue text, file counts or module layout.
  Generated documentation delivered by a public command may be real behavior.
- Use observable barriers and bounded safety deadlines instead of arbitrary
  sleeps. Safety timeout bounds are not performance speed thresholds.

Update PLAN/ROADMAP and affected issues with exact source/driver identities,
selected checks, actual outcomes and remaining limitations. Do not repeat large
historical narratives or claim measurements from fixture constants. Code and
focused verification proceed while optional audits run.
