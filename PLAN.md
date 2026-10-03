# fo Gremlin implementation plan

Updated 2026-10-03; planning input `2e781fdf79db869470849a199fb25ba36476c7c7`.
This is the complete provider plan for Gremlin, fo's continuous randomized
regression-testing engine and serial/parallel worker controls. The
[ffc roadmap](https://github.com/lazy-fortran/ffc/blob/main/PLAN.md) depends on
this stage **before further compiler work**. This document is a plan; new
interfaces below are proposed, not implemented by this documentation change.

## Goal and smallest architecture

Run useful tests continuously while humans or coding workers prepare the next
change. Test immutable generations, keep the last compilable generation running
through failed candidate builds, and preempt obsolete tests immediately when a
new successfully built generation is ready. Preserve every completed observation
and rerun old failures first against the newest generation. Editing never waits
for a whole suite.

Use **one shared engine**, one owned supervisor per project session, and thin
CLI/MCP adapters. Reuse fo's dependency DAG, action cache, process layer, project
locks and dispatcher. No separate ffc scheduler, provider-specific daemon,
second test runner or model-driven per-test polling loop is needed. Gremlin is
continuous randomized testing, not fault-injection chaos engineering or input
fuzzing. Start with a small working serial service; add capacity using the same
state machine rather than a parallel-only rewrite.

## Project map and current mechanisms

| Area | Current files and behavior |
| --- | --- |
| CLI | `app/main.f90`; `fo test --random N --seed S`, named/all and `--only-changed` exist; random cannot yet combine with names/affected selection. |
| Selection | `src/check/fo_test_random.f90`; seeded sampling without replacement. Sampling membership and actual execution order are distinct. |
| Build/test | `src/build/fo_build_backend.f90`, `fo_gfortran_build.f90`; DAG/action caches and FO_JOBS; native team verdict reporting waits for the team. |
| Async queue | `src/run/fo_run_queue.f90`; one pending request coalesces but waits for active completion. |
| Watch | `src/watch/fo_watch.f90`; blocks in check and watches Fortran suffixes. |
| MCP | `src/mcp/fo_mcp.f90`; async check/status/diagnostics/cancel exists, test is synchronous, selection fields are missing, progress polling depends on new requests. |
| Process lifecycle | `src/proc/fo_process.c` and `.f90`; asynchronous cancel signals the parent and can wait indefinitely; bounded synchronous group termination is a reusable starting point. |
| State/output | `src/check/fo_test_results.f90`, `fo_check_output.f90`, `src/lock/fo_lock.f90`; JSON truncation and receipt durability need behavioral verification. |
| Shared cache | `src/cache/fo_cache.f90`; FO_CACHE_DIR or the normal user cache; atomic action publication and complete action keys must remain authoritative. |

Read `CLAUDE.md`, this plan, the active issue and its verifier first. Load the
relevant portions of `doc/FO.md` when a public build/cache/process contract is
changed. New cohesive modules may live under `src/run/`; avoid expanding
`app/main.f90` or `fo_mcp.f90` into independent scheduler implementations.

## Public interface and parity

Keep the public controls small. Proposed CLI:

```bash
fo gremlin                          # start/attach; promptly return owner ID
fo gremlin start --background        # owned detached supervisor
fo gremlin status --json
fo gremlin wait --failure --json     # cursor-based bounded/event wait
fo gremlin failures --json
fo gremlin reproduce ID
fo gremlin stop
fo test --continuous                # alias into the same engine
```

The start policy accepts explicit targeted tests, affected-test selection,
`--random N`, `--seed S`, a distinct `--shuffle`, campaign/per-case budgets,
lane identity and build/test capacity. `--jobs` controls process capacity, not
model-worker count. External agent controllers supply task semantics, worktrees,
model selection and integration; fo supplies build, test and Gremlin feedback.
CLI capability discovery must distinguish currently implemented and proposed
controls and reject unsupported parameters.

Extend the existing **single fo MCP tool** with equivalent start, status,
wait/events, failures, reproduce and stop actions/fields. CLI and MCP
must call the same request validator, scheduling policy, generation handling,
receipt reader and process lifecycle. All capabilities, cancellation semantics,
errors and JSON/event schemas must match; do not put essential logic in MCP.
Background execution is an engine lifecycle, not a third policy implementation.

**CLI must always offer the complete feature set.** After a fo fix, use the
newly built/refreshed CLI immediately if the attached MCP cannot reload. Do not
wait for a new agent session or route around fo. A stale MCP can report its
version/capabilities; it cannot advertise features it lacks. CLI can stop/restart
or reconnect to the supervisor using a versioned state protocol, preserving
receipts and ownership. Protocol mismatch must fail clearly and provide the
CLI recovery path. Reload/restart never guesses that abandoned PIDs are safe to
kill. Test the stale-MCP/new-CLI scenario explicitly.

Start returns a stable run/session ID promptly. Status reports active generation,
candidate build, lane/task, capacity leases, progress, coverage debt and
unclassified/current failures. Wait uses event cursors, bounded client timeouts
and cancellation rather than busy polling; no client request is necessary for
the engine to make progress. Reproduce restores the recorded case, seed/order
prefix, flags, environment and oracle, defaulting to the newest generation;
allow explicit historical reproduction. Stop is scoped and bounded.

## Generations and state machine

A generation identifies a frozen input closure:
`H(source, includes, manifests, tests/oracles, toolchain, flags, dependencies,
runtime libraries, harness, relevant environment)`, plus its built artifacts.
Freeze before building or prove the entire captured input vector stayed
unchanged; a build that observed changing inputs cannot be published. Store exact
base commits and patch digests, including untracked inputs. A clean commit is
convenient, but a reproducible uncommitted hashed patch is valid evidence.

Each lane keeps an editable worker checkout, a frozen candidate build and a
last-compilable test generation. The protected integration lane has the same
structure. Runtime/compiler libraries and sibling path dependencies are part of
the bundle, not live symlinks into another worker's changing tree. A binary copy
alone is insufficient. ffc's gauntlet fingerprints the checkout containing its
harness even with `--ffc`, so freeze that harness/source/runtime together.

1. Start/attach, validate inventory and resource leases, recover owned state.
2. Coalesce edits into the newest candidate input; incrementally build it in
   isolated materialization while tests continue on the last compilable version.
3. If build fails, publish the build failure and retain useful testing. Never
   relabel last-compilable as verified good, and never test an unbuilt candidate.
4. If build succeeds, atomically publish its artifact/input identity, preserve
   old completed receipts, cancel only this lane's obsolete test campaign, and
   start reproducers/affected tests then a short randomized subset.
5. On another successful generation, repeat immediately; do not drain old work.
   If a subset finishes without a new build, start another seeded subset.
6. Explicit stop/shutdown releases only owned processes and leases. Restart
   recovers receipts and coverage position; partial execution remains unknown.

The switch from old to new test generation is not promotion to main and does
not mean all tests passed. Key cancellation by session/lane/generation/artifact,
not a global timestamp. A worker update must not stop another worker or the
integration campaign. Candidate bursts can skip intermediate unpublished builds;
already published failures always survive.

## Test policy and failure response

Default campaign limit: 60 seconds, at most 32 exploratory cases, one corpus
process per campaign and five seconds per ordinary corpus case. Mandatory
reproducer/neighbor checks have their own small budget. These are starting
settings to measure, not a reason to hide slow tests. A newer successful build
preempts even a campaign with an exceptional budget. Random membership,
shuffled dispatch order and parallel completion order are reported separately.

Priority: confirmed current regressions, impacted tests, historically
failure-prone tests, then unexplored randomized cases. Deduplicate the mandatory
and sampled sets; never sample away a requested reproducer. Stratify suites and
feature/negative-case families, shuffle without replacement, record the seed and
exact selected/started order, and rotate starting strata across short campaigns.
Reserve about a quarter of exploratory slots for least-recently completed,
repeatedly cancelled and expensive cases. Coverage debt persists across versions;
correctness verdicts do not automatically persist.

When an old generation fails, record case, generation, action key, configuration,
seed/order prefix, environment, exit/output and signature. Do not interrupt an
uncommitted edit merely because the background runner went red. Rerun the exact
reproducer first on the newest compilable version. If it passes, keep working;
if it fails reproducibly, assign a compatible fixer ahead of feature work. In
parallel mode unrelated workers continue; serial mode fixes it next. Flaky or
order-dependent results get replay and an owned repair issue, never silent
acceptance. Restore/revert a reproducibly broken integrated build before unrelated
promotion. A failing candidate does not advance protected integration.

Old failures are reproduction priorities. A successful cached action may be
reused only when its full input/dependency/oracle closure and environment key
remain valid. Report cache reuse distinctly; a matching test name or old PASS is
insufficient. Missing suites, zero selected cases, dropped dependency edges and
unavailable oracles fail inventory/evidence checks rather than creating green.

## Durable observations and ownership

Publish each completion to an append-only journal before waiting for another
case. Preserve original public test names, exit status, output artifacts,
duration, seed/order and version/action identity. Separate completed oracle
pass, known failure, candidate/confirmed regression, cancellation/untested,
flaky, timeout, OOM and infrastructure/build errors. Cancellation is not a
behavioral failure or pass. Summary must expose observed/selected/untested
counts and remain partial until the exact required set completes.

Fsync/atomic publication policy must make completed receipts survive supervisor
crash; recover a truncated tail without inventing or duplicating completions.
Large stdout/stderr stays in referenced artifacts rather than an unbounded MCP
response. Event cursors page losslessly beyond old 16 KB/256-entry limits.
Compare only compatible same-case observations. Never union different compiler
versions into a synthetic full green. The integration journal is authoritative;
worker journals support review and triage.

Give every campaign a dedicated owned process group/session, scratch directory
and process start identity. TERM, at most two seconds grace by default, then
KILL/reap surviving owned children; verify termination before reuse. Contain
escaping descendants if the advertised contract promises whole-tree cleanup.
Never broad-pkill, remove another process's lock, reuse a live build directory,
or assume a PID alone proves ownership. Preserve proven-owned lock/cache
invariants through crash recovery. Cache cleanup cannot race active leases.

## External controller orchestration

Use one controller, a protected integration worktree and one writable Git
worktree per implementation/fix worker. Read-only investigators need no extra
checkout. Workers may commit on their task branches, returning exact commits or
base-plus-patch digests; they never self-promote to main. Independent worktrees
share Git objects and valid fo CAS actions, with private mutable build trees.
Keep path dependencies intentionally bundled rather than accidentally using an
unpublished sibling checkout.

Serial or parallel coding modes belong to the external controller. It owns
file/API/ABI/registry conflicts, task dependencies, model selection, worktree
creation and escalation. Distinct worktrees
also isolate alternative approaches; overlapping results still integrate in an
explicit order. Worker capacity and total compiler/test resource capacity are
separate budgets. Use shared admission/leases across nested pools so N workers
never silently create N times FO_JOBS times sixteen corpus jobs.

On worker result A, construct integration candidate D+A in a temporary worktree
or immutable candidate commit. Run current regression reproducers and the union
of affected tests before advancing D. Return a failing candidate to its worker;
leave protected integration unchanged. A fast targeted pass advances integration
and starts its randomized campaign, not a full-green claim. Integrate one
candidate at a time; unaffected workers continue. Eventually run comprehensive
verification on a fixed final generation without pausing useful implementation
merely to drain an obsolete suite.

Use available parallel luna capacity when parallel mode is selected; after repeated failure
on an **individual task**, transfer that task to sol. Record failure signatures
and at least two substantive repair attempts, interrupt the previous writer,
freeze its useful patch/evidence, then give sol the same task, verifier and ownership. Parallel mode uses a
native Sol worker; serial mode uses the GPT skill for a bounded Sol task and
returns evidence to the same main session. Do not spawn coding subagents in
serial mode. Read the current GPT skill before that exception. Ordinary first
test failures are feedback, not immediate escalation.
Do not change the whole team's model or give two agents the same writable branch.
Root/controller remains responsible for integration, main commits and pushes.

## Cache, concurrent starts and disk budget

Use one storage architecture for ordinary fo and Gremlin:

```text
immutable fx store: blobs + tree manifests + action-result manifests
                              │
                              ▼
compact fo input generation → lane-private transactional build session
                              │
                 ┌────────────┴────────────┐
                 ▼                         ▼
        one-shot fo build/test      Gremlin supervisor loop
```

Ordinary fo and Gremlin call the same one-generation build/test engine. Gremlin
adds watching, last-compilable activation, coverage and ownership policy; it
does not own another cache or recursively run a public fo build inside a shared
writable generation directory. Generation identity is a compact manifest over
shared immutable source/dependency blobs. Mutable object/module/link output
lives in a lane/session-owned working directory and becomes usable only through
an atomic successful-session commit.

The shared store uses raw content IDs for bytes and canonical manifests for
roles, modes, names and tree/action structure. Publishing an existing verified
blob performs no rewrite. Equal action/result publication is idempotent; one
action ID producing different result IDs is durable nondeterminism or an
incomplete-key error, never last-writer-wins. Optional Linux reflink and macOS
clone materialization are optimizations with byte-copy fallback. No database,
cache daemon or application-level bulk RAM cache is introduced; the filesystem
and OS page cache remain the storage substrate.

Canonicalize project/worktree identity and lane/config namespace. One native
supervisor owns the worktree session and lane registry. Two starts in the same
namespace must acquire-or-attach **one** owner atomically, returning its stable
session ID; incompatible policies return a clear conflict, never a second owner. Distinct lanes can coexist within global leases. PID/start
identity, state protocol and active ownership prevent stale-lock/PID accidents.
CLI and MCP starting simultaneously use the same path. A racing start/stop/build
publication must yield a valid owner and explicit result, not orphaned children
or an invented pass. Current-generation pointers and CAS outputs publish atomically. Static-library
archives, shared libraries and executable link outputs also build under owned
temporary names, validate, then atomically publish; a racing linker sees complete
old or complete new output. Existing keyed artifact pathname alone is insufficient.
[fo #144](https://github.com/lazy-fortran/fo/issues/144) owns the observed unsafe
archive publication/partial-reuse path; preserve source evidence and do not
assume every undefined symbol has the same cause.

Reuse the existing content-addressed compiler and successful-test caches with
complete keys and atomic publication. Snapshotting must not destroy fast
incremental reuse. Identical inputs should attach/reuse without rebuild; changed
includes, path dependencies, compiler flags, link/runtime inputs or oracle data
must invalidate affected actions. Avoid global tool installs/shared-library mutation inside worker tasks.
Worker self-builds must not auto-refresh the globally installed fo candidate;
provide an explicit opt-out and isolate linked-worktree builds from authoritative
CLI publication. Only the controller promotes the installed tool. Concurrent independent worktrees may read shared
immutable cache entries; publication/cleanup respects active leases.

Keep source snapshots compact, share repository objects/CAS content, and avoid
full repository clones, copies of external corpora or one fresh full build tree
per test. Never hardlink a mutable live source file into an immutable snapshot.
Keep current plus last compilable generations and explicitly pinned reproduction
artifacts by default; remove only proven-inactive generated materializations.
Retain small receipts/failure reproducer metadata with bounded/paged logs. A
configured disk budget reports pinned usage and coverage/retention debt rather
than deleting valuable uncommitted work, active artifacts or unique evidence.
Test repeated no-op starts and many short generations for bounded disk growth;
measure bytes rather than asserting that cleanup probably works.

fx owns immutable-store publication, action results and rooted collection. fo
owns semantic roots and private build-session lifetimes. Replace the global
Boolean generation pin with independently releasable owner/reason roots before
automatic collection. Maintain the store through infrequent thresholded bounded
sweeps; unchanged reads and idle Gremlin status do not rewrite access metadata
or scan the store. Legacy store/v1 and copied generations are lazily validated
and imported while live old owners remain protected.

## Test implementation language

All fo test programs, orchestration and assertions are Fortran. Small C shims
may expose OS process, signal, FIFO, symlink and monotonic-clock facilities that
Fortran does not portably provide. End-to-end tests remain separate processes
driving the public CLI/MCP boundary; process-boundary independence does not
require JavaScript. A test-only Fortran JSON parser validates protocol output
without sharing the production parser.

Issues #158--#163 migrate every current JavaScript fixture in behavior-preserving
slices, then remove Node from CI. Each old fixture is deleted only after its
Fortran replacement preserves or strengthens the crash, concurrency, JSON,
filesystem and execution oracle. Production code never depends on the test
harness.

## Ordered implementation and independent verifiers

| Stage | Issue | Future behavioral verifier |
| --- | --- | --- |
| Selection | [#138](https://github.com/lazy-fortran/fo/issues/138) | Mandatory targets execute; CLI/MCP replay the same seed/selection; sample and shuffle are separate. |
| Process ownership | [#139](https://github.com/lazy-fortran/fo/issues/139) | Owned child/grandchild heartbeats stop within grace; unrelated sentinel survives. |
| Durable completion | [#140](https://github.com/lazy-fortran/fo/issues/140) | Pass/fail receipts survive crash/cancel; interrupted test stays unknown. |
| Generation supervisor | [#141](https://github.com/lazy-fortran/fo/issues/141) | A keeps testing during failed B build; successful C preempts A; frozen A never reads changing C inputs. |
| CLI/MCP/background | [#142](https://github.com/lazy-fortran/fo/issues/142) | Idle client needs no polling; reconnect resumes events; new CLI works with stale MCP. |
| Compiler adapter | [ffc #798](https://github.com/lazy-fortran/ffc/issues/798) | ffc dispatcher/corpus runs use this engine, frozen dependency bundles and partial-result rules. |

Provider selection, process and journal slices can be implemented independently
with explicit ownership; supervisor consumes their APIs. Integrate the smallest
working Gremlin CLI first, with shared-core MCP access, then use **Gremlin itself**
to finish coverage, recovery and performance controls. Do not claim the
initial version satisfies an unimplemented mode or option. ffc semantic work
begins after the enabling contract needed for parallel Gremlin work is verified.

Existing [#119](https://github.com/lazy-fortran/fo/issues/119) complete JSON,
[#130](https://github.com/lazy-fortran/fo/issues/130) logs/visibility,
[#134](https://github.com/lazy-fortran/fo/issues/134) truthful timeout reporting,
[#135](https://github.com/lazy-fortran/fo/issues/135) dependency freshness and
[#131](https://github.com/lazy-fortran/fo/issues/131)/[#132](https://github.com/lazy-fortran/fo/issues/132)
dispatcher routing remain exact prerequisites where they block these verifiers.
Verify delivered behavior before closing; no pre-existing issue disappears by
being renamed Gremlin. Fix fo whenever its workflow misbehaves; do not add a
private project workaround or depend on reloading MCP to make progress.

Behavioral fixtures use fake compiler/worker executables with barriers, known
outputs and exit codes. No paid model calls are needed to prove scheduling.
Tests must cover simultaneous same-project CLI/MCP starts, two independent lanes,
TERM-ignoring descendants, source edits during capture, crash during receipt/cache
publication, stale ownership, large event streams, unchanged action reuse,
changed dependency invalidation, and bounded disk growth. Preserve these oracles
when adding features. The normal workflow is focused `fo test`/`fo exec`, then
bare `fo` before delivery; record pre-existing failures without masking them.

Performance acceptance measures no-op attach/reuse, incremental build latency,
first useful verdict, build-ready-to-replacement-test latency, cancellation grace,
CPU/RSS and bytes per retained generation. No-op attach must launch no duplicate
build/test supervisor; replacement dispatch begins as soon as ownership cleanup
and required build publication finish. Target cancellation within the two-second
TERM grace plus measured bounded reaping, and supervisory overhead small relative
to one targeted test. Record baseline/p95 and hardware; do not invent universal
speedups. Continue coding while bounded background measurements run.

## Bootstrap and dogfooding

The first milestone is a usable **Gremlin mode**, not the entire eventual worker
platform. Parallel mode assigns disjoint primitive providers plus thin adapters;
serial mode implements them in the main session in dependency order. Fix the
shared library/archive publication and freshness blockers first where needed.

1. Freeze source and test inputs, derive a complete generation/action identity,
   and run a tiny named fixture through the existing fo build/test path.
2. Add one native owner with durable per-case receipts, status/reproduce/stop,
   bounded owned cancellation and duplicate-start attachment. Candidate builds
   happen in isolated materialization while last-compilable tests continue.
3. Publish CLI `fo gremlin` and shared-core MCP parity; public start always
   returns promptly. A stale MCP cannot prevent updated CLI use or supervisor
   upgrade/reconnect. Advertise the exact supported initial options.
4. Prove bootstrap with small source-level oracles: pass/fail/blocking test,
   failed replacement build, newer successful version, source edits during
   capture, concurrent same-place starts, another unaffected lane and crash tail.
5. Use this initial Gremlin to develop the remaining Gremlin engine, coverage
   ledger, worker admission, cache/performance and repository-matrix features.
   Each fixed version supersedes its old campaign; no whole-suite drain per edit.
6. After parallel-mode/capacity/recovery acceptance, integrate the ffc corpus
   adapter and execute the compiler plan. Keep final comprehensive evidence
   separate from the fast development gate.

Suggested module boundaries for a luna coordinator: shared selection policy,
process lifecycle, generation capture, ownership/capacity state, durable journal,
small supervisor, and CLI/MCP adapters. Freeze API ownership before parallel
edits. The provisional shared boundary is
`gremlin_handle(action, project_dir, request_json, response_json, exitcode)` in
`fo_gremlin_supervisor`; adapters normalize argv/transport only. The core performs
one validated request parse and delegates mechanical tasks to providers. Use
real typed requests internally, bounded JSON/events externally and existing
argv helpers; avoid shell string interpolation and duplicate scheduling rules.
Worker prompts state exact issue/base/files/API/verifier and output-size limits.

## Project-specific test adapters

Generic Gremlin can use a project's existing ordinary fo/fpm test inventory,
a shared named-case dispatcher, or a narrow external-corpus adapter. Define a
small declarative adapter schema in the provider implementation: list case IDs,
run one named case or bounded batch, report independent oracle outcome, input
closure, duration class, artifact ownership and cache/reproduction data. Invoke
argv directly. The exact configuration path/schema is finalized in #142/#798
rather than inventing an ffc-only scheduler or mandatory plugin framework.

ffc has hundreds of compiler cases and several slow whole-corpus wrappers.
Its existing dispatcher already shares statically linked code while preserving
public case names and subprocess exit status. Keep this optimization. Gremlin
selection must operate on cheap named/leaf cases, not repeatedly choose a giant
wrapper and spend each generation's budget discarding unpublished observations.
A sampled discovery adapter and complete full-verification mode share the same
oracle/manifest semantics; leaf scheduling must not claim that a complete wrapper
passed. Current gauntlet one-case batches are a bootstrap retention mechanism.

Track per-project rebuild/link granularity, test binary count/bytes, monolithic
wrapper cost, true nested compiler fanout and runtime/library hashes. Prefer
shared dispatcher/prebuilt artifacts and valid reference-cache reuse. Preserve
applicable default reduced debug payload, linker DCE, stale-profile hygiene and
optional shared development linking when independently measured. Do not force
ordinary external fpm projects to rewrite their tests to use Gremlin; support
existing targets, and document efficient optional granularity improvements.

## Project matrix

[fo #145](https://github.com/lazy-fortran/fo/issues/145) owns generic compatibility
and performance evidence. Inventory the user's maintained GitHub repositories
read-only; identify real fo users from manifests/workflows and fpm projects as
additional candidates. Every maintained fo-using repository eventually receives
a measured row/disposition. Keep private repo names and evidence out of public
plans/issues. Selected external fpm projects are pinned capability examples,
not uncontrolled unbounded cloning. Execution is authorized; each row still
uses a frozen revision, bounded command and independent project oracle.

Start with fo, a small library, FortFront and focused ffc cases, then cover all
maintained fo projects and representative external fpm projects. Exercise native,
fpm and CMake backends where fo promises them, modules/submodules, path/Git
inputs, ordinary standalone and shared-dispatch tests, mixed-language/linking,
intentional failures and external corpora. Use existing project-local contracts
and normal supported build/test workflows as independent oracles. Do not weaken
legitimate failures to make cross-project sampling appear green.

Record repository/toolchain/dependency revisions, normal/Gremlin commands, test
identities/dispositions, no-op and incremental latency, first useful verdict,
cache hits, relink count, CPU/RSS and retained bytes. Repeated same-place starts,
parallel independent worktrees, cancellation and action-cache reuse must preserve
normal behavior. Missing/unsupported/infrastructure-failed rows remain visible
and owned. Early representative rows gate rollout; bounded rotating subsets run
while development continues; complete fixed-version matrix coverage is a final
provider-completion gate. Store compact receipts and replayable public summary
when observations exist, never invented benchmark claims.

## Final continuous-verification contract

Gremlin is a deterministic continuous-verification state machine. It reports
facts and never encodes repository governance, merges or pushes.

For one immutable execution generation, keep these milestones distinct:

1. **gate**: capture and build pass; every known current reproducer, affected
   test and configured mandatory test passes; no gate case has FAIL, TIMEOUT or
   INFRA_ERROR; no relevant input changed; the active generation is unchanged;
2. **ordinary**: every continuously eligible ordinary case has a current-
   generation PASS;
3. **full**: the supported slow/full inventory passes with each case's normal
   runner budget;
4. **quiescent**: full is green, inputs are clean, and no candidate or pending
   reproducer exists, so no tests, builds, captures, hashes or compiler probes
   run until a relevant filesystem event wakes the owner.

`local_gate_green=true` at the gate. What happens next belongs to the external
repository policy. Lazy-fortran controllers push the exact integrated generation
to `main` immediately when this gate is green and no known current regression
exists; they do not wait for ordinary/full coverage or GitHub CI. A later
reproducible failure makes the gate red and gets repair/revert priority. Focused
receipts support iteration but never manufacture a complete green.

Expose orthogonal `phase`, `health`, `dirty`, active-generation identity,
`local_gate_green`, `verification_level`, `fully_verified`, per-level coverage
counts, and epoch identity/cursor. Emit durable typed transitions for generation,
build, local-gate, regression, coverage completion, quiescence, wake and
supersession. CLI and MCP share typed waits for local-gate-green, ordinary-
verified, fully-verified, quiescent and failure. Lifecycle events use a versioned
event envelope; never encode them as fake pass/fail case receipts.

### Finite randomized coverage

For generation `G`, canonicalize its eligible inventory, persist its digest and
derive a replayable randomized permutation without replacement. Persist
generation ID, inventory digest, epoch number, seed, cursor and each current-
generation case outcome. Campaign durations are execution chunks; they do not
reset the epoch. Crash/restart resumes its unseen set.

Regression, affected, mandatory and bounded reproducer cases may jump ahead. If
one is still unseen, its run satisfies that coverage obligation and the cursor
later skips it. No exploratory case repeats before all eligible cases are
attempted. Historical outcomes prioritize later generations but do not satisfy
their coverage; CANCELLED remains unknown. Cross-generation PASS transfer is
allowed only through a future rigorously complete action/oracle cache key.

### Change events, identity and provenance

One shared `fo_change_watch` provider watches project execution inputs and path
dependencies, debounces bursts and marks the session dirty. Gremlin performs one
initial capture, then no discovery/capture until a relevant event. `fo watch`
becomes a thin compatibility policy on the same provider or is deprecated; it
does not retain an independent event-to-check engine. `.git` and unrelated paths
are excluded execution inputs.

An editor or coding agent may write several temporarily inconsistent files in
one burst. After the short debounce, Gremlin captures only the newest coherent
candidate it can observe. A candidate that fails to build publishes its compiler
diagnostic but never runs tests or replaces the active generation. The frozen
last-compilable generation continues useful testing. A later candidate takes
over atomically only after its build succeeds; completed old receipts survive,
and interrupted old cases remain unknown. The continuous-preemption oracle keeps
this last-good behavior independent from the watcher/debounce implementation.

Execution identity includes project, dependency, test/oracle and runtime input
contents, toolchain/flags/relevant environment, and the pinned fo driver digest.
Git base commit and working-tree patch are provenance fields, not execution-key
inputs. A metadata-only commit therefore preserves the generation and evidence;
a relevant content edit creates a new generation with fresh provenance.

The target module boundaries are `fo_change_watch`, `fo_gremlin_types`,
`fo_gremlin_request`/`fo_gremlin_codec`, `fo_gremlin_context`,
`fo_gremlin_generation`, `fo_gremlin_state`, `fo_gremlin_journal`,
`fo_gremlin_coverage`, `fo_gremlin_policy`, `fo_gremlin_campaign`,
`fo_gremlin_commands`/`fo_gremlin_session`, and a small state-machine-only
`fo_gremlin_supervisor`. CLI and MCP remain adapters.

## Delivery and architecture consolidation

Progress is published continuously even when it is not ready for `main`:

- [draft core PR #146](https://github.com/lazy-fortran/fo/pull/146) targets
  `main` from `gremlin/core-review`; draft pushes publish progress without CI.
  Historical work-mode and combined branches remain immutable evidence only;
  they are not integration candidates.

Push each integrated milestone and exact verifier state. Focused receipts permit
the next repair but do not redefine the merge gate. `main` advances only after
the current-head full pipeline, required behavioral fixtures, CI, review and
matrix dispositions pass. Never accumulate unpublished controller commits merely
because a continuous campaign is partial.

**Never wait for GitHub CI while implementation work is available. GitHub CI is
post-submit evidence, not part of the development loop. Use Gremlin and focused
local oracles while developing.** Workers reuse one exact built driver and run
their owned independent oracle while iterating. For lazy-fortran, push an exact
integrated generation to `main` immediately when `local_gate_green` is true and
no known current regression exists. Do not wait for ordinary/full coverage,
remote CI or unrelated review. CI audits current `main` asynchronously and may
cancel superseded runs. A confirmed real regression gets immediate repair or
revert priority; unrelated work may continue, but unrelated main pushes normally
pause. Gremlin never models, polls or schedules CI, refs or external receipts.

Dogfooding begins during implementation. Keep one named resident `fo gremlin`
integration lane per active repository and rotate task-worktree lanes for
focused changes. Six simultaneous cold lanes wrote 1.8 GB of state and all
blocked in kernel writeback, so unrestricted per-worktree residency is rejected
behavior pending #148/#155 and cross-lane I/O admission. Preserve receipts,
first-verdict latency, failures, restart behavior, idle activity and resource
use as live evidence. Independent oracles remain authoritative; self-testing
evidence alone never promotes the tested Gremlin implementation.

The implemented core is mature enough to pause unrelated feature growth for an
architecture and finite-verification pass. Ordered blockers before core merge
are:

1. [#148](https://github.com/lazy-fortran/fo/issues/148): shared filesystem
   events and dirty/debounce/fingerprint gating so idle Gremlin performs no full
   captures; consolidate or deprecate the independent `fo watch` check loop.
2. [#149](https://github.com/lazy-fortran/fo/issues/149): extract request,
   context, campaign/history and session services from the supervisor.
3. [#150](https://github.com/lazy-fortran/fo/issues/150): converge domain JSON
   on the existing typed parser and shared CLI/MCP validation.
4. [#151](https://github.com/lazy-fortran/fo/issues/151): pin/hash the exact fo
   driver used by a live session and expose it in reproducible receipts.
5. [#157](https://github.com/lazy-fortran/fo/issues/157): bind the complete
   compiler/helper/runtime/external-dependency closure to executed bytes.
6. [#153](https://github.com/lazy-fortran/fo/issues/153): finite deterministic
   randomized coverage epochs with crash-safe current-generation accounting.
7. [#154](https://github.com/lazy-fortran/fo/issues/154): local-gate facts,
   ordinary/full verification, semantic events and typed waits.
8. [#155](https://github.com/lazy-fortran/fo/issues/155): enter quiescence after
   full green and wake only on relevant shared change events.
9. Register every late behavioral oracle in the post-submit workflow and keep
   the complete matrix as provider-completion/milestone evidence.
10. [#158](https://github.com/lazy-fortran/fo/issues/158)--[#163](https://github.com/lazy-fortran/fo/issues/163): replace all Node fixtures with standalone Fortran process drivers and remove Node from the test contract.
11. [#164](https://github.com/lazy-fortran/fo/issues/164): make stat-memo publication cross-process safe.
12. [fx #42](https://github.com/lazy-fortran/fx/issues/42), [fx #43](https://github.com/lazy-fortran/fx/issues/43), [fo #165](https://github.com/lazy-fortran/fo/issues/165)--[#168](https://github.com/lazy-fortran/fo/issues/168), and [fx #44](https://github.com/lazy-fortran/fx/issues/44): converge generation capture, action outputs, private build sessions, ordinary fo and Gremlin on one immutable store and engine with rooted low-churn collection.

Experimental agent scheduler PR #147 is closed without merge; #143 and #152 are
closed as not planned. External controllers own worker DAGs, worktrees, model
adapters and integration. Generic aggregate build/test/I/O admission remains a
fo core requirement. Legacy MCP async queue consolidation and the C process-file
split follow the core merge unless a behavioral verifier proves that they block
current correctness.

## Active delivery state

**Execution is user-authorized in parallel mode.** The controller started from
fo main at `e4fc193`. The reviewed core and dependency-shadow repair are now on
fo `main` at `1c68c04`; PR #146 is merged. The exact integrated head passed its
focused local gate and post-submit GitHub Actions run 37151346404. Development
continues as small locally gated main increments without waiting for CI.

The `a0d3515` candidate passed the isolated full fo pipeline: static 109/109,
build 62/62, test build/run 48/48, lint and format check, in 26.6 s. The same
binary passed campaign history/seed replay, both owner-recovery crash barriers,
timeout ordering, continuous generation preemption, sequential and overlapping
reproduction logs, atomic link/run publication, Gremlin bootstrap, MCP named
eligibility, stale-MCP/new-CLI behavior, lossless 300-receipt pagination and the
then-current work-mode fixture. Four existing array-temporary warnings remain.

The abandoned work-mode stack ended at `86870cf`; its generic CMake named-test
routing fix and identical oracle already exist in core. The installed fo now
comes from the locally gated `1787f64` source, SHA256
`f6d0d66ed041b5046d7b3ad790ffb450b29eb62ce194d4c4d946b5eeb9a16f64`; the earlier full
pipeline passed 109/109 static, 62/62 build, 48/48 test, and lint in 19.0 seconds,
followed by its historical work-mode fixture and MCP system test (79/79). It is
used only as the current bootstrap CLI and will be replaced by the accepted core.

Implemented issue state:

- #138 selection/history and seed replay, #139 owned-tree cancellation and
  timeout ordering, #140 durable receipts/recovery, #142 CLI/MCP lifecycle and
  pagination, and #144 atomic artifact publication have passing focused behavior
  on main. Keep their remaining scopes open through architecture consolidation.
- #141 generation replacement is complete on main. Its behavioral oracle proves
  that a failed intermediate build retains the active generation and running
  test, never tests the broken candidate, preserves completed receipts, and
  switches only after the repaired candidate builds.
- #143/#152 and PR #147 are closed without merge after the KISS review assigned
  agent scheduling to the external controller; no work-mode code enters core.
- #145 has one bounded fpm row passing on the exact candidate, one independent
  CMake/CTest row passing while exposing a fo CTest-name discovery defect, and
  one row correctly blocked by a missing private path dependency. This is early
  matrix evidence, not complete coverage.
- #149's first three reviewed slices are integrated: request DTO/parsing/
  validation lives in `fo_gremlin_request`; context, dependency, environment,
  toolchain, capture and CAS-root discovery lives in `fo_gremlin_context`; live
  and terminal session persistence/recovery lives in `fo_gremlin_session` at
  `b987338`. Campaign and command extraction remains and waits for #153/#154
  semantics respectively.
- `58542eb` selects the host ABI for the async-filter oracle while preserving
  explicit synthetic targets. Escalated repair `1cdbaa0` moves standalone C
  fixtures outside fpm auto-discovery and explicitly registers the mixed-unit
  generation test. Clean `fpm test`, the isolated full pipeline, async lifecycle
  and all seven C variants passed; exact-head CI is live.
- #150 is integrated at `b73f9ac` after Luna repairs and task-specific Sol
  escalation. The shared typed decoder now enforces JSON grammar/depth, retains
  int64/raw values, decodes Unicode without key/path aliasing, rejects controls,
  and gives CLI/MCP the same domain validation. Focused supervisor, MCP parity,
  request-structure and full-pipeline gates passed. GitHub Actions also passed
  the exact published `b73f9ac` head (`build + fpm test`, run 37148134220).
- #151 repair attempt 2 remains rejected: argv[0] can identify replacement
  bytes rather than the running image, and the recorded compiler digest is not
  bound to the compiler actually executed. Its frozen reproducer is in a
  task-specific Sol escalation after two substantive Luna attempts.
- The dogfood-discovered mixed native/fpm dependency collision is repaired at
  `1787f64`: distinct path/Git providers prove root-provider precedence and the
  duplicate-object link failure no longer reproduces. Exact integrated focused
  build, `test_dep_resolve` and the behavioral CLI fixture passed.
- Resident dogfood lanes now run on the core, fx and active task worktrees.
  They already exposed the hard five-second case budget, a missing diagnostic
  on initial capture failure, and cold-start resource contention; these are live
  evidence, not substitutes for independent task oracles.

After each meaningful delivery, update this plan, workspace master and affected
issues; commit and push the controller branch immediately. Preserve exact bases,
patch digests and independent behavioral evidence. Root `PLAN.md`/`AGENTS.md`
are outside Git; repository plans are committed.

## Research basis

[Saff and Ernst, ISSRE 2003](https://homes.cs.washington.edu/~mernst/pubs/wasted-time-issre2003.pdf)
sections 5.3–5.4 describe background testing, random priority and restarting on
the next compilable version. Their later
[Eclipse continuous-testing plug-in](https://homes.cs.washington.edu/~mernst/pubs/conttest-plugin-etx2004-abstract.html)
used otherwise idle developer-machine cycles for background regression feedback.
[Infinitest](https://infinitest.github.io/) automatically runs relevant tests
after code changes, while [JUnit Max](https://newsletter.kentbeck.com/p/the-economic-case-for-junit-max)
emphasized automatic post-compile runs and latency to first failure. Gremlin
extends this lineage with immutable compiled generations, last-compilable
retention, owned preemption, durable receipts, crash recovery, finite randomized
coverage and headless CLI/MCP operation. [pytest-randomly](https://github.com/pytest-dev/pytest-randomly)
records reproducible shuffle seeds. [Git worktrees](https://git-scm.com/docs/git-worktree)
provide separate checkout/HEAD/index state while sharing repository objects.
The lane protocol, resource leases, disk budget and CLI/MCP contract are fo design
choices; published continuous-testing results do not prove ffc speed or coverage.
