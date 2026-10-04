# fo / Gremlin implementation and verification plan

Updated 2026-10-04. Planning/source audit: `d166c393fb1fbdd166a12f814030a5277b7a97fd`.
This is the authoritative current plan. Issue bodies define focused acceptance;
this document orders the work and resolves obsolete bootstrap instructions.
New interfaces/workflows below are planned, not claimed implemented by this edit.

## Immediate controller instructions

1. Continue small, locally verified main increments. Use the exact candidate's
   focused correctness tests and known reproducers; reuse warm caches. Do not
   wait for GitHub CI, a full Gremlin epoch, a benchmark, or a project matrix.
2. **Benchmarks are never the development gate.** Real timing/resource runs and
   third-party project acquisition belong to independent scheduled/manual CI or
   an explicit slow/performance audit. They do not block main, merge, ordinary
   development, or unrelated issue closure. Timing thresholds are advisory.
3. Keep correctness distinct: a confirmed stale build, wrong result, lost
   receipt, missed required test or ownership violation is a real defect even
   when discovered by a non-blocking audit. Reproduce against the exact current
   candidate; prioritize repair/revert without stopping independent coding.
4. #175 and #189 are the immediate input/impact spine. Finish shared declared
   inputs, then derive affected tests from baseline/candidate content, not cache
   misses. #148 and #170 consume that same inventory for watching and scratch.
5. Land independent storage/session and correctness slices as ready. Do not wait
   for the complete #165-#168 migration to measure the existing baseline, and do
   not wait for its measurements to land a locally correct migration slice.
6. External controllers own agents, task DAGs, worktrees and integration.
   `fo work` is abandoned (#143/#152, PR #147); do not recreate it or add a CI
   scheduler, promotion engine, cache daemon, database or bulk RAM cache to fo.

## Product scope

fo is the Fortran build/test/development driver: build, test, run, incremental
content-addressed caching, affected/continuous testing, formatting, lint/static
diagnostics, LSP/MCP convenience and the dependency/bootstrap work required by
those commands. Its intended boundary is comparable to Cargo / the Go toolchain,
not a general scientific-computing platform.

Explicitly out of scope:
- Nix/Spack-style hermetic environment management or system package management;
- capturing/pinning complete compiler helper/sysroot/loader/system-library closures;
- containers/sandboxing as a product feature;
- scientific workflow/provenance/capsule/checkpoint systems;
- symbolic derivation/proof/synthesis pipelines;
- cluster/job, CI/promotion or coding-agent orchestration.

Historical capsule/Nix/FortOS issues #1–#7 are already closed. #120
(proof/synthesis) and #157 (complete host toolchain/runtime capture) are closed
not planned under this boundary. Users needing hermetic reproducibility provide
it externally with CI images, containers, Nix, Spack or equivalent.

Inside fo, reproducibility means correct and inspectable build/test identities:
project inputs, declared dependencies/fixtures, compiler identity/version,
effective flags/link policy, deterministic selection/replay and exact result
receipts. These facts are sufficient for cache invalidation and continuous
testing; they do not claim a portable captured operating environment.

## Repository policy versus verification facts

Gremlin produces exact local evidence, not permission to merge. Lazy-fortran's
current policy is to push small integrated increments to main as soon as the
configured local correctness gate is green and no known current regression
remains. GitHub CI is asynchronous post-submit evidence. Human/CI/golden-record
requirements in repositories such as SIMPLE or NEO-2 are external policies and
are neither weakened nor assumed by Gremlin.

`local_gate_green` is only as sound as the represented requirements and input
model. Until #189 fixes cache-dependent impact discovery, do not treat an
arbitrary tiny sampled set as proof that all affected requirements passed. Use
explicit focused reproducers/known affected targets or conservative widening,
and label incomplete selection honestly. No old PASS or cache hit manufactures
current-generation verification.

Controller integrates one candidate at a time; workers never self-promote main
or replace a global tool. A confirmed current correctness failure normally
pauses unrelated main promotions while independent work continues. Slow timing,
upstream baseline failures, missing optional audit prerequisites and unfinished
audits are not such a pause condition. Full correctness suites remain useful
background/milestone evidence; performance audits are not mandatory milestones.

## Current state, not future claims

Core PR #146 is merged. Main has last-compilable retention/preemption (#141),
finite randomized epochs (#153), readiness/events/waits (#154), a shared event
provider, atomic artifact work and substantial native behavioral fixtures.
These are implemented foundations, not grounds to claim every remaining input,
closure, storage or performance problem solved.

Known remaining limitations at the audit snapshot:

- #148: shared event delivery exists, but declared-input relevance and the old
  `fo watch` policy still need consolidation. A PLAN.md-only edit previously
  triggered capture; declared dependencies under build/ must not be ignored.
- #189: `fo_changed_modules` uses compile-cache availability; Gremlin queries it
  after building. Cache state is not an execution-generation difference.
- #151: provenance repairs exist and exact fo-driver pinning still needs its
  remaining platform acceptance. Complete host compiler/helper/runtime capture
  is intentionally out of scope (#157 closed not planned); ordinary compiler
  identity/configuration remains part of build/cache invalidation.
- #170: tests need private writable execution views, not a read-only source
  snapshot as their working directory.
- #165-#168: shared fx store primitives exist; compact fo manifests, private
  sessions, one ordinary/Gremlin engine and owner-aware retention are migration
  work, not already completed by the shared-cache foundation.
- #149/#150/#185/#186: supervisor/campaign/command, residual JSON, MCP async and
  source-policy responsibilities still overlap.
- #183/#161: native recovery parity includes remaining receipt-versus-coverage
  crash windows. A case marker before exit is not a committed PASS.
- Gremlin bootstrap children are restricted to jobs=1 and recursive public fo
  calls. Cache reuse does not establish final parallel or edit-to-green speed.
- `bench/src/bench_engine.f90` currently measures incremental_leaf/core using
  metadata-only touch and enforces timing targets. #190 owns correction and
  removal of benchmark gating; #176's native harness migration stays complete.

The previous detailed delivery log, exact historical commits/digests, platform
limitations and research links remain available without duplicate narratives:
[immutable prior plan and evidence](https://github.com/lazy-fortran/fo/blob/d166c393fb1fbdd166a12f814030a5277b7a97fd/PLAN.md#active-delivery-state).
Historical timings are not current measurements. This planning revision does
not assert new test passes, speedups or resolved CI failures.

## Current incremental delivery (2026-10-04)

Driver pinning and canonical inventory providers are integrated through
`c591e79`, preserving planning commits `fe78809`/`73f59b1`. Linux #151's native
A/B replacement oracle passes in 3.55s and generation oracle in 2.11s; #175's
declared-input mutation oracle and state gate pass. The combined supervisor,
MCP request-structure and public MCP gate pass 3/3 in 8.73s. Validation uses exact
executable SHA256 `7dd3dfeedb313c04f6c8007ac4d1f6435274f13214186a532918dbe7d98c0c86`.
Source/driver pinning intentionally stops at fo's own driver; complete host
toolchain closure is out of scope. #175's watcher/capture/execution consumers
and #189 campaign adoption remain separate work.

Mac #151 is pending fx #56's public feature-declaration repair. Linux evidence
is not a Mac receipt. Darwin startup uses public UUID/held-descriptor checks;
same-UUID substitution before descriptor capture remains an explicit limit.
No private OS API or generalized security framework is added.

fx #54's one strict typed parser is locally green and published; #150 consumes
it next. fx #51/#55 remove production synchronization hooks and compliance
meta-testing, preserving meaningful public behavior. #191 bounds source scanner
cycles/truncated paths before #186's wider policy extraction. #190 remains an
independent optional audit and starts with workflow separation, not a benchmark
run imposed on these increments.

## One architecture

```text
canonical declared inputs and dependency/impact model
                    |
           immutable generation
                    |
       private transactional build session
                    |
        shared compile / link / test services
                    |
           verified fx action/blob store
                    |
           +--------+---------+
           |                  |
    ordinary fo once    Gremlin watch/supersede/coverage
```

Share semantics and providers, not unnecessary I/O. Ordinary warm commands must
not copy the whole source tree just to use the same API. Gremlin alone needs
resident watching, generation replacement and coverage lifetime management.
CLI/MCP are adapters. A one-shot check must not become an endless campaign.
Preserve supported native/fpm/CMake behavior and report unsupported Gremlin
backends explicitly. No special ffc scheduler or mandatory adapter framework.

Keep identities distinct:

| Identity/relation | What it establishes |
| --- | --- |
| content/tree | exact bytes plus canonical logical paths, kinds and modes |
| generation | represented source, dependency, test/oracle, configuration and execution inputs |
| compile action | source/includes, effective flags, compiler and required module interfaces |
| link result | objects, archives/libraries, linker policy and runtime companions |
| test action/receipt | exact executable, public case/argv, harness, data/oracle, environment and outcome |
| impact decision | baseline/candidate/input/model identities, affected cases and widening reasons |

A private-body change may preserve a .mod interface and avoid caller
recompilation while changing linked behavior and test results. Do not conflate
compile dependencies with runtime/test dependencies. Build-cache reuse is not
PASS reuse. Reusable test evidence requires a complete compatible test-action
contract; until then execute selected tests and retain current-generation
receipts. Audit the current broad test-object key during #167 rather than
weakening result invalidation to improve hit counts.

## Canonical inputs and affected-first testing

#175 owns one typed declared inventory: logical root/path, role, kind/mode,
content digest, owning target/case/shared scope and writable-fixture intent.
Watch (#148), capture (#165), execution view (#170/#166), build (#167), source
policy (#186) and test impact (#189) consume it. Discovery has no copy/build/test
side effect. Relative declarations reject escapes and ambiguous aliases; known
unsupported ambient reads report incompleteness. This is not an OS sandbox.

Represent Fortran modules/submodules, includes/preprocessing, C/BIND(C), generated
code/generators, path/Git/dev dependencies, linked/runtime inputs, fixtures,
arguments, relevant environment and subprocess-executed tools. A test that invokes
fo/ffc has an execution dependency even when no USE edge connects its source.
Preserve outside roots and declared dependencies nested below generated-looking
paths. Root role matters; never blindly exclude a declared build/nested_dep.

#189 compares a frozen baseline/candidate by stable logical inputs, independently
of CAS warmth, prior builds or other processes. Use old/new dependency edges for
deletions and renames. New tests, missing baseline, unsupported edges, overflow,
ambiguous inputs and global configuration/toolchain changes widen conservatively
unless a sound narrower dependency relation exists. Include affected slow
correctness tests with their normal budgets. Do not convert incomplete discovery
into an empty successful gate or make the first random case its substitute.

Freeze requirements per generation and run:

1. unresolved compatible regression reproducers;
2. the complete deduplicated affected/configured-mandatory set;
3. other useful historical priorities and the finite unseen background epoch.

A chunk limit controls dispatch, not required-set membership. More affected cases
than one chunk remain required. CLI/MCP and ordinary --only-changed use the same
service and report selection reasons/completeness. Changes to requirements or
relevant inputs revoke the generation-bound gate token.

Use existing remainder execution for shadow evaluation; do not add a second full
suite. A failure outside the selected set is first a triage observation. Confirm
the same-generation outcome and compatible baseline/control before attributing
a regression; distinguish selector omission under the supported model from
preexisting, flaky, infrastructure, unsupported-input and unexplained failures.
Superseded/unrun remainder is censored, not evidence of no misses. Zero observed
misses is not a proof of universal selector soundness.

Per-test runtime coverage is a later optional ranking experiment, not a new
completion dependency. Missing/stale traces, initialization, new branches,
subprocesses, native libraries, runtime data and environment need conservative
handling. Start by ordering the static set, not excluding its members. Isolate
parallel coverage outputs and bind maps to exact case/generation/profile/tool
identity. No default instrumentation or ML system.

## Generation lifecycle, coverage and evidence

Arm watches before the capture used to claim clean. Coalesce a burst, capture a
coherent immutable candidate, and build in an owned session. On build failure,
report diagnostics, never test that candidate, and keep testing the last
compilable generation. Only successful build publication activates the new
generation; it is not yet verified. Preempt only its obsolete owned work,
preserve completed receipts and leave cancelled/in-flight cases unknown.

For each exact generation, canonicalize eligible public case IDs and persist
inventory digest, randomized-without-replacement permutation/seed, cursor and
outcomes. Campaign boundaries do not reset coverage. Priority executions can
discharge unseen obligations once. Recover launch/receipt/coverage transactions
without losing or duplicating credit. Old-generation PASS and reproduction logs
cannot silently inflate current coverage.

Keep independent milestones: local gate, ordinary coverage, full supported
local coverage, and quiescence. Publish phase, health, dirty state, generation,
model/closure completeness, required/remaining counts and a token bound to owner,
baseline/candidate, input/model/requirement digests and watcher epoch. Existing
typed waits/events remain the interface; no CI/ref/promotion state enters it.

The default full/local inventory does **not** include optional performance audits,
remote project matrices or corpus experiments unless explicitly configured as
that separate run's scope. Ordinary slow correctness cases are not benchmarks.
Neither full coverage nor conservative widening should accidentally trigger
network benchmarks. Always report the scope/denominator; never imply all possible
tests or all environments were proved.

When the configured scope has completed, stop scheduling redundant work. Full
green with clean inputs and no candidate/reproducer permits quiescence; status
must retain any closure incompleteness. A relevant edit/overflow/root loss clears
freshness and wakes bounded reconciliation. No idle source recapture, toolchain
probe, payload hashing or build/test loop. Preserve finite red/unknown outcomes
without a busy retry loop. Git commit metadata is provenance, not a new execution
generation when bytes/configuration are unchanged.

### Resident development and temporary task lifetimes (#142/#155/#139)

This is the refined target contract; task-bound cleanup is remaining acceptance,
not a claim that every lifetime path is already implemented. Keep lifetime in
one shared session API, independent of coverage progress and CLI/MCP transport.

| Purpose | Remains alive through | Ends on |
| --- | --- | --- |
| Persistent development lane (normal integration use) | launching CLI exit, observer/harness disconnect, worker completion, finished tests and quiet periods | explicit stop or deliberate controller shutdown/replacement |
| Explicit temporary task lane | task activity and completed coverage while its task supervisor still owns it | explicit task-end or loss of its owning supervisor connection, using bounded scoped cleanup |

Full verification leaves the same development owner **resident and quiescent**:
it watches for relevant edits and serves controls, with no redundant payload
work. An edit wakes that owner without another start. Never infer abandonment
from completed tests, no edits, no polling or an idle/heartbeat timeout.

Keep one named persistent integration lane per watched integration checkout;
compatible repeated starts attach atomically rather than spawning duplicates.
Task-specific worktree lanes are explicitly temporary. Their ownership belongs
to the enduring task supervisor, never the short-lived CLI command that starts
or observes them. A held connection/pipe with EOF detection is sufficient where
supported; no heartbeat framework or agent scheduler is needed.

An attaching observer does not acquire automatic cleanup ownership. Reject an
incompatible lifetime request clearly; a temporary task cannot silently adopt
or downgrade the shared persistent lane. Expose created/attached, lifetime
purpose and exact session/owner identity. MCP EOF preserves intentionally
persistent sessions and cleans up only sessions bound to that owning connection.

Task cleanup reuses #139's exact session/owner-start identity checks and bounded
cancel/reap path. Retain completed receipts; interrupted cases stay unknown.
Stale cleanup cannot stop a replacement owner, another lane or an unrelated
process. #142 owns the common lifetime/adapter acceptance; #155 owns resident
sleep/wake. Native public oracles must prove same-owner wake after completion,
start/attach deduplication, persistent survival after launcher/observer exit,
temporary task-end/disconnect cleanup, and unrelated/replacement owner survival.

Durable JSONL receipts/events record actual outcomes before later work. Preserve
PASS/FAIL/FLAKY/TIMEOUT/INFRA_ERROR, unknown/cancelled distinctions, original names,
seed/order, exact driver/artifact inputs and referenced logs. Page losslessly;
no former 16 KB/256-case truncation. A tiny durable record and recreatable build
payload have different durability requirements.

## Shared storage, sessions and concurrency

Use fx verified blobs/trees/action results (#42/#43), private mutable sessions
(#166), and the root/read/publication-lease protocol (#44 Phase A). Publish
complete results atomically; equal action results reuse without rewriting;
conflicting results quarantine the action and retain evidence. Atomic rename
alone does not prove complete action keys or safe multi-file collection.

#165 stores one unique input blob and compact manifests, not full source and
verification payload trees per generation. Descriptor-rooted capture and mutation
checks stay authoritative. Optional reflink/clone materialization has byte-copy
fallback; never hardlink writable source/compiler output into the shared store.
Stable logical paths or explicit path-sensitive keys preserve compiler semantics.

#170 supplies unique writable fixture/scratch views from declared inputs;
source/build evidence stays immutable. #166 centralizes source, dependency,
working, output and process roots and commits complete compilable sessions.
Current/last-compilable/reproduction processes lease exact result/runtime graphs.
Public paths are compatibility views, not execution authority.

#168 Phase A adopts independently releasable owner/reason roots. Collection
remains disabled until every legacy/new live owner is covered. Only then enable
fx #44/#168 Phase B bounded reachability collection, hysteresis/low-frequency
retention touches and owner-aware cleanup. Budget pressure reports pinned debt;
it never deletes active artifacts or unique evidence. Separate CAS, private
sessions, logs and pins; logical/allocated bytes are not NAND wear.

Use existing bounded host admission for compile/test/capture work; one lane must
not multiply FO_JOBS into nested pools. Extend jobs=1 only with an ownership and
capacity oracle, exposing actual capacity. The temporary one-heavy-lane limit
can remain while coding is parallel; optional #190 measurements guide tuning,
not unrelated implementation admission. No database, remote cache protocol,
per-object global payload lock or application blob-RAM cache.

## Optional performance audit (#190)

This section is a measurement plan, **not a development or integration gate**.
#190 owns real workload semantics, phase/counter collection, advisory reporting
and independent CI/slow placement. #145 owns automatic project acquisition and
reference compatibility. Both can run on today's code before migration.

Separate four things: correct program output, valid measurement procedure,
hardware timing observations, and advisory performance targets. A slow valid run
is not a product-test failure. An invalid/failed/cancelled measurement has no
successful timing result. Keep raw diagnostics and classify it; never hide it
with a fake PASS. No required check or development job depends on benchmark
completion. Even audit implementation uses tiny focused harness oracles rather
than running the full external matrix per edit.

Replace mislabeled touch-only incremental rows with explicit metadata_touch.
Real-edit workloads include warm no-op; cold build; warm CAS with fresh private
outputs; private-body leaf edit; public-interface edit; central fan-out; one
test-body edit; include/path/C/header/runtime-fixture changes; broken build then
repair; and intentional rollback to already cached content. Edits occur only in
owned copies with before/after hashes and independent expected behavior.

Repeated A/B toggling may already have compiled B: distinguish that cache-hit
experiment from recompilation. Use fresh semantic revisions or a warm-baseline
cache not containing the edited result for each real-recompile sample. Do not
purge user caches. Separate acquisition/network/bootstrap from build latency.

Record same-host monotonic timestamps from last relevant edit through debounce,
capture, build ready, first test start, first durable verdict, local gate and
optional full/quiescent completion. Record compiled units, action hits/misses,
restored objects, link count, selected/executed unique tests, copied/cloned/stored
bytes, retained bytes, CPU/RSS and effective capacity. Missing platform counters
are unknown. Compare paired baseline/candidate runs with exact tool/project/
dependency/profile/hardware/filesystem identity; retain raw samples and dispersion.
Do not present p95 without adequate sample count, fixture constants as timings,
or historical link measurements as current edit-to-verdict latency.

Include tiny and realistically scaled synthetic workloads plus #145's pinned
real projects. Measure one/two warm lanes and bounded larger configurations,
quiescent I/O, and true compile-versus-link-versus-test cost. Optional coverage
ranking experiments use an isolated instrumented profile. Instrument shared
service seams lightly; no always-on profiler or durable per-stat tracing.

Scheduled/manual audit workflows run independently, with explicit time/disk/CPU
bounds and artifact retention. A local audit is explicitly requested and does
not fetch without permission/options. Superseded measurements are partial;
nightly pinned runs should have an independent concurrency group rather than be
restarted by every main push. Development never waits for those workflows.

## Automatic third-party matrix (#145)

Use a small versioned public allowlist/manifest, for example bench/projects.toml,
not a plugin framework. Each row records repository URL/full commit SHA,
dependency/submodule pins, logical layout, backend, reference/fo argv, eligible
case IDs, preprocessing requirements, expected baseline dispositions, reversible
edit recipes and resource/network bounds. Resolve branch/tag pins only during
explicit refresh, not silently during a measurement.

Initial rows:

| Project | Purpose and cautions |
| --- | --- |
| lazy-fortran/fo | self-dogfood plus independent process/output oracles |
| lazy-fortran/ffc | large external consumer; pin matching FortFront/liric/runtime; named dispatcher cases first; corpora separate |
| fortran-lang/fpm | real CPP/Git-dependency project; separate pinned reference fpm from candidate fpm to avoid self-recursion |
| selected fixtures in pinned fortran-lang/fpm | small library/app, test-only, modules/submodules, includes, mixed language and dependencies; inspect actual paths/commands |
| fortran-lang/stdlib | preprocessed stdlib-fpm commit or explicitly declared Fypp/CMake preprocessing; never assume raw source builds as plain Fortran |
| optional fortran-lang/stdlib-cmake-example | small distinct CMake capability row after inspecting its pinned contract |

Inspect each chosen revision, fetch exact immutable objects into owned isolated
scratch/cache, verify identity/checksums and preserve licenses. Reuse downloads
safely; no full corpus/repository copy per test/generation. Offline replay uses
existing pins or reports missing inputs. Ordinary project dependency resolution
is unchanged; **benchmark-project acquisition never occurs automatically in the
normal fo/test/Gremlin loop**.

Run the documented upstream reference workflow separately before comparing fo;
this explicit audit exception permits fpm/CMake reference builds. Preserve named
inventory and output/exit behavior, not merely aggregate counts. Unsupported,
not-run, upstream/preexisting-failure, acquisition, infrastructure, fo-regression
and cancelled rows remain visible. Do not advertise Gremlin CMake support from
an ordinary CTest pass. Partial ffc samples never become release/full-corpus
claims. Do not modify upstream projects to hide fo incompatibilities.

Third-party execution uses disposable least-privilege CI without secrets/write
credentials. Never use privileged pull_request_target for fork-controlled code,
modify live user repositories, overwrite installations, or access production
research hosts. Upstream tools such as Fypp are explicit external prerequisites,
not exceptions allowing fo-owned benchmark orchestration to become Python/shell.
Private repositories remain opt-in and never appear in public evidence.

## Ordered implementation and independent verifiers

These are ownership/dependency edges, not a single serial mega-gate:

| Work | Owner / dependencies | Small independent acceptance |
| --- | --- | --- |
| shared declared inputs | #175; coordinate #151 for fo-driver identity | declared/ignored/root-change parity across consumers |
| conservative impact | #189 using #175; policy #138 | same selection with empty/warm/prebuilt cache; real failing delta included |
| watcher and writable views | #148 / #170 using #175 | no idle captures; no missed declared dependency; private relative writes |
| exact fo driver | #151 | public-path replacement cannot switch an active Gremlin session |
| compact manifests | #165 using fx #42 and #175 | one shared payload, exact immutable restoration, no verification copy |
| transactional sessions | #166 using #170/#165, fx #43/#44 Phase A | failed candidate retains runnable prior result; concurrent views isolated |
| one engine | #167 using #166, #189 | ordinary/Gremlin identities, invalidation and actual outputs agree |
| roots then collection | #168 / fx #44 | independent roots/leases protected; collection enabled only after owner audit |
| receipt recovery | #140/#183 and native migration #161 | receipt-before-coverage recovery; marker-before-exit never earns PASS |
| quiescence | #155 with #148/#153/#154 | same owner remains resident; no payload work asleep; edit wakes; scope/completeness honest |
| decomposition | #149/#150/#185/#186 | existing behavioral parity; no new competing service or line-count gate |
| optional audit | #190 / #145 | tiny harness/acquisition correctness now; real runs asynchronous, never blocking |

Fix concrete regressions with focused local reproducers before unrelated main
promotion. Independent planning/provider work can proceed in parallel. Apply one
review per material concurrency/platform/API change and repeat for actual
findings, not every mechanical extraction. Close issues for their stated scope
and retained evidence; unrelated matrix, language migration or broad architecture
completion is not an excuse to keep finished work permanently open. This edit
does not itself close or reopen implementation issues.

## Adjacent issue ownership

| Issues | Scoped responsibility |
| --- | --- |
| #56 / #59 | unsaved-buffer LSP and opt-in deep lint; do not add them to Gremlin's critical path |
| #117 | formatter token-boundary correctness |
| #119 / #130 | lossless output, useful slow-child attribution and owned log retention |
| #129 / #144 | link-result inventory/retention and atomic complete artifact publication |
| #131 / #132 | public named dispatcher routing and per-case isolation; no forced in-process batching |
| #134 / #135 | honest timeout diagnosis and path-dependency freshness |
| #139 / #172 | scoped process ownership and explicit platform limitations |
| #142 / #185 | reconnectable CLI/MCP parity, explicit persistent/task lifetimes, and removal of transport-owned execution |
| #161 / #163 / #184 | native fixture migration, interpreter cleanup and lower test cost without weakening oracles |
| #188 | test-only Git-dependency bootstrap without dummy root library or redundant full build |

Closed administrative issue #62 (fortrun archive/retirement) is also not active fo implementation work.

Closed out-of-scope research issues #120 (proof/synthesis) and #157 (complete
host toolchain/runtime capture) remain historical references only; do not assign
implementation work from them. Earlier capsule/Nix/FortOS issues #1–#7
are likewise historical.

Completed foundations (#141/#153/#154/#158/#159/#160/#162/#164/#169/#171/#173/
#174/#176/#177/#178/#179/#180/#181/#182, fx #42/#43) are reused. Check live issue
state before assigning; linked issue threads carry later updates. Do not reopen
#176 because the new audit asks different measurement questions.

## Test implementation, execution and handoff

All fo-owned test/benchmark assertions and orchestration are Fortran, with narrow
test-only C OS/process/clock shims. Preserve independent protocol parsing where
sharing production parsing would mask faults. Declarative workflow/configuration
files are allowed. Upstream reference tools keep their documented dependencies.
Do not add generic fault-injection/interception machinery or meta-tests asserting
plan text, file counts, module layout or issue status. Test observable behavior.

Classify checks by purpose: fast correctness, deliberately expensive correctness,
and optional performance/third-party audit. Use existing --all/slow semantics for
slow correctness, and an explicit audit command/workflow for performance. Merely
renaming a benchmark *_slow is insufficient if it still enters the default
Gremlin full inventory. Safety deadlines prevent hangs; they are not speed goals.

Use a controller-supplied immutable absolute fo executable path and SHA256 for
validation; verify before use. Missing/mismatched binaries fail, with no PATH,
newest-file or installed-tool fallback. Keep normal worktree builds private;
only explicit controller installation changes the global driver. #151 makes
pinning automatic for Gremlin children. Historical installed digests in the old
plan are evidence, not today's executable selection.

Coding workers own disjoint worktrees/files/APIs. Parallel mode uses the existing
external luna coordinator/workers; serial mode stays in the main session. After
two substantive failed attempts on one task, freeze its evidence, stop that
writer and transfer only that task to Sol (native parallel worker or current
GPT skill in serial mode). No model calls are needed by test fixtures. Preserve
workspace host restrictions; this planning change authorizes no production-host
execution or unrelated compiler semantic changes.

Update this plan/ROADMAP and affected issues after meaningful delivery, with exact
source/driver pins, selected checks, real outcomes and remaining limitations.
Use short evidence updates, not duplicate historical narratives. Keep coding
while optional audits run; no asynchronous benchmark promise is a correctness
receipt. The next agent should implement #190's workflow separation early,
continue #175/#189 and finish current correctness slices without waiting for a
performance dashboard.
