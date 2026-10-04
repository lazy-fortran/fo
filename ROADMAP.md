# fo issue ownership and adjacent roadmap

Updated 2026-10-04. [PLAN.md](PLAN.md) is the authoritative execution and provider
plan; this file maps responsibilities, not a competing schedule. Issue bodies
carry focused acceptance and later delivery evidence. Check live issue state
before assigning work; planning edits do not prove implementation complete.

## Product boundary

Keep fo in the build/test developer-tool lane: fpm/CMake project discovery,
incremental compilation/cache, run/test/affected/continuous testing, formatting,
lint/static diagnostics, LSP/MCP convenience and closely related correctness
infrastructure. Implementation and focused verification are active; follow
PLAN's staged standalone FPM and defined ITpPlasma CMake replacement target.
Environment/system package management, scientific provenance
capsules, proof/synthesis, containers, CI/promotion and agent orchestration are
outside the product.

Closed historical research scope includes #1–#7; #120 and #157 are now
closed not planned. Hermetic environments are supplied externally. Internal
bench/project audits may use pinned disposable environments but do not become
public fo environment-management features.

## Development versus audits

Use warm focused local correctness gates and push small exact green increments
to main. GitHub CI is asynchronous; benchmarks and third-party matrices are
explicitly **non-blocking scheduled/manual CI or optional slow audits**. They are
not main/merge gates, required development checks, or prerequisites for unrelated
issue closure. Confirmed current correctness regressions still get repair/revert
priority. Slow correctness cases and safety deadlines are not timing benchmarks.

[Performance audit #190](https://github.com/lazy-fortran/fo/issues/190) owns real
byte-edit scenarios, edit-to-verdict phases, counters, raw paired measurements,
advisory reporting and moving benchmark execution out of the normal CI job.
[Project audit #145](https://github.com/lazy-fortran/fo/issues/145) owns automatic
allowlisted pinned acquisition and independent reference workflows for ffc,
fortran-lang/fpm, selected fpm fixtures and stdlib/preprocessing/CMake examples.
Acquisition is isolated and explicit; normal fo/test/Gremlin never downloads a
benchmark matrix. Ordinary declared project dependency bootstrap is unchanged.

## Delivery milestones

Follow PLAN's six milestones: resident Fo Gremlin
[#200](https://github.com/lazy-fortran/fo/issues/200), cleanup through that loop,
FFC's FPM-only workflow, repairs for up to three confirmed current FFC failures,
standalone FPM compatibility, then the required native ITpPlasma CMake profiles.
Only demonstrated bootstrap blockers precede the first resident lane.

The initial standalone FPM work is split into independently verifiable issues:

| Issue | Fo-owned native scope |
| --- | --- |
| [#201](https://github.com/lazy-fortran/fo/issues/201) | Current Fo/FFC manifest subset and preserved Fo extensions. |
| [#202](https://github.com/lazy-fortran/fo/issues/202) | Path and pinned-Git closure/acquisition without an FPM process. |
| [#203](https://github.com/lazy-fortran/fo/issues/203) | Namespaced registry and version resolution. |
| [#204](https://github.com/lazy-fortran/fo/issues/204) | Executable installation into an explicit prefix. |

These are initial slices, not full FPM replacement. Required remaining acceptance
includes all supported target declarations, features/profiles/preprocessing,
remaining dependency/metapackage and registry behavior, public command/options
parity, and library/module installation. Milestone 5 remains open until these
are implemented and verified with FPM absent. Implement FPM semantics within Fo;
generic libraries are allowed, FPM-specific source imports are not. The existing
FPM-backed #188 repair is an interim consumer fix, separate from #202.

## Current implementation spine

| Issues | Responsibility and dependency boundary |
| --- | --- |
| [#175](https://github.com/lazy-fortran/fo/issues/175) | One canonical declared logical input inventory; watcher, capture, scratch, build and impact consume it. |
| [#189](https://github.com/lazy-fortran/fo/issues/189), [#138](https://github.com/lazy-fortran/fo/issues/138) | Cache-independent baseline/candidate impact, whole required-set gating, reproducers first, finite background coverage. |
| [#148](https://github.com/lazy-fortran/fo/issues/148), [#170](https://github.com/lazy-fortran/fo/issues/170) | Declared-input event relevance and private writable execution views; no second watch policy. |
| [#151](https://github.com/lazy-fortran/fo/issues/151) | Pin fo's own driver so an active Gremlin session cannot switch implementation mid-run; host toolchain capture is out of scope. |
| [#165](https://github.com/lazy-fortran/fo/issues/165) | Compact generation manifests over shared immutable blobs, not copied payload trees. |
| [#166](https://github.com/lazy-fortran/fo/issues/166), [#167](https://github.com/lazy-fortran/fo/issues/167) | Private transactional sessions and one ordinary/Gremlin engine; separate compile/link/test identities. |
| [#168](https://github.com/lazy-fortran/fo/issues/168), [fx #44](https://github.com/lazy-fortran/fx/issues/44) | Phase A roots/leases with collection disabled, then owner-audited Phase B collection. |
| [#140](https://github.com/lazy-fortran/fo/issues/140), [#183](https://github.com/lazy-fortran/fo/issues/183) | Durable per-case receipts and exact receipt/coverage crash recovery; markers are not PASS. |
| [#155](https://github.com/lazy-fortran/fo/issues/155) | Quiescence/wake over configured local scope, excluding optional audits. |
| [#188](https://github.com/lazy-fortran/fo/issues/188) | Test-only Git dependency bootstrap without dummy root libraries or full root builds. |

[fx #42](https://github.com/lazy-fortran/fx/issues/42) immutable blobs/trees and
[fx #43](https://github.com/lazy-fortran/fx/issues/43) action-result publication
are delivered foundations. Phase A of fx #44 precedes long-lived session graph
adoption; collection remains off until all live ownership is represented. No
new database, cache daemon or application-level blob RAM cache is planned.

## Remaining integration and architecture

| Issues | Scope |
| --- | --- |
| [#119](https://github.com/lazy-fortran/fo/issues/119), [#130](https://github.com/lazy-fortran/fo/issues/130) | Lossless public output, useful slow-child attribution and bounded owned logs. |
| [#129](https://github.com/lazy-fortran/fo/issues/129), [#144](https://github.com/lazy-fortran/fo/issues/144) | Link/result inventory, atomic complete artifact publication and ownership-safe retention. |
| [#131](https://github.com/lazy-fortran/fo/issues/131), [#132](https://github.com/lazy-fortran/fo/issues/132) | Shared executable/public case routing with per-invocation isolation; no mandatory in-process batching. |
| [#134](https://github.com/lazy-fortran/fo/issues/134), [#135](https://github.com/lazy-fortran/fo/issues/135) | Truthful timeout/accounting and path-dependency freshness. |
| [#139](https://github.com/lazy-fortran/fo/issues/139), [#172](https://github.com/lazy-fortran/fo/issues/172) | Scoped process ownership and actual platform limits, not PID/name guesses. |
| [#142](https://github.com/lazy-fortran/fo/issues/142), [#185](https://github.com/lazy-fortran/fo/issues/185) | CLI/MCP lifecycle parity and removal of transport-owned scheduling. |
| [#149](https://github.com/lazy-fortran/fo/issues/149), [#150](https://github.com/lazy-fortran/fo/issues/150), [#186](https://github.com/lazy-fortran/fo/issues/186) | Cohesive campaign/command services, shared strict JSON, Fortran-owned discovery policy. |
| [#161](https://github.com/lazy-fortran/fo/issues/161), [#163](https://github.com/lazy-fortran/fo/issues/163), [#184](https://github.com/lazy-fortran/fo/issues/184) | Remaining native fixture migration and useful test-cost reduction; no doc/layout conformity suite. |

Completed #141/#153/#154 and native/concurrency/platform slices
#158/#159/#160/#162/#164/#169/#171/#173/#174/#176/#177/#178/#179/#180/#181/#182
are foundations to reuse, not tasks to redo. Historical exact evidence is linked
from PLAN. #176 completed the benchmark harness migration, **not** real-edit
performance validation; #190 owns the new measurement work.

## Adjacent in-scope developer capabilities

| Issue | Scope |
| --- | --- |
| [#56](https://github.com/lazy-fortran/fo/issues/56) | Unsaved-version LSP diagnostics; never build/test on every keystroke. |
| [#59](https://github.com/lazy-fortran/fo/issues/59) | Explicit optional fluff deep lint, separate from cheap native checks. |
| [#117](https://github.com/lazy-fortran/fo/issues/117) | Formatter identifier/keyword boundary correctness. |


#62 fortrun administration is closed not planned in the fo implementation roadmap.

Closed out-of-scope research directions remain in issue history rather than the active roadmap: #120 proof/synthesis and #157 complete host toolchain/runtime capture, plus earlier capsule/Nix/FortOS issues #1–#7.

[ffc #798](https://github.com/lazy-fortran/ffc/issues/798) is a narrow consumer of
fo's shared leaf/corpus runner and exact input model. fo's optional matrix may
fetch a pinned isolated ffc copy as authorized, without starting unrelated
compiler language work or changing ffc's release dashboard. Its giant corpora
are not a prerequisite for fo development.

## Coordination

Core PR #146 is merged. PR #147 and #143/#152 were abandoned: external controllers
own coding agents/worktrees/integration. Do not add GitHub, CI, promotion or model
scheduling inside fo. Protected scientific repositories retain their external
review/CI policies; this fast-main convention is lazy-fortran's current policy.

Each changed behavior receives a bounded named native independent correctness
oracle and a pinned exact driver. Broad correctness suites remain asynchronous
or milestone evidence, not a full rerun per mechanical extraction. Benchmarks
remain optional audits even at milestones. Preserve failures and unsupported
cases; never convert partial/cancelled evidence into full green.

Update the owning issue and PLAN after meaningful delivery. Keep historical
measurements labelled, close only actually completed scope, and avoid duplicate
narratives or blanket dependencies on every issue in the programme.
