# fo adjacent roadmap

Updated 2026-10-04. The complete Gremlin roadmap, bootstrap and ordered provider
issues are in [PLAN.md](PLAN.md). This file contains adjacent fo responsibilities,
not a competing Gremlin schedule. The workspace master orders repositories;
[ffc PLAN.md](https://github.com/lazy-fortran/ffc/blob/main/PLAN.md) owns compiler
language work. Current delivery is the user-authorized fo Gremlin implementation
in parallel mode, including bootstrap and subsequent Gremlin dogfooding. ffc
implementation is outside the current objective and awaits a later explicit
trigger.

## Build and tool contracts

Retain module/submodule ancestry from source, complete action/dependency keys,
atomic cache publication, original dispatcher names and child exit status.
Performance never permits dropping a source, diagnostic, negative test or result.
Use the existing native/fpm/CMake backend contracts. Gremlin repairs missing
freshness/result/process behavior before consumers can rely on it.

## Existing issue ownership

| Issue | Responsibility |
| --- | --- |
| [#117](https://github.com/lazy-fortran/fo/issues/117) | Formatter correctness for assignment identifiers ending in function; preserve independent oracle. |
| [#119](https://github.com/lazy-fortran/fo/issues/119) | Lossless machine-readable results beyond old count/byte limits; shared Gremlin event transport consumes this. |
| [#129](https://github.com/lazy-fortran/fo/issues/129) | Link-stage/stale-artifact inventory; cleanup must preserve live ownership and useful CAS data. |
| [#130](https://github.com/lazy-fortran/fo/issues/130) | Suite visibility, slow-child timing and durable logs. |
| [#131](https://github.com/lazy-fortran/fo/issues/131), [#132](https://github.com/lazy-fortran/fo/issues/132) | Shared test binary/case dispatch; verify existing behavior before closing stale reports. |
| [#134](https://github.com/lazy-fortran/fo/issues/134) | Truthful timeout/hang status rather than confusing low CPU with deadlock. |
| [#135](https://github.com/lazy-fortran/fo/issues/135) | Path-dependency freshness and valid recompilation/link reuse. |
| [#138](https://github.com/lazy-fortran/fo/issues/138), [#139](https://github.com/lazy-fortran/fo/issues/139), [#140](https://github.com/lazy-fortran/fo/issues/140), [#142](https://github.com/lazy-fortran/fo/issues/142), [#144](https://github.com/lazy-fortran/fo/issues/144), [#145](https://github.com/lazy-fortran/fo/issues/145) | Open Gremlin enabling stages, atomic link publication and maintained/external project matrix; exact sequence in PLAN. |
| [#141](https://github.com/lazy-fortran/fo/issues/141) | Completed last-compilable retention and successful-generation preemption; the behavioral oracle remains a permanent CI gate. |
| [#148](https://github.com/lazy-fortran/fo/issues/148), [#149](https://github.com/lazy-fortran/fo/issues/149), [#151](https://github.com/lazy-fortran/fo/issues/151), [#153](https://github.com/lazy-fortran/fo/issues/153), [#154](https://github.com/lazy-fortran/fo/issues/154), [#155](https://github.com/lazy-fortran/fo/issues/155), [#157](https://github.com/lazy-fortran/fo/issues/157) | Event gating, service decomposition, execution closure, finite coverage, factual local-gate reporting and quiescence. |
| [#158](https://github.com/lazy-fortran/fo/issues/158)--[#163](https://github.com/lazy-fortran/fo/issues/163) | #158, #159 and #162 are complete, including the GCC 13.3 MCP ownership repair and publication timeout cleanup; migrate the remaining 13 fixtures plus the generated compile-database Python assertion, then remove repository-owned scripting runtimes. |
| [#164](https://github.com/lazy-fortran/fo/issues/164) | Complete cross-process stat-memo publication and recovery at `d482807`. |
| [#165](https://github.com/lazy-fortran/fo/issues/165)--[#170](https://github.com/lazy-fortran/fo/issues/170), [fx #42](https://github.com/lazy-fortran/fx/issues/42)--[#44](https://github.com/lazy-fortran/fx/issues/44) | Writable execution views, one immutable blob/action store, compact generations, private transactional build sessions, one ordinary/Gremlin engine and rooted low-churn collection. fx #44 lands its root/lease protocol before enabling collection. |
| [#171](https://github.com/lazy-fortran/fo/issues/171) | Complete through `9331c04`: state-provider declarations and portability oracles pass on Linux/macOS; #172 separately owns public lifecycle. |
| [#172](https://github.com/lazy-fortran/fo/issues/172) | Add scoped Darwin asynchronous owner/descendant containment for public Gremlin lifecycle. |
| [#173](https://github.com/lazy-fortran/fo/issues/173) | Complete: accept documented platform archive index metadata while preserving exact expected-object verification. |
| [#174](https://github.com/lazy-fortran/fo/issues/174) | Complete through `4c4a36e`: initialize and diagnose the shared Darwin change provider without adding a second polling engine. |
| [#175](https://github.com/lazy-fortran/fo/issues/175) | Own one declared execution-input inventory shared by watcher relevance, compact generation capture and later action invalidation. |
| [#176](https://github.com/lazy-fortran/fo/issues/176) | Complete through `8431a6d`: standalone Fortran benchmark orchestration, JSONL reporting and exact inventory verification replace the shell/Python executables. |
| [#177](https://github.com/lazy-fortran/fo/issues/177) | Complete: every command-local help path is side-effect free, including `fo install --help`. |
| [#178](https://github.com/lazy-fortran/fo/issues/178) | Complete at `9a8ca76`: standalone install-help C code is explicit-only and cannot enter FPM's automatic Fortran test links. |
| [#179](https://github.com/lazy-fortran/fo/issues/179) | Complete at `24018a3`: the archive-metadata Fortran oracle compiles within the portable source-line limit with byte-identical generated script behavior. |

Adjacent scope is [#56](https://github.com/lazy-fortran/fo/issues/56) LSP,
[#59](https://github.com/lazy-fortran/fo/issues/59) deep-lint provider,
[#62](https://github.com/lazy-fortran/fo/issues/62) fortrun retirement and
[#120](https://github.com/lazy-fortran/fo/issues/120) accepted Synthesis pipeline.
They do not delay Gremlin unless an exact current dependency blocks its verifier.
Future proposal acceptance belongs to standard; no speculative syntax expansion
is implied by this queue.

## Verification and status

Run the exact integrated candidate's focused local gate before each small push,
recording source/toolchain/dependency and artifact identities. Gremlin continues
background bounded tests while implementation proceeds; full pipelines and
fixed-version matrix evidence remain milestone/completion criteria. CLI and MCP have one shared engine
and feature set; use current CLI after fixes when MCP cannot reload.

Past snapshots remain in Git history. Discussion PR #146 is merged; reviewed
locally green increments now advance `main` directly while GitHub CI audits the
latest main asynchronously.
Issues #148--#151 own the core change-event, supervisor, JSON and execution-
identity consolidation. #153--#155 add finite coverage epochs, explicit
local-gate/full-verification facts and event-driven quiescence. Early matrix evidence includes
one bounded fpm pass, one independent CMake/CTest pass with a fo discovery defect,
and one honest missing-dependency block. Controller alone promotes reviewed
paths to main and pushes every integrated milestone while it is under review.
Agent scheduler PR #147 and issues #143/#152 are closed without merge; external
controllers own worker/task orchestration.


Issue #169 is complete through `af5d144`: self-refresh is worktree-private so
worker builds cannot publish a global driver; only an explicit controller install
may do that. The cold standalone Fortran oracle has an explicit 240-second wall
budget.
