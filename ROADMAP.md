# fo adjacent roadmap

Updated 2026-10-03. The complete Gremlin roadmap, bootstrap and ordered provider
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
| [#138](https://github.com/lazy-fortran/fo/issues/138), [#139](https://github.com/lazy-fortran/fo/issues/139), [#140](https://github.com/lazy-fortran/fo/issues/140), [#141](https://github.com/lazy-fortran/fo/issues/141), [#142](https://github.com/lazy-fortran/fo/issues/142), [#143](https://github.com/lazy-fortran/fo/issues/143), [#144](https://github.com/lazy-fortran/fo/issues/144), [#145](https://github.com/lazy-fortran/fo/issues/145) | Gremlin enabling stages, atomic link publication and maintained/external project matrix; exact sequence in PLAN. |

Adjacent scope is [#56](https://github.com/lazy-fortran/fo/issues/56) LSP,
[#59](https://github.com/lazy-fortran/fo/issues/59) deep-lint provider,
[#62](https://github.com/lazy-fortran/fo/issues/62) fortrun retirement and
[#120](https://github.com/lazy-fortran/fo/issues/120) accepted Synthesis pipeline.
They do not delay Gremlin unless an exact current dependency blocks its verifier.
Future proposal acceptance belongs to standard; no speculative syntax expansion
is implied by this queue.

## Verification and status

Run focused independent behavioral checks, then the normal fo delivery pipeline,
recording source/toolchain/dependency and artifact identities. Gremlin continues
background bounded tests while implementation proceeds; full fixed-version
matrix evidence remains completion criteria. CLI and MCP have one shared engine
and feature set; use current CLI after fixes when MCP cannot reload.

Past snapshots remain in Git history. Gremlin progress is now published through
draft core PR #146 and stacked work-mode PR #147; `main` remains at the last
accepted release until the architecture and exact-head verification gates pass.
Issues #148--#151 own the core change-event, supervisor, JSON and execution-
identity consolidation. #153--#155 add finite coverage epochs, explicit
promotion/full-verification state and event-driven quiescence. #152 owns the
work-mode modularization after core merges. Early matrix evidence includes
one bounded fpm pass, one independent CMake/CTest pass with a fo discovery defect,
and one honest missing-dependency block. Controller alone promotes reviewed
paths to main and pushes every integrated milestone while it is under review.
The work-mode stack stays frozen while core semantics change.
