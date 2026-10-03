# fo adjacent roadmap

Updated 2026-10-03. The complete Gremlin roadmap, bootstrap and ordered provider
issues are in [PLAN.md](PLAN.md). This file contains adjacent fo responsibilities,
not a competing Gremlin schedule. The workspace master orders repositories;
[ffc PLAN.md](https://github.com/lazy-fortran/ffc/blob/main/PLAN.md) owns compiler
language work. Current delivery is plans only, with execution deferred.

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

Past August snapshots and pipeline narratives remain in Git history. The
planning input is fo main `2e781fd`; no integrated Gremlin runtime or new matrix
has been verified on main. Paused worker candidates remain unpromoted. Rewrite
PLAN state and modify/close issues with exact evidence after execution is later
authorized. Controller alone promotes explicit reviewed paths to main.
