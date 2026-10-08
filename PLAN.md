# Fo delivery goals

Updated 2026-10-08; reconciled with published main and GitHub issue states.
Issues define observable success; this plan orders delivery.
Apply [goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md), choose
the smallest complete change, and preserve supported behavior and independent
failure detection. The workspace master plan, when present, owns delivery order.

## Current delivery

Handoff requested by the user on 2026-10-08: publish the assembled work and stop
for another computer to continue. Main contains the native Win32 provider,
complete multi-module cache artifacts, private resident scratch, absolute FPM
path capture, declared executable directories, an opt-in native CMake slice,
and opt-in deep lint. This is a source handoff, not a stable release: the final
combined source has not completed its runtime gates or full platform suites.

The reviewed source baseline is Fo `de85cf9`, Fx `f55e30d` and Fluff
`9076ff6` on their published main branches. The local Fluff checkout is older;
use the published provider for the deep-lint recheck. The recorded handoff
stopped task processes and preserved the VM/TPX and evidence. Worker assignments
and driver paths in earlier receipts do not identify an active lane now.

For Linux and Windows testing, SSH to **mailuefterl** (`ssh mailuefterl`).
The source checkouts on that host are:

- Fo: `/home/ert/code/lazy-fortran/fo` (published `main`).
- Fx: `/home/ert/code/lazy-fortran/fx` (published `main`).
- FortFront: `/home/ert/code/lazy-fortran/fortfront`.
- FFC: `/home/ert/code/lazy-fortran/ffc`; preserve its unrelated local edit.
- Fluff: `/var/tmp/fo-fluff-reference-20261008` (published `main`).

Run Linux checks on mailuefterl. Its running Windows VM is `tpx-win2025`;
use the existing authenticated WinRM helper
`/var/tmp/fo-windows-20261008/run_winrm.py` through `127.0.0.1:15985` on that
host. Windows scripts, source archives and receipts are under
`/var/tmp/fo-windows-20261008`; native guest task files are under
`C:\fo-windows-20261008`, with the isolated UCRT64 toolchain at
`C:\fo\msys64\ucrt64`. Rebuild from the current source checkouts: earlier
Windows archives and pinned executables precede the final combined main.

The excluded allocator proposal is in
`/var/tmp/fo-linux-compile-flags-20261008`; the excluded barrier experiment is in
`/var/tmp/fx-action-publication-oracle-20261009`. These are unfinished task
worktrees, not the published source. The local evidence paths below refer to
mailuefterl; the Darwin worker's handoff was copied there too.

Resume in this order:

1. Repair the remaining reproducible Linux parallel allocator/parser failure
   tracked under Fx #57. The earlier `63f010f` ASAN repair and original Fo
   eight-job consumer recheck passed; this later failure remains unresolved.
   The flags-preparation proposal still fails and was excluded from main.
2. Build the exact combined Fo/Fx source and recheck resident scratch recovery,
   absolute dependencies, naming, compiler paths and declared executable/install
   behavior. The readiness stamp was advanced to invalidate old discovery.
3. Finish native Windows coherent build/replay/cleanup tests: a simple Fortran
   pipe variant still increases the handle count; permission and full-suite
   gates remain open. The VM and TPX were left running, with no task processes.
4. Run deep lint against repaired Fluff and fix its remaining Windows fixture
   assumptions; recheck the original Darwin Fluff submodule consumer. Verify
   native CMake against the actual GORILLA oracle and then the maintained
   ITpPlasma profiles. The current CMake slice is deliberately
   partial; neither broader goal is complete.
5. Correct the recovery-test barrier only after its deterministic reproducer
   passes; its experimental patch was excluded. Recheck the parallel-restore
   oracle's OpenMP flags and timeout. Then run exact full platform coverage and
   delete obsolete scripts only after their replacements pass.

Local evidence remains in `/var/tmp/fo-platform-controller-20261008/resumed`,
`/var/tmp/fo-linux-compile-flags-evidence-20261008`,
`/var/tmp/fo-deep-lint-20261008-transfer/final-worker-handoff.json`, and
`/var/tmp/fo-windows-20261008/windows-stopped-handoff.json`. Existing source
branches retain earlier task commits. FPM's verified include-path repair is
[upstream PR #1339](https://github.com/fortran-lang/fpm/pull/1339).

## Delivered slices and remaining evidence

The 2026-10-08 GitHub audit finds **36 open issues**. Unsaved-buffer LSP #56,
formatting #117, test-only projects #188, path/Git closures #202, local registry
#203 and initial resident dogfooding #200 are closed. Their delivered behavior
remains available; standalone FPM manifest coverage #201 is still open.

Published increments include declared-header capture/edit/frozen replay,
PURE/ELEMENTAL submodule companion restoration, complete per-source module
artifact vectors, native Windows adapters, private execution scratch and
absolute FPM dependency capture. Earlier Linux/Darwin focused gates passed on
recorded exact sources. The original Darwin Fluff submodule consumer recheck,
combined-source runtime gates and full platform suites remain open. Historical
129/130-case counts and mixed-generation receipts do not establish the current
inventory or complete green.

Fx #57 remains open for broader warm-cache behavior and the remaining Linux
failure. The demonstrated deferred-character restoration race was repaired in
Fx `63f010f`: the independent varied-path oracle passed 640 cold/warm pairs,
and the original instrumented Fo parallel consumer passed with eight jobs.
Older allocator aborts remain unattributed. That completed repair must stay
distinct from the failed flags-preparation proposal in the current handoff.

#211 remains open for the unidentified historical invalid-store-root caller.
Fx `f9a3f76` rejects invalid roots before metadata writes; owning checks and
focused Fo bootstrap/manifest source-cleanliness checks passed. Continued
resident coverage and caller attribution remain necessary.

FPM path-cache/archive reports [#1334](https://github.com/fortran-lang/fpm/issues/1334)
and [#1335](https://github.com/fortran-lang/fpm/issues/1335) remain open; their
historical Linux/Darwin warm-runtime checks passed without clearing caches.
The separate include-directory repair is submitted as open upstream PR #1339.
Fo's native build path remains the delivery authority.

Detailed earlier source/driver identities, test selections and evidence paths
are preserved in [the audited handoff plan](https://github.com/lazy-fortran/fo/blob/de85cf9cdb8710fab414cfae7184476c311d3b89/PLAN.md)
and the evidence roots above. This documentation review does not rerun or extend
those runtime claims and does not change GitHub issue states.

## Delivery order

1. Sustain resident Fo dogfooding; repair concrete workflow blockers.
2. Reduce duplicated maintained code through #205 alongside useful delivery.
3. Keep FFC's FPM project and shared dispatcher usable in Gremlin. This initial
   handoff and the three-current-compiler-defect pilot are delivered; full FFC
   coverage and the separately owned complex SIN case remain open.
4. Finish standalone FPM compatibility on unchanged supported manifests.
5. Finish native CMake/CTest compatibility for the maintained ITpPlasma profiles.

Only actual consumer blockers gate the next consumer task. Publish exact
locally green increments promptly, without waiting for GitHub CI or unrelated
full coverage. Use pinned candidate copies, warm caches and task-private TMPDIR;
stop and remove completed task-owned lanes/worktrees after verifying ownership.

## Product goals

- The fastest practical local build/test feedback, particularly for agents and
  worktree swarms, with serial and parallel development supported externally.
- One-shot commands stay bounded. Gremlin provides resident edit feedback,
  last-compilable retention, current-generation evidence, finite coverage and
  quiescence with reliable wakeup.
- CLI and MCP have equivalent first-class capabilities and truthful errors.
- Correct shared reuse and inexpensive warm operation; no stale results or
  duplicated maintenance machinery merely to share an interface.
- Fortran-owned implementation/tests/orchestration with small C OS shims;
  consumer/reference tools retain their own legitimate prerequisites.

## Active goal owners

| Outcome | Issues |
| --- | --- |
| Substantially less maintained code across the affected stack | #205 |
| Resident lifecycle and exact driver behavior | #139, #142 |
| Relevant edits and quiet completion | #155 |
| Reliable process/session lifetimes, CLI/MCP and Darwin support | #139, #142 |
| Durable receipts | #140 |
| Trustworthy inputs and affected-first finite testing | #175, #189, #138 |
| Correct shared dispatcher routing | #131 |
| Independent writable execution and artifact coverage | #166, #170 |
| Reusable stable generations with less storage/I/O | #165, #168 |
| Less duplicated build, source, protocol and scheduling behavior | #149, #150, #167, #185, #186 |
| Smaller useful native tests and no Fo-owned script runtime | #161, #163, #184 |
| Safe cleanup, useful progress and correct timeout attribution | #129, #130 |
| Source-clean store metadata and invalid-root rejection | #211 |
| Remaining native FPM manifests and dependency forms | #201 |
| Faithful native ITpPlasma profiles | #192–#198; optional reuse #199 |
| Independent optional project/performance evidence | #145, #190 |
| Explicit deep-lint behavior | #59 |

These are goal owners, not a prescribed service/module decomposition or one
serial dependency chain. Existing delivered behavior should be reused. Resolve
an actual correctness defect before adding mechanisms around it.

## Compatibility destinations

Standalone FPM means the supported manifest, dependency, build/test/run/install
semantics execute without installed FPM. Verify behavior against a pinned
independent reference, including execution with FPM absent. The initial Fo/FFC
subset and path/Git/registry/executable-install issues do not claim full parity;
remaining required forms need explicit supported behavior or diagnostics.

Standalone CMake/CTest means the required ITpPlasma profiles execute without
CMake/CTest while preserving their project configuration, generated inputs,
dependencies, test/fixture/resource/verdict rules and scientific options.
[The profile goals](doc/CMAKE_GREMLIN_PLAN.md) define this finite destination.
Delegated execution remains identified interim support. SIMPLE/NEO-2 reviews,
golden-record requirements and production-host restrictions remain unchanged.

## Evidence and maintenance

Use independent public behavior and the smallest relevant focused checks on the
exact integrated candidate. A cached compilation is not a cached test PASS.
Build failure preserves last-compilable testing; old-generation evidence cannot
fabricate current full green. Full fixed-version verification remains a
completion/milestone claim, not a per-edit wait.

Benchmarks and acquired-project audits are opt-in workloads outside ordinary
coverage/quiescence. Measure real byte edits and report raw identity/timing/work
counts. Correctness defects discovered there receive focused reproducers;
slow measurements remain advisory.

Earlier details and historical evidence remain at
[the exact prior plan](https://github.com/lazy-fortran/fo/blob/95490c97f73e53b2b9c70a61b287bd3aa36b8d38/PLAN.md).
They explain earlier decisions; active goals and current accepted contracts
govern new work. Keep this plan short and update the owning issue when work moves.
