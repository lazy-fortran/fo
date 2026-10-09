# Fo delivery goals

Updated 2026-10-09; current development-support delivery and retained backlog.
Issues define observable success; this plan orders delivery.
Apply [goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md), choose
the smallest complete change, and preserve supported behavior and independent
failure detection. The workspace master plan, when present, owns delivery order.

## Current delivery

Current scope is active completion of the Fo issue inventory, isolated local
verification and regular gated main pushes. One controller integrates parallel
source workers; each host admits one heavy campaign. Full platform and consumer
qualification remain completion gates in this delivery.

Main `7a434fb` removes all thirteen remaining checked-in Python/JavaScript
fixtures and their redundant CI launches. Useful native behavioral coverage
replaces the Python reproducers and reuses the existing MCP, journal and process
fixtures. The same increment repairs MCP envelope validation, native manifest
debug information, required slow-test impact and frozen CMake reproduction.
It also deletes the unused debug-flag compatibility helper and its shallow tests.
The combined diff removes 1880 maintained lines across 34 files.

The exact Linux candidate passes 23/23 required cases on one immutable resident
generation in offline, unprivileged container `fo-complete-linux-20261009`.
The qualification image contains neither Python nor Node. Native architecture
filter probes and actual i386/x32 containment probes pass separately. The new
MCP, DWARF and affected-slow oracles detect their intended parent failures.
Receipt, driver identity and promotion evidence are under
`/var/tmp/fo-complete-20261009/output`; the promotion receipt is
`promotion-7a434fb.json`. This is a focused gate, not a complete platform suite.

Darwin qualification is active on authorized faepmac1. Remaining source workers
own canonical capture simplification, safe retention, dependency completeness,
cleanup action history, idle MCP progress and native Windows fixtures. Deep lint
also requires the witnessed owning Fluff configuration/import repair. Each
candidate still needs combined local verification before promotion.

Fo `feb2318` is now published after 13/13 combined resident cases pass on fixed
generation `8b472de606fb`, using digest-pinned driver `fcaabca506e1`. Pending MCP
work progresses while clients are idle; failed fixtures retain their evidence
outside owned temporary scopes; child/execution-view fixtures use native Windows
interfaces. Fx `9e97af4` provides the native timed reader, with three transport
gates and the recorded ASAN check passing. Promotion receipt:
`/var/tmp/fo-complete-20261009/output/promotion-feb2318.json`.

Current follow-up repairs Fo's dependency C-header planning, which incorrectly
resolves transitive development dependencies. Canonical inventory capture and
duplicate artifact/test cleanup are next. Darwin's frozen 143-case milestone
and a fresh Windows native qualification remain active; the historical native
Windows bootstrap hit a stack overflow while building current source, so its
output is not yet a qualified candidate. These focused deliveries do not claim
complete platform, FPM, deep-lint or scientific CMake coverage.

The earlier development deployment remains recorded below for its exact scope.

Fo source `2ac0121` and Fx `090acb4` are published on main. The Fx repair makes
manifest encoding length metadata caller-owned: an independent Linux ASAN/OpenMP
oracle changes from ten parse failures to zero across 20000 varied-length calls.
The focused immutable-store test passes with eight jobs/OpenMP. The original
Fo parallel library-include consumer passes with eight jobs on the rebuilt
combined driver. This repairs the reproduced parser race; broader Fx #57
warm-cache qualification remains open.

Fo `2ac0121` exposes Darwin temporary-directory declarations in the benchmark
helper; the parent fails its C compile check and the fixed source passes.
Rebuilt release drivers are installed at `/opt/homebrew/bin/fo` locally and
`/home/ert/.local/bin/fo` on mailuefterl. Their SHA256 digests are respectively
`0a30bed211446db2e18b5e8253cc9b5440f86e2703d5ae09ffbb34323406f972` and
`65e15c374cab0c92e526a77d82f9c05cc9f057b6b57ccde5e5247ff27ab66559`.
Each build pins Fx `090acb4` in its disposable manifest; no consumer manifest
change is committed. Original manifests, build logs and deployment receipts are
under `/var/tmp/fo-finish-development-20261009` on each host.

KIN6D's registered mapped-P2 finite-forms test passes through the new Darwin
driver. Its lint JSON exactly matches the prior verified result, with 53 existing
findings. The Darwin immutable-store parent and fixed versions have the same
59 existing watcher/process-fixture failures; the new concurrency regression
passes. These focused checks do not establish full-platform green.

Fourteen abandoned task-owned Linux residents and eighteen local fixture
residents were stopped with identity checks; previous local abandoned MCP
cleanup is retained in the deployment receipts.
Live unrelated development/MCP sessions remain. Completed task worktrees were
removed after preserving source bundles, patches and diagnostics. Preservation
archives are under `worktree-preservation` and `final-worktree-preservation` in
the evidence root; the Linux stop receipt is
`/var/tmp/fo-deploy-20261009-linux/stopped-background-20261009.json`,
with the final Linux and local stop receipts in the current evidence root.

Main also contains the native Win32 provider, complete multi-module cache
artifacts, private resident scratch, absolute FPM path capture, declared
executable directories, an opt-in native CMake slice and opt-in deep lint.
The prior handoff baseline Fo `de85cf9`, Fx `f55e30d` and Fluff `9076ff6`
remains available for historical reproduction. Worker assignments and old
driver paths do not identify an active task lane.

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

The excluded allocator proposal and recovery-barrier experiment were preserved
as bundles and patches before their task worktrees were removed. They are not
published fixes; the parser repair above supplies the demonstrated Linux fix.

Remaining qualification includes resident scratch/dependency/install behavior,
Windows handle/permission/full-suite checks, deep lint with repaired Fluff,
native CMake against GORILLA and maintained ITpPlasma profiles, and a corrected
recovery-barrier oracle. Existing focused receipts retain their exact scope.
None of these broader gates is claimed complete by the development deployment.

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
broader combined-source runtime gates and full platform suites remain open.
Historical 129/130-case counts and mixed-generation receipts do not establish
the current inventory or complete green.

Fx #57 remains open for broader warm-cache behavior. The reproduced Linux
manifest-parser race is repaired by `090acb4` and rechecked above. The earlier
deferred-character restoration race was repaired in Fx `63f010f`: the independent
varied-path oracle passed 640 cold/warm pairs,
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
and the evidence roots above. Current focused rechecks are recorded above; older
runtime claims retain their original scope. GitHub issue states were not changed
by this handoff.

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
