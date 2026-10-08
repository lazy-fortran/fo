# Fo delivery goals

Updated 2026-10-08. Issues define observable success; this plan orders delivery.
Apply [goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md), choose
the smallest complete change, and preserve supported behavior and independent
failure detection. The workspace master plan, when present, owns delivery order.

## Current delivery

The stopped Claude session is recovered. Main includes its published
readiness/recovery/test-runner fixes, install diagnostics and Darwin fixtures.
The controller independently passed five resident baseline cases on `31c795b`;
the full inventory remains 129 cases, with current full coverage still open.
Fx main `f9a3f769905ccd74895b2e43e0e7ab1d213996d4` rejects invalid store roots
before metadata writes. Its owning tests passed and the controller rechecked
leases/GC (2/2). Fo's source-cleanliness increment passed bootstrap/manifest
(2/2) on exact main source with that published Fx dependency and pinned driver
SHA256 `42e03e2cbc4728212606bc5097a1f3aa1a89967e741fa06eac1b883245223268`.
#211 remains open because the historical sporadic caller is unidentified.

Combined source `116a209` passes eight Linux focused cases and five required
resident cases (`local_gate_green`, generation `9fd5b9e7…254c9b2`) through
read-only driver `f29c8249…a07e28`. It preserves Darwin watcher changes, exact
generation coverage, CMake compiler profiles and physical input paths. Native
FPM builds now honor package naming, implicit/source-form policies, declared
include directories and test-only Git/dev closures. Serial construction of
per-source cache flags fixes a reproduced parallel warm-restore race; three
independent repeats restore thirteen objects without recompilation.

The combined FPM increment passes four Darwin focused cases. The preceding
platform candidate passed ten focused cases and 128/129 full cases; the sole
failure expected lexical `/var/tmp` instead of the compiler's physical
`/private/var/tmp` source path. Its corrected fixture passes independently.
This does not claim a full green run of the new 130-case source.

Windows Server 2025 is reachable through authenticated WinRM without using the
active TPX console. Native Win32 delivery uses isolated MSYS2 UCRT64 GCC,
gfortran, CMake, Ninja and fpm; Cygwin evidence is provisional. Native store
and process probes are advancing, but a native Fo build/full Windows green
remains open. Separate workers own custom-include resident capture, unsaved
LSP diagnostics, deep lint, safe cleanup, native fixture migration and the
first native CMake consumer slice. Exact sources, drivers and receipts remain
under `/var/tmp/fo-platform-controller-20261008` and task-specific evidence roots.

Earlier handoff evidence follows; it does not replace current platform gates.
The reviewed Claude handoff is published on main, based on
`95490c97f73e53b2b9c70a61b287bd3aa36b8d38`. On exact source `7056854` and pinned
driver `369515f7…f786` the final gate passed 42/42 Linux cases, 20/20 Darwin
public cases and 15/15 Darwin resident cases (faepmac1, user-authorized). These
selections do not claim the full Fo inventory. Nested host admission delegates
bounded capacity to descendants and retains upstream reservations through owner
death and payload drainage. Recovery publishes descendant identities before TERM
removes their ancestry. The frozen registry replays the captured value 19 instead of
the live value 31 for project roots up to the supported 512 characters and
rejects longer ones explicitly. The Darwin resident gate exposed that the state
portability test read undeclared C sources. Both are now declared fixtures.
After installation, `test_native_registry_cli` (outside the Darwin selection)
failed on Darwin: its restricted tool PATH lacked the `realpath` and `dirname`
that Xcode's `as` wrapper needs. It now passes on both hosts.
An earlier loaded Linux run failed two resident readiness fixtures on their fixed
30 s budget; #208 tracks that fixture defect.

| Handoff goal | Independently checked behavior |
| --- | --- |
| #207 private scratch | Parent TMPDIR reaches CLI children; parallel exits are checked; async fixtures use registered private scratch |
| #119 lossless results | Large CLI/MCP/check reports, late failures, UTF-8 and diagnostics survive; JSON compilation failures and malformed result records retain a nonzero process exit |
| #203 native local registry | Exact/latest identities, regular/transitive/root-dev dependencies, content/version/config edits and frozen replay after stop through physical directory aliases |
| #144 safe artifacts | Owned bytes enter cache before publication; corrupt shared images are rejected; conflicting producers preserve complete artifacts |
| #135 dependency freshness | Source/include/C-header/flag/compiler changes, restored timestamps, missing outputs, private-body runtime changes, long external dependency paths and warm reuse |

Review also repaired registry watcher reactivation, frozen compiler-view roots,
terminal-session replay, physical identity for aliased frozen roots, and the Darwin test harness's concurrent process-group
setup. The harness now has one group writer and cleans up a child whose group
has not yet been established. Independent native controls and timeout/grandchild
oracles distinguish this defect from artifact failures. External dependency
object basenames use full path digests so deep private execution views compile
without exceeding the filesystem component limit. Backend fixtures identify
providers by independent exported symbols and verify warm artifact reuse with
nanosecond timestamps. Gremlin keeps elapsed-clock values in real64 throughout
supervision and bounded reproduction, avoiding false timeout jumps on Darwin
epoch-based clocks. The Darwin platform-link fixture passes complete paths to
OS listing/image tools with dynamic arguments and retains failed scratch before
cleanup; resident paths had exposed truncated test-only argument arrays.

The #205 JSON increment removes the duplicate scanner and verifies strict
receipt recovery with nested metadata. It removes 40 physical lines across its
source and tests; #205 and #150 remain open for their broader goals. Fx main
`63f010feff379452f849995f91af327af6e87a31` supplies include-search-aware source
keys, verified against real compiled runtime changes and rechecked through Fo.

Exact source, pinned drivers, commands and receipts are retained under
`/var/tmp/fo-codex-controller-20261008/combined`; Darwin evidence is retained
under `/var/tmp/fo-codex-144-darwin-20261008` on faepmac1. FPM bootstrap defects have tested owning-repository repairs and public reports
[fpm#1334](https://github.com/fortran-lang/fpm/issues/1334) and
[fpm#1335](https://github.com/fortran-lang/fpm/issues/1335). Linux and Darwin
warm runtime checks pass without clearing caches; upstream scope review
precedes a PR. Fo's native build path remains the delivery authority.

Fx #57 remains open. The current resident diagnostic build repeated an allocator
abort. An ASAN Fo build independently identifies an out-of-bounds access in Fx
local-result matching through a deferred-character function result during
parallel cache restoration. Fx `63f010f` replaces that private result with a caller-owned
output; the same varied-path oracle fails under ASAN on old code and passes 640
cold/warm pairs after repair, including the controller recheck. Evidence is retained under
`/var/tmp/fx-heap-audit-20261008`; The original instrumented Fo parallel consumer recheck passes with eight jobs.
The older allocator aborts remain unattributed until evidence links them.

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
| Resident lifecycle and exact driver behavior | #139, #142 |
| Relevant edits and quiet completion | #155 |
| Reliable process/session lifetimes, CLI/MCP and Darwin support | #139, #142 |
| Durable receipts | #140 |
| Trustworthy inputs and affected-first finite testing | #175, #189, #138 |
| Correct shared dispatcher routing | #131 |
| Test-only project execution | #188 |
| Independent writable execution and artifact coverage | #166, #170 |
| Reusable stable generations with less storage/I/O | #165, #168 |
| Less duplicated build, source, protocol and scheduling behavior | #149, #150, #167, #185, #186 |
| Smaller useful native tests and no Fo-owned script runtime | #161, #163, #184 |
| Safe cleanup, useful progress and correct timeout attribution | #129, #130 |
| Source-clean store metadata and invalid-root rejection | #211 |
| Remaining native FPM manifests and dependency forms | #201, #202 |
| Faithful native ITpPlasma profiles | #192–#198; optional reuse #199 |
| Independent optional project/performance evidence | #145, #190 |
| Adjacent formatting, LSP and explicit deep-lint behavior | #117, #56, #59 |

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
