# Fo delivery goals

Updated 2026-10-08. Issues define observable success; this plan orders delivery.
Apply [goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md), choose
the smallest complete change, and preserve supported behavior and independent
failure detection. The workspace master plan, when present, owns delivery order.

## Current delivery

The reviewed Claude handoff is implemented through `7fa581b`,
based on `95490c97f73e53b2b9c70a61b287bd3aa36b8d38`. Publication requires the
controller's exact combined gate; individual worker receipts do not substitute
for it. The final gate selects 42 Linux resident cases, 20 Darwin public cases and
15 Darwin resident cases. Nested host admission now delegates bounded capacity to descendants and retains
upstream reservations through owner death, payload drainage and gated
authority revocation. Recovery publishes exact descendant identities before
TERM can remove their ancestry; a direct unpolled-scope heartbeat fixture and
the real public reproduction crash oracle verify drainage. Its independent crash/stop oracles and these combined
gates must pass before publication. These selections do not claim the full
Fo inventory.
The user explicitly authorized faepmac1 for Darwin verification.

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
epoch-based clocks.

The #205 JSON increment removes the duplicate scanner and verifies strict
receipt recovery with nested metadata. It removes 40 physical lines across its
source and tests; #205 and #150 remain open for their broader goals. Fx main
`879fec283e7c5a8c13436d80af88555697af7bc8` supplies include-search-aware source
keys, verified against real compiled runtime changes and rechecked through Fo.

Exact source, pinned drivers, commands and receipts are retained under
`/var/tmp/fo-codex-controller-20261008/combined`; Darwin evidence is retained
under `/var/tmp/fo-codex-144-darwin-20261008` on faepmac1. FPM bootstrap defects have tested owning-repository repairs and public reports
[fpm#1334](https://github.com/fortran-lang/fpm/issues/1334) and
[fpm#1335](https://github.com/fortran-lang/fpm/issues/1335). Linux and Darwin
warm runtime checks pass without clearing caches; upstream scope review
precedes a PR. Fo's native build path remains the delivery authority.

Fx #57 remains open. Its two historical allocator aborts do not establish the
first invalid write or a recurrent reproducer. The audit is retained at
`/var/tmp/fx-heap-audit-20261008/AUDIT.txt`; a bounded warm Valgrind build is the
next discriminating check if the failure recurs. Do not infer repair from a retry.

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
