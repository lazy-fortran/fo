# Fo delivery goals

Updated 2026-10-04. This plan orders delivery; issues define observable success.
[Goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md) applies to
every task. Make architectural decisions as soon as required and as late as
possible. Agents may simplify, consolidate or replace internal mechanisms while
preserving supported behavior and independently demonstrated correctness.

## Current goal

Keep one useful resident Fo Gremlin lane running and develop through it.
[#200](https://github.com/lazy-fortran/fo/issues/200) owns current source/driver,
session, gate and failure evidence. The resident lane built and passed
its four required cases; the controller published that locally green increment.
Background coverage found async-start, driver-input and inherited test-directory
failures and cleared readiness. Those repairs are pushed at `318ecea`; driver
`6e54f805` passes six focused reproducers and two existing process/session cleanup
checks. Resident session `4014645-1791139032-043537972` is now running on the
controller checkout with eight explicit cases including all four previous
failures; affected/history selection currently widens its required gate to 24.
Its new generation built and is testing; earlier receipts remain.
This is actual Fo
dogfooding, not a claim of full verification or final speed.

The initial utility/input-inventory/execution-view gate and contained-launch
reproducer are independently exercised. Fx materialization/warm-hit repairs are
locally verified. Remaining fresh-view I/O is a demonstrated optimization target,
not a reason to restart an upfront architecture program.

## Delivery order

1. Sustain resident Fo dogfooding and repair concrete workflow blockers.
2. Substantially reduce maintained code and documentation through
   [#205](https://github.com/lazy-fortran/fo/issues/205), preserving features and
   correctness. Cleanup accompanies useful increments; completion of every
   architecture issue is not a prerequisite for the consumer pilot.
3. Make FFC's FPM project and public shared dispatcher work well in Gremlin.
   Only reproduced consumer blockers gate this handoff.
4. Repair up to three confirmed current FFC failures through that loop.
5. Finish standalone FPM compatibility on unchanged supported manifests.
6. Finish standalone CMake/CTest compatibility for the maintained ITpPlasma
   project/profile subset.

Implementation, focused verification and locally gated publication are active.
Existing controller/host rules govern resource admission and ownership. Do not
wait for GitHub CI while useful implementation remains available. CI is
independent post-submit evidence; Gremlin reports facts and does not govern Git.

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
| Working resident development and exact driver behavior | #200, #151 |
| Relevant edits and quiet completion | #148, #155 |
| Reliable process/session lifetimes, CLI/MCP and Darwin support | #139, #142, #172 |
| Lossless results, durable receipts and exact recovery | #119, #140, #183 |
| Trustworthy inputs and affected-first finite testing | #175, #189, #138 |
| Correct shared dispatcher routing | #131, #132 |
| Fresh dependencies and test-only project execution | #135, #188 |
| Complete safe artifacts and independent writable execution | #144, #166, #170 |
| Reusable stable generations with less storage/I/O | #165, #168 |
| Less duplicated build, source, protocol and scheduling behavior | #149, #150, #167, #185, #186 |
| Smaller useful native tests and no Fo-owned script runtime | #161, #163, #184 |
| Safe cleanup, useful progress and correct timeout attribution | #129, #130, #134 |
| Native FPM manifests, dependency closures and install | #201, #202, #203, #204 |
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
[the pre-revision plan](https://github.com/lazy-fortran/fo/blob/f74daf7/PLAN.md).
They explain earlier decisions; active goals and current accepted contracts
govern new work. Keep this plan short and update the owning issue when work moves.
