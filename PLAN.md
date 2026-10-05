# Fo delivery goals

Updated 2026-10-05. This plan orders delivery; issues define observable success.
[Goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md) applies to
every task. Make architectural decisions as soon as required and as late as
possible. Agents may simplify, consolidate or replace internal mechanisms while
preserving supported behavior and independently demonstrated correctness.

## Current goal

Keep one useful resident Fo Gremlin lane running and develop through it.
[#200](https://github.com/lazy-fortran/fo/issues/200) owns current source/driver,
session, gate and failure evidence. Earlier async-start, driver-input and
inherited test-directory repairs remain pushed at `318ecea`; driver `6e54f805`
passes six focused reproducers and two existing process/session cleanup checks.

The prior 24-case session `4014645-1791139032-043537972` ended with an
`INFRA_ERROR` when a nested campaign-history owner start returned ESTALE. Its
trace identified a stable root/member/owner process identity whose parent and
session fields changed after reparenting. Member-record comparison now validates
those fields while tolerating that change for the same stable identities. The
readiness report maps `capture_pending` to `starting` or `testing` according to
the active generation. The campaign seed oracle preserves the mandatory first
case and shuffles the remaining order. The killed-writer test now blocks after a
one-byte temporary write, so the parent kills it before atomic publication.

On base `1c45a55`, patch SHA256 `12fd995d`, generation
`8f568aa75a0af5f73aace5d8fd9ef131771f35f6b4a2e8c50671cb23d758d96f` passed the
three-case required local gate: `test_stat_memo`,
`test_gremlin_campaign_history`, and `test_gremlin_capture_pending`. Candidate
driver SHA256 `a643b884` also passed `fo test test_gremlin_capture_pending` with
both `FO` and `FO_BIN` set to that candidate.

On clean commit `f310e24`, generation
`776cb130c3ea809f5a5a0b84c5c172fbf969cdfa0f6d6cbeb93a534b90d7513c` was built
and tested through candidate driver SHA256
`a643b88469790cf5bb7d0f834a44a593f7e6a470b4ce8b4d5118eaca35f38218`. The
first `--random 24` session, `582742-1791155656-236633237`, timed out
`test_gremlin_campaign_history` at the default wall cap (exit 124); its case log
SHA256 is `c4edd7bb89dc27ca43135fe122733b71db4946e7cbedef18e14b4ff69472d493`.
The same generation passed that case with `FO_TEST_WALL_TIMEOUT=300`; the 10 s
CPU budget was unchanged. Session `643490-1791156457-310338900` then recorded 28
distinct PASS case receipts, including the required gate case and
`test_gremlin_campaign_history`, with zero failures. The gate reached 1/1 and
ordinary coverage reached 28/103. The lane was stopped after the sample and
required gate; full-suite verification, persistent idle residency and final
speed remain unverified.

On clean `main` commit `ef5af56e2c5b7e1517950235d045ffc3dced9397`, the same
driver built generation `776cb130c3ea809f5a5a0b84c5c172fbf969cdfa0f6d6cbeb93a534b90d7513c`.
Session `763987-1791158219-395285111` (`resident-current-5case-20261005`)
passed the exact five-case required gate: `test_stat_memo`,
`test_gremlin_input_inventory`, `test_gremlin_execution_view`,
`test_gremlin_campaign_history`, and `test_gremlin_capture_pending`. The
controller set `FO_TEST_WALL_TIMEOUT=300` and a 300-second case wall bound; the
campaign-history case passed in 106.20 seconds. The gate reached 5/5 with zero
failures and ordinary coverage 5/103 before the owner was stopped. Build and
case logs remain under
`/var/tmp/fo-resident-development-20261005/fo/gremlin/projects/358e93b5c75070e9/4d74cc6b29c64e08/logs/`;
the build log SHA256 is `17fb0cbcb88cc5433435a38967c1ed1849f65e017da376de9716762f9c329d6c`.
Full-suite verification and persistent idle residency remain unverified.

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
