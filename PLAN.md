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

Latest #200 milestone: Fo commit `090058a89bf4af5e9be32ab63443176a5994b683` is
pushed to `main`, based on `9c213511a16becf7ea91a6ddd21e33aeb4006e23`; the
source patch SHA256 is
`a2a3ae408267e022a12bb9d2de7fe0ab890240363c1eb9a46822636722861629`. It makes
readiness read the exact session receipt journal, accepts `random_count=0`, and
quiesces once required gate targets pass. Candidate driver SHA256 is
`8cf2187c35ab5b9e8fea493084aa32aba77b8d54a7851bf4bf070fb9ad7f86b5`.

Clean-worktree Gremlin session `3403700-1791201746-795986148`, generation
`be82a6e9397c76b729ba22b0dba31be1985e99403b48b813f2d89dae2e181ce3`, passed
the 10-case required gate with zero failures. `test_gremlin_campaign_history`
passed in 119.63 s with `FO_TEST_WALL_TIMEOUT=300`. The owner then reached
quiescence with `local_gate_green=true`; a follow-up status 10 seconds later
showed the same live session/generation, 10/10 gate receipts, and 8/103 ordinary
PASS receipts (95 ordinary cases remained). The public candidate passed
`test_gremlin_supervisor` and the full CLI/MCP Gremlin behavior test. That test
confirmed MCP/CLI attachment, gate-only quiescence with ordinary cases left,
and wakeup into a new green generation after an edit. Full ordinary coverage
is not a #200 milestone gate. The exact candidate also passed
`test_gremlin_public_readiness` in 78.85 s with the wall-time override unset;
the oracle exercised timeout attribution, stale-token invalidation, owner
restart, failure and repair. The preceding `9c21351` milestone also passed the
resident Mac watch gate, including `test_gremlin_watch`.

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

Fo main commit `b3a387f4990d724fc6ca67041c62d0dcd3f2563e` bridges app outputs
from the exact active profile into each private test execution view, including
the fresh-build and cached-test paths. On base `d31db5c`, frozen patch SHA256
`fe7899422999465159a9e4c456d18825843c3e436b770f84e75605ed00b7a29c` passed
`test_gremlin_execution_view` through candidate driver SHA256
`e83f708b1874fe7b403e3f6cd23946920026241203ac8a22f838e90123ffd711`. Gremlin
generation `f41563173efcb27a7aa94ed67843c78b22e07150e57c69cbb70c506cb1c1b87c`
passed the two-case gate (`test_gremlin_execution_view`,
`test_gremlin_reproduce_logs`) and sampled `test_link_gc_sections`; its
`local_gate_green` receipt is in session `1209314-1791168456-528134238`.
Later exploratory cases `test_gremlin_nested_capture` and `test_gremlin_watch`
also failed on unmodified main, so they are recorded as existing suite issues.

Fo #186 has an initial source-policy slice on `main` at
`485226135d8467a366342e2afd1f752b6997e934`, based on `0ea2244`. The native
scanner and Fo watch filter now share the `.f90`, `.F90`, `.f`, and `.F` suffix
predicate. On the combined candidate, generation
`879bfdb00dc63f10d4dd2258608bd26756452a47481c4fc96e988c5e7b6ebbc4` built and
`test_scan` passed (gate 1/1, zero failures); the test checks discovery of all
four suffixes. Logs are under
`/var/tmp/fo-resident-development-20261005/fo/gremlin/projects/295419f1b143ec1c/8ba60bb1f57c3d77/logs/`;
build log SHA256 `f48342a3c768c10755c75242e3cb3b2bc007264a6df556df608e6198a6eecd53`,
case log SHA256 `23d4eec3e1d23dcac9b96589cbb34255060e42396fccb8af1b774bd09373a17e`.
Broader inventory, exclusion and impact alignment remains open. A branch-only
`.f` assertion in `test_gremlin_watch` timed out at the 300-second case limit
without returning an assertion result; that test edit was excluded from main.

The initial utility/input-inventory/execution-view gate and contained-launch
reproducer are independently exercised. Fx materialization/warm-hit repairs are
locally verified. Remaining fresh-view I/O is a demonstrated optimization target,
not a reason to restart an upfront architecture program.

2026-10-05 FFC capture exposed two provenance gaps. On current Fo base
`17db6d81e0cf86b2325da159aa62964755eb92e6`, patch SHA256
`00ba377268b634e14eb7829e2a8c179acbb40458fdf719cff25387ef34fb231e` adds
`LIBRARY_PATH` to the captured execution environment and bumps generation
identity to v4 so the Git base commit and uncommitted patch digest affect the
generation key. The typed manifest test verifies dependency-byte changes,
metadata-only commits, dirty edits, isolated linker-path changes, and compiler
path provenance. Candidate driver SHA256
`4db8ba18608c0375506cc5e10c3d14fa98fcd28e812b44f508dfc4e7c0547fa2` passed
`test_gremlin_context_provenance` in session
`3999079-1791214209-696033962`, generation
`9356d99c1b1eb2f74fbd2528b19093e1888107da30bf6a5ea0cd12a18b15b2b4`; case log
SHA256 `35e5ad411fbb1ab5a9e5b6ddcfaecaa93930c9356f8c42cd4548ad9a18739ced`.
This pass used the updated Fo base that arrived during the preceding run, so it
also confirms that the new base commit produces a distinct generation.

The same candidate repaired Fx's lease row cap and captured FFC generation
`3825c278a877817415ca1ed82a49ff324439977af45ed8fb35462235edb8dc74` in session
`4044827-1791215266-014327427`; the FFC build is underway. A prior Fo/Fortnum
resident had stopped at publication with `cannot protect generation objects
during publication`, which the Fx capacity reproducer now covers.

The next FFC corpus case exposed two Fo defects: fixture declarations could not
name a dependency root or directory, and `reproduce` rejected the
`timeout_seconds` field already honored by its handler. On base
`d51aa37a03b32f463ff821661da948e8847cb865`, source/test patch SHA256
`959ad9f9d6ed4b8c791902c753651ac5c2906c5dc1452ea06792e3394cb12eb4` adds
`root` and `kind` to `[[extra.fo.inputs]]`, recursively inventories declared
directories, materializes their private execution-view paths, and accepts the
reproduction timeout. The required four-case Gremlin gate passed in session
`1129433-1791234376-597691208` on generation
`93c33bfcaf2b82024ee2b03938b2b625bed0d9d0ab7f71d35f69e9a66fd6578d`:
`test_gremlin_input_inventory`, `test_gremlin_execution_view`,
`test_gremlin_reproduce_logs`, and `test_gremlin_supervisor` (4/4, zero
failures; local gate green, full coverage incomplete). Candidate driver SHA256
`c3e41b5bc5f6bfb5550832269092b69a53dedf47c62e21fa8fc1984153fc8b5d` also
passed a public `gremlin reproduce --timeout-seconds 300` invocation for
`test_gremlin_supervisor`. The read-only candidate copy is at
`/var/tmp/fo-gremlin-driver-candidates/c3e41b5bc5f6bfb5550832269092b69a53dedf47c62e21fa8fc1984153fc8b5d/fo`.
The Fo#175 consumer repair was pushed to `main` in commit `b88cc36`; FFC's
declared FortFront corpus fixtures are being checked on that candidate.

2026-10-06 Fo dogfood follow-up: the Gremlin candidate built against Fx
`4f016abab099a92b3c1eb57aa3bb8da43481f0a6` and passed the five focused gates
`test_gremlin_generation`, `test_gremlin_manifest`,
`test_gremlin_input_inventory`, `test_gremlin_test_impact`, and
`test_backend_gfortran` (5/5, zero failures). The gate-only lane reached
quiescence with `local_gate_green=true` before it was stopped. Candidate driver
SHA256 is
`9dc4a415a8f4ef497867553447857503c1b83fdafa5429069c51f5bbc7ab57e8`; session
`4137039-1791270183-328637191`, generation
`8519015416f565b4a7cb2356536c82d285c08fb32a8ce1b2135c829c6fcfdf25`. This
increment completes path mapping for materialized bundle inputs, keeps affected
test selection exact across generations, and fixes execution identity so Git
provenance remains recorded in manifests without invalidating execution reuse
when only commit/patch metadata changes. The identity schema is now v5.

2026-10-06 bounded Gremlin output change, based on Fo main
`a973a939f6daa5581cf6e0e246db1ab49e571574`: summary detail is now the CLI/MCP
default for status and wait; full status remains opt-in, with event/failure
pages available separately. Before the change, a completed-lane MCP status
reply was 19,443 text characters and CLI status was 22,239. The focused CLI
oracle now asserts that default status is smaller than full detail, stays within
4 KiB, and omits event pages and cursors. MCP assertions cover summary defaults
and retention of the latest failure receipt.

The controller fixed a summary lookup defect found during the first run: a
read-only session held the terminal journal directory, while current failures
live in the lane campaign journal. Summary now resolves the lane state directory
and filters receipts to the exact session and generation. The regression fixture
places an older session's failure on the same generation and verifies that the
current session's receipt is returned. Frozen source/test patch SHA256 on base
`14d052367a25a67df157ebfda43f4d1d9fa5795b` is
`bc80999f0b5988a61705dcb031afc50869c03167812e8c9a28b58dc8a2c73495`.

Pinned bootstrap driver SHA256 `9dc4a415a8f4ef497867553447857503c1b83fdafa5429069c51f5bbc7ab57e8`
built generation `9cf1ecc76cbc3a39449a30076c154f5bfc0255b8888dc6013aee573112b0aec3`.
Session `1521629-1791282052-162327391` passed `test_gremlin_summary_request`
(gate 1/1, zero failures; `local_gate_green=true` before the lane was stopped).
The exact candidate driver SHA256 is
`7c1dedd168e07e7eea7a29f4d4abe60492c7a4cdce1e71cc3c09e7d0bb760e81`; it passed
`test_default_gremlin_cli` with both `FO` and `FO_BIN` pinned to that image, and
`node test/test_mcp_gremlin.js <candidate>` passed the MCP status/wait, failure
receipt and paging checks. Commit `0d9ef7fa38b8f4b60f680a9b380696eb3a223fcf`
is pushed to Fo `main`. The installed Fo 0.3.2 binary at
`/home/ert/.local/bin/fo` has SHA256
`95222d18cff2b3dfe82dce41827ddcdae202465fe7680d0a9afca5db2a6c9280`. On the
same failed session, installed CLI summary output is 1,210 bytes versus 16,173
bytes for full detail, and retains the current latest failure receipt. Full
ordinary coverage is not claimed. The active MCP connector returned
`Transport closed` after its old workers were stopped; reconnect the Codex
session to launch the stdio server from the installed binary.

## Delivery order

The kin6d CMake consumer exposed a profile-isolation defect: public Debug flags
were written into global `CMAKE_Fortran_FLAGS` and persisted into later native
Release configuration. On base `2f2bf21` with scoped patch digest
`8cbfa18f45234d1f1a2b6782fd6be888a76bc3dcedaa04edbd408b0dcfd0d7e5`,
named profiles now select a configuration unless one is explicitly supplied,
and compiler flags are scoped to that configuration. The native bootstrap
candidate at `/var/tmp/fo-cmake-profile-driver-20261005/fo`, SHA256
`6bb8bb26cf7374d8206f506ac36e36009ae8b26e195357648771938c435c9d02`,
passes the public CLI regression and 73 backend assertions. Its independent
fixture preserves project macros, checks actual Debug bounds failures, retains
an explicit custom configuration, and executes the same-directory native
Release preset without inherited Debug flags. The prior pinned driver fails
this fixture. The original kin6d Debug gates pass 2/2 through the candidate;
subsequent native Release gates pass 5/5 with global flags still empty.
Native FPM was used for diagnostic bootstrap of this exact candidate after a
duplicate cold Fo build remained incomplete; no cold-build pass is claimed.
This repairs delegated profile behavior; native CMake input capture remains
the separately documented milestone.

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
