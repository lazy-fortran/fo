# Fo delivery goals

Updated 2026-10-07. This plan orders delivery; issues define observable success.
[Goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md) applies to
every task. Make architectural decisions as soon as required and as late as
possible. Agents may simplify, consolidate or replace internal mechanisms while
preserving supported behavior and independently demonstrated correctness.

## Current goal

Fo `1d47f93` is pushed to `main`: explicit-prefix release app installation is
native through shared CLI/MCP behavior. Its exact committed candidate passed
the focused two-app case 1/1, including output, example exclusion, preserved
prefix files, failed builds and nonregular destination refusal. A separate
public CLI install/run check passed with FPM absent from `PATH`; public MCP
install/run passed too. Fo #204's direct parity is now verified against an FPM
binary built from pinned source `90bb83a70e9bcf04d941fb43cca014ae1c0fc5ea`
(binary SHA256 `1160d794d3e45f36674802449f34a38cf5970d660ab06833edab11539a638483`).
Both release-profile fixture installs produced exactly two matching mode-0755
executables, excluded the example, and ran with identical expected output.
Fo `42c2531` rejects conflicting dependency sources and Git selectors. Its
parser case passed 1/1, and three public invalid manifests failed with
dependency-named causes. Resident session `3485903-1791379560-556554879`
passed the required case 1/1 on generation
`a25f97ba940e62c24b9ad59a57b563973e45a988a144a036cdb85c15b11bf48c`
with `local_gate_green=true`; the owner then stopped. An earlier run of the
same generation aborted in Fx lease release during parallel warm restoration.
Fx #57 remains open: focused cache, OpenMP, ASan and TSan stress did not
reproduce or localize that allocator fault. Revisit it promptly if it recurs.
Fo #205's JSON-RPC cleanup candidate `b523484` removes duplicate MCP response
wrappers. Exact driver SHA256
`7b08451783898b510e090253d0a2cfb0e04dcb18efae37c558ae6f91c6064f7a`
passed the public MCP request structure case 1/1 and resident session
`3510542-1791380001-608643440` passed its required case 1/1 on generation
`4313ebbf80feeb7bc7a545d9b88a1f219e0fa08c1c6214227d6a7d56cf662296`
with `local_gate_green=true`. Further #205 reduction remains open.
Fo #132's existing cold and warm dispatcher routes passed an added public
negative check: a stale private case executable cannot replace the shared
dispatcher. Candidate `3955210`, driver SHA256
`6e3f3c38bd475a889ab5a3dbbc2225aecec7ad3f82dfb66e347cd0ad891d8dc8`,
passed the direct case 1/1 and resident session `3629199-1791381968-197556150`
passed its required case 1/1 on generation
`727dd1fd06319203897695b79fd75642478e70ae939cb465bcfea6cdf6c23d89`
with `local_gate_green=true`. The initial red fixture expected the old result
after changing its source; correcting that oracle removed the false failure.
Fo #119's CTest result parser now preserves complete logical lines and
deferred-length names, and reports malformed records explicitly rather than
returning partial JSON. The public fixture retained 402 results, including two
names over 1,200 characters, and rejected a malformed record. Exact candidate
driver SHA256 `8ae5d8c316aad8934cb13ee00e0a702eb96de643eece54d1d5f7e4c3449d16aa`
passed the direct case 1/1 and resident session `3658970-1791382533-920280692`
passed its required case 1/1 on generation
`8e8245fa155c7ad1e369f5274f20d42613008e017c812d18d6bab0885f33d431`
with `local_gate_green=true`. Fo #119 stays open for distinct `fo check` and
Gremlin result surfaces that still have fixed name limits.
Fo `309a883` advances those surfaces: check JSON is growable, FPM manifest
test names and Gremlin request/runner/coverage paths retain supported long
names, and oversized request/manifest input fails explicitly. Heap-backed
name buffers avoid the 8 MiB stack crashes exposed by the new oracles. Exact
candidate SHA256 `4064578434b0e8740d71688bd167e7a36333cec7d45abeb3abac697e35cab806`
passed the public 185-character check name in compact and full JSON, focused
tests, and resident session `3884540-1791387062-304791062` gate 6/6 on
generation `8832acb1e69dc70200fb1e756cb3e3859de23447eabb7973cc548c1476ca2205`
with `local_gate_green=true`. #119 remains open for its broader result-status,
Unicode, pagination and malformed-input acceptance outside these name paths.
Fo #151's exact-driver contract now also has a metadata-only commit receipt
oracle. It locates the same PASS completion and log after an empty commit on an
unchanged generation. Exact current-source driver SHA256
`d7d033fc5037a3d12c3b151f4653b1b0b24854613cf22e9e32004a0dde7ad425`
passed the direct case 1/1 and resident session `3694000-1791383114-832543212`
passed its required case 1/1 on generation
`6e5d7ae3e2e08cbcc78ed2fb84feac292025743de716cc1e13c318be4c05cedd`
with `local_gate_green=true`. The first red assertion compared different source
generations; the corrected oracle compares the exact pre-commit completion.
The installed Fo CLI is now the verified combined #155/#119/#183 release binary
SHA256 `fe262b955c131c5c07d4fa9a62bd0cb5a31a2817c1f89e63b241d799294b490d`.
The exact integrated main candidate passed the terminal-receipt,
requirement-recovery and summary cases 3/3 before atomic installation.
Existing MCP server processes retain their prior image until client reconnect.
Fo #155 now skips periodic cache retirement/compaction while a Gremlin owner is
quiescent. Candidate SHA256
`9b98635dd592060e586517b749edf32b8af344c1794a2b40c17648e72c05ac8b`
passed the full public MCP fixture: a populated private cache and receipts
stayed unchanged for 11 seconds, status/stop remained responsive, and a declared
edit produced a new green generation. Resident session
`3791663-1791385285-461712331` passed required `test_gremlin_supervisor`
1/1 on generation
`dd1aa807ea061c53198d4812af7a9ecd9dbbbff6fdd1ac3320a99893d2db8d85`
with `local_gate_green=true`. The initial 30-second case cap was too short;
the 300-second retry passed. Cross-lane admission remains open.
The bounded warm-lane measurement now supports capacity two on this host for
the measured eight-case fixture with aggregate `FO_JOBS=2`: one lane took
23.49 seconds, first PASS 3.40 seconds, peak descendant RSS 100.8 MiB and
process-tree writes 307.1 MiB; two lanes took 13.00 seconds, first PASS
3.81 seconds, summed lane peak RSS at most 179.7 MiB and process-tree writes
237.4 MiB. A second timing pair was 23.49/12.71 seconds. Shared-cgroup
writeback counters were not attributable to these lanes; process-tree writes
include runner/cache output. Capacity-two admission remains implementation
work, so the one-heavy-lane host limit still applies.
Fo `3c183da` repairs the durable terminal-receipt recovery seam in both gate
selection and readiness. The exact pre-merge candidate SHA256
`db4bf5ca2839ddd9b069fe59ea6ac26391fefe12ce1e5a80cfa234afa9bea448`
passed the public terminal-receipt and requirement-recovery cases 2/2; its
resident session `3947508-1791388344-967411682` passed 2/2 on generation
`360bf77d034ef07d55a0bf2c7343bef9436e97002085841ff8676f70cbde9a69`
with `local_gate_green=true`. The controller's temporary combined candidate
SHA256 `2e0184d10d11a772be5793b4cedbf098b7ad26f4123d64e213052eb1439c5aea`
passed the two recovery cases plus the #119 summary case 3/3 before main
integration. #183 stays open for remaining crash-seam and stale-generation
injection acceptance.
Fo `5770cf7` routes test-harness scratch through the task's `TMPDIR`, with
`/var/tmp` fallback and registered cleanup. Its direct private-TMPDIR oracle
passed 1/1; resident session `3997205-1791389278-854770813` passed its
required case 1/1 on generation
`42df2f3809a4c96c96617ccf91317a371f6fe5829939fa4aba8f7dd69589daaf`
with `local_gate_green=true`. The exact combined main candidate passed the
same case 1/1 before promotion. Task worktrees and scratch were removed.

Keep one useful resident Fo Gremlin lane running and develop through it.
Fo `190d3e8` restores a single expected `.smod` output from the shared action
cache in fresh execution views and bypasses sources with multiple outputs. Its
independent fixture counted zero source compiler calls in the second view,
checked restored interfaces and executable behavior, and covered include
invalidation and the multi-output bypass. Its pinned candidate driver SHA256
`09d0e252eca6875bb3c3ffb435be1fbc04f7eeb0674421cf57198b4d5742b885`
passed the focused Gremlin gate 1/1. FFC consumer commit `8d82881` then passed
the conformance smoke and full FortFront corpus cases 2/2 in generation
`2b905e37f9470e67979828e754fad6019a54ac54bb3db3a508b70710c8b47645`.
Fo `58586b4` keeps an explicit `--target ... --random 0` gate focused after a
source edit. Its public edit/rebuild fixture failed on the pre-fix driver and
passed on candidate SHA256
`5d6ba22ff566db1a4a24c643aafcd5045f7153a9d951d9ab50707469643a53d7`.
The same candidate passed a resident 1/1 gate with zero failures in session
`3131773-1791372424-658412454`, generation
`022ff62a21dacbe16a0ba25a6f7d9921d68add0fd7498edabca0b722f3b38853`.
Continue bounded FFC discovery; full Fo and FFC inventories remain open.

The 2026-10-07 housekeeping candidate scopes child temporary files to private
execution views, reaps abandoned scratch on owner replacement, removes probe
logs, uses `/var/tmp` by default, and removes owned read-only scratch trees.
Its exact candidate driver SHA256 is
`2eda4f4ac3d7a04f550d43d38123f5fe8937f9a235a0b306a0d2f822eb834254`.
The focused execution-view, session-state, input-inventory and utility cases
passed 4/4; the v5 provenance oracle passed separately. Fo `2be13db` gives
each manifest materialization its own durable Fx root, releases it after a
safe explicit generation prune, and retries interrupted release. Fo `6dc64ad`
adds materialization-specific state, atomic register-and-lease admission, and
bounded inactive generation pruning on owner handoff and stop. Its exact
two-case resident gate passed 2/2 with zero current failures on generation
`82d90b21d391ed885856e59c46532416a7dba23ce6d426edbbfcf80dec7082af`.
Old ambiguous shared roots remain retained until ownership can be established;
the new namespace bounds prune victims at eight per pass but still scans all
sidecars. Old-root migration and automatic cold cache retirement are next.
Fo `df2a456` now roots each new generation as one canonical Fx tree. Its
focused manifest oracle passed 1/1: nine child blobs used one durable row and
a 240-byte isolated lease snapshot; GC preserved the children until release
and then reclaimed them. Existing global roots still occupy 8,260,961 bytes
and 107,636 metadata rows, so exact-group migration is required to remove
their write amplification without discarding live worktree/version ownership.
Fo `3d3a655` removes dead APIs, aliases and historical docs, migrates cache
consumers off v1, and reaps adopted Gremlin children while idle. The combined
current-Fx focused gate passed `test_cache`, `test_backend`, `test_mcp_system`,
`test_gremlin_supervisor`, and `test_async_adopted_reap` (5/5); the installed
release binary SHA256
`b13261e72f425a73e47171beb3496f59be3997d52067b3d040e3ba5e779e8933`
passed a separate 2/2 cache/adopted-child check. The initial gate failure came
from Fo's stale generated Fx checkout at `22a6530`; `fo update` fetched current
Fx `c75ed04` before the passing gate.
Fx `e236bd0` can now replace an unchanged inactive old blob-root group with a
verified tree under an exact metadata compare-and-replace; its focused lease,
GC and compaction gate passed 3/3. Fo invokes bounded Fx action retirement and
one-group root compaction at owner start, during idle periods and on stop.
Against Fx `e236bd0`, Fo's `test_cache`, `test_gremlin_manifest` and
`test_gremlin_supervisor` passed 3/3. The global old-root population still
needs a live owner pass and measured post-migration snapshot.
The Fo release SHA256 for that compaction gate was
`8709e137513175f52aaebe87e5456d6d45261c7f2dcb2bb9d220440d139edf3e`.
Public resident session `533484-1791356967-930118364` built generation
`386deb0f33dadf48c6d185b43523d594ff270c06216d77ac464d749435e71719`,
passed `test_cache` with `local_gate_green=true`, and reached quiescence.
During that run the global cache's compacted old Fo root groups fell from 178
to 175; the owner then stopped before another heavy lane. The inactive v1
store (2.7 GB and about 102,000 files) was removed after its old resident
owner stopped and an open-file check found no active readers.
Fo `b802b0c` now captures declared symlink dependency roots after checking the
opened directory against the inventory's device/inode; descendant paths remain
no-follow. The focused symlink inventory oracle passed 1/1. Exact release
driver SHA256
`81c096068873c99e0211c1754ab2a122092c0df6817c8b1c4f325b669aa51105`
passed an isolated FFC consumer gate 1/1 on generation
`1c5e179f22bf2142262686301dea494430ee1800eebbeb04bb45d5a192eea15b`,
including the FortFront file whose capture had failed previously. The owner
stopped. Full FFC coverage remains open.
Fo `c856ff0` now retires inactive execution views after publishing a stopped
session's durable terminal snapshot, while preserving live views, logs,
receipts, pinned drivers and captured generations. Its exact candidate gate
passed `test_gremlin_reproduce_logs`, `test_gremlin_execution_view` and
`test_gremlin_state` (3/3) on generation
`a43957c5f6e5eed29461f35e14dd36bbd221f0553fa4ff09d3308c7cc9c51402`.
Nested CLI checks used that candidate, including a normal stop and replay.
The installed Fo release SHA256 is
`241f780ddfac7240b0f97feb706b73f46b8c7226adb8ee811c27b16290227c37`;
it includes Fx `fb726d6` with orphan snapshot temp recovery. An exact
inactivity check removed 166 stopped-session execution views (1.86 GiB) and
134 unregistered older generation bundles (2.4 GB), preserving active and
registered artifacts. Fo `3b6c19b` now retains concrete Gremlin discovery
errors in terminal status and launcher output. Its exact candidate passed the
new nested public CLI oracle through Fo Gremlin (1/1, generation
`74e4d8e3a4f0fe7ddb30b248dc1da30baf771964d9771f194d338b1acb9e98c8`).
The original FortFront consumer exposed a source scanner defect: a test
source defining a module followed by a program was excluded from eligible
inventory. Fo `778faef` records the program in that case while preserving
synthetic external-procedure wrappers as link objects. The exact candidate
passed its direct public CLI regression and resident Fo Gremlin gate 2/2
(session `983165-1791365594-530815523`, generation `3449c2dbed36564b42cd62f8273e300692d527c2b5e6a39ae388ef46f0565086`).
The FFC consumer built under this driver after replacing a dead FortFront
facade import. The final FortFront `8eced2d` consumer gate passed 3/3 in FFC
generation `f6e57e6851e4441e1e665f34a3ba2866014ef53f6ac02388917fe2946e97a50f`.
Full FFC coverage remains open.
Fo `4ded437` removed the duplicate Gremlin `--failure`/`fail_on_failure`
wait API; callers use `--until failure` or `wait_until=failure` and read the
JSON result. Direct supervisor/readiness checks passed 119/119 and 49/49;
the exact candidate passed its resident 2/2 gate in session
`1104274-1791367525-301056715`, generation
`46691421f86abe14a3e36084e6cd6427fa32a691cd9e704646538c4eceb97e88`.
Fo `22f6dd3` removed Gremlin's forced `FO_JOBS=1` from candidate builds,
selected tests and reproduction. In a fresh public Gremlin fixture with
`FO_JOBS=2`, independent compiler actions overlapped, the dependent source built,
and the required test
passed in session `2886283-1791306950-470690383` with the local gate green.
Fo `f551f5a` protects dirty generated Git dependency checkouts during
`fo update`; its public CLI fixture passed 18 checks and the current Fo
checkout's dirty Fx source was preserved. Fo `4a4349a` synchronizes the
compiler capability probe; its delayed eight-thread oracle passed.
Fx `918fb94` repaired the same-key restore race: a mixed-flag oracle failed
6/640 before and passed 640/640 after the fix. Fo `43dcd04` now restores warm
source actions in parallel. Fo `78e3c5c` allows independent source hashes to
overlap, `3e2dad1` avoids syncs in temporary execution-view copies, and
`d4a2e82` avoids syncs in temporary manifest capture payloads. Fx `f12fc86`
buffers lease snapshot writes; its large-fixture comparison fell from 483,226
snapshot write calls to 968 with the same published bytes and passing lease
behavior. Fx `92a59b8` indexes lease rows once; the 16,272-row focused warm
fixture fell from 5.69 s to 2.92 s with lease behavior passing. Fx `165f9fe`
adds an explicit ephemeral materialization API for rebuildable outputs, and
Fo `f6eb7ac` uses it only for manifest generation bundles. The FX materialize
oracle passed 70/70, and Fo's combined manifest oracle rebuilt a deleted
bundle from the durable manifest with matching bytes and mode. The combined
Fo build against Fx `165f9fe` passed focused manifest, execution-view, copy,
stat, update and backend checks. Fx `22a6530` now caches validated immutable
lease snapshots across same-process actions. Its focused lease gate passed
normally and with OpenMP, including concurrent cold roots, child-process
replacement, collection and corrupt-snapshot rejection; warm fixture runtime
fell from 3.67 s to 2.08 s before the added concurrency assertions. The
combined Fo build against Fx `22a6530` passed the affected manifest,
execution-view, stat, update and backend checks. Driver SHA256
`3ce5ca366981ba94e2de3b8a3adb308dc56052e6e7e002138c5549d9432c31a5`
is installed. A fresh public Gremlin fixture with `FO_JOBS=2` observed compiler
overlap and passed its required case (session `3381163-1791313181-144997158`,
generation `a11d55f2`, local gate green). The preceding FFC six-case recheck
passed 6/6 with zero failures and `local_gate_green=true` in session
`3046333-1791310754-498154325`, generation `cb0ca871`, on lane
`ffc-final-a0e2-20261006`; the owner was then stopped cooperatively.
Full FFC coverage remains open (503/509 unknown cases). The final installed
driver passed a separate 1/1 FFC consumer gate on generation `30dd180c`
(session `3382726-1791313223-162516488`), then the owner stopped
cooperatively. With the cache, the warm FFC build reached 291/516 actions at
about 53 s; the preceding driver reached 86/516 at a similar point. This is
measured improvement on the actual consumer, not a full-suite speed claim.
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
| Working resident development and exact driver behavior | #200 |
| Relevant edits and quiet completion | #155 |
| Reliable process/session lifetimes, CLI/MCP and Darwin support | #139, #142 |
| Lossless results, durable receipts and exact recovery | #119, #140, #183 |
| Trustworthy inputs and affected-first finite testing | #175, #189, #138 |
| Correct shared dispatcher routing | #131 |
| Fresh dependencies and test-only project execution | #135, #188 |
| Complete safe artifacts and independent writable execution | #144, #166, #170 |
| Reusable stable generations with less storage/I/O | #165, #168 |
| Less duplicated build, source, protocol and scheduling behavior | #149, #150, #167, #185, #186 |
| Smaller useful native tests and no Fo-owned script runtime | #161, #163, #184 |
| Safe cleanup, useful progress and correct timeout attribution | #129, #130, #134 |
| Native FPM manifests and dependency closures | #201, #202, #203 |
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
