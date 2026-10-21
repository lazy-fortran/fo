# Fo delivery goals

Updated 2026-10-21; compile-cache qualification `291862c`, Fx `c8d892e`
(earlier consolidation qualification `d1b4a69`).
Issues define observable success and this plan orders delivery. Apply
[goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md), choose the
smallest complete change, and preserve supported behaviour and independent
failure detection. The workspace master plan, when present, owns delivery order.

## Destination

Fo is the simplest, fastest and most trustworthy Fortran build and test tool
for people and AI agents. Gremlin is its centre.

- **One model, any manifest.** `fpm.toml` and CMake/CTest projects enter one
  backend-neutral project model (targets, layers, cases, labels). Selection,
  planning, layering, impact, caching, receipts and Gremlin are implemented once.
  Only adapters and the `build(targets)`/`run(cases)` primitives differ.
- **Fine-grained, auto-discovered cases.** The case is the unit of selection,
  execution, verdict, timing and cache, down to individual cases inside a test
  executable (#217). FLAKY and SLOW are first-class verdicts, never disguised PASS.
- **Never a false green.** A result is reused only on an exact input-closure
  digest match (Go/Bazel rule), never because a build was cached. Unknown or
  unclassified inputs widen selection or report incompleteness.
- **Fast.** Interface-hash cutoff, so a body-only edit recompiles one file; one
  build per generation; parallel cases within shared admission (#216); a warm
  shared cache without whole-tree copies (#165).
- **Gremlin.** A resident lane that is always correct: exact generations,
  last-compilable retention, reproducers first, then affected, then finite
  background coverage, then write-free quiescence until a relevant edit. Live
  scoping by label, layer or name; cancellation of only owned work; many agents
  and worktrees attach without interfering.
- **AI-native and context-economical.** CLI and MCP are equal adapters over one
  service. Output is a one-line verdict first, deltas since the last observation,
  bounded failure excerpts with lossless paging, compact stable JSON, and typed
  waits that return the answer (#218).
- **Small and elegant.** No dead code, no parallel implementations, no silent
  capacity caps (#215). Every module earns its lines (#205).

The product boundary in `CLAUDE.md` still holds: no hermetic environments,
Nix/Spack replacement, CI/promotion engine or agent orchestrator.

## Self-refresh and install CI repair (2026-10-21)

- [V] The profile-switch reproducer replaces an unchanged app inode on main.
  Private app publication now retains identical executable copies and repairs
  damaged ones. Archive validation extracts once into owned scratch, retaining
  exact membership and per-object byte checks instead of one process per member.
- [V] Source `a2f22e3`, release driver SHA256
  `96c0b35c82bb1e64abe32d74181d3b9e5eb8ec3d5e719bf49febd1dd277e3ece`
  passes native atomic-archive, install, compiler-artifact, CMake-build and
  fixture-failure-evidence checks. Incorrect member bytes remain rejected.
- [V] GCC 12.2 rejects resetting a called host-associated procedure pointer.
  Copying and clearing the teardown hook before calling it compiles on the
  compiler floor and preserves once-only teardown. CI's install-help step now
  retains its pinned driver when HOME is isolated.
- [V] The GCC 12 release image also emitted `cmake --build ... --target -j 4`
  without the delegated target names after growing the route's string vector
  through array constructors. The vector now reserves the known target bound
  once and stores each name directly. The existing
  duplicate-module consumer is part of the focused CI gate.
- [V] [PR #220](https://github.com/lazy-fortran/fo/pull/220) owns the complete
  self-refresh CI and deployment qualification; focused passes alone do not
  establish a green main run. Evidence and installation receipts belong
  under `/home/ert/data/handoffs/20261021_cluster_triage/fofix2-logs`.
- [V] Both GCC 12.2 cluster images pass the same five affected programs;
  sCluster's cold self-refresh oracle passes in 314.95 seconds. Darwin passes
  the four repair-specific programs and both smoke consumers; its original
  installed image reproduces five unrelated relocation-oracle failures.
- [V] CI run `39553210344` confirms the inode repair but still fails explicit
  install: the 180-second child cap expires at 483/507 compiled sources.
  A two-CPU aCluster trace completes cold release installation in 316.77
  seconds (job `21816920`), without a hang. The controller classifies this as
  a command safety timeout: the oracle's cold build commands receive a
  900-second cap, with all behavioral assertions intact. CI `39557841818`
  then reached install but exhausted its separate 600-second parent cap;
  this one oracle now receives 1800 seconds within a 40-minute CI job.
- [H] Cold release-build cost reduction is follow-up work, outside this repair.
  PR #220 still requires green CI, merge-queue publication and deployment.
- [V] CI 39560225391 passes the complete Fortran gate, including cold
  self-refresh, then exposes the standalone C fixture link command's missing
  fo_sys.c dependency. The corrected command passes all seven native/filter
  variants locally. The cancellation fixture explicitly installs the strict
  inherited-group filter it asserts; ordinary native groups remain allowed.

## Lifecycle qualification (2026-10-21)

- [V] Synchronous completion sent SIGKILL but waited only for the leader.
  It now retains that exact leader unreaped until its private group stops;
  timeout cleanup also retains the group identity until signalling finishes.
  The backend oracle repeats named and unnamed background-child completion.
- [V] Gremlin previously deleted build scratch and maintained its cache after
  publishing stopped and releasing ownership. Both now precede publication.
- [V] Contained capture's owner could fail before stop when an empty inventory
  reached coverage_open, which requires members. Campaign selection now returns
  no debt for that empty inventory. The public oracle waits for inventory
  discovery: the pre-fix driver fails and the repaired driver settles idle.
- [V] Immutable local driver SHA256
  `f862da4869275382bc9c4bffe95f10d91899c024a0cf25243d32d2a2d4691bdc`
  passes six affected native programs with retries disabled. GCC 12.2's option
  table SELECT CASE compilation uses a typed copy with unchanged probes.
- [V] Cluster tracing reproduces a pre-existing parallel link exit 126 on the
  baseline: execve receives an object file as argv[0]. Per-worker compiler
  detection clears the shared executable path while another worker packs argv.
  Test workers now reuse the compiler identity resolved before their loop.
  The original cold/warm parallel regression repeats four fresh projects.
- [V] Cluster jobs 3300457/3300478 passed the initial focused four-program gate;
  the latter's FO_JOBS=16 suite reports 177 PASS / 7 FAIL, including the empty
  inventory failure repaired above. Job 3301176 compares the exact baseline
  and repaired driver, including six other failing controls. No full green is
  claimed. Evidence: `folife-logs` in the 20261021_cluster_triage handoff root.

## Native CMake Fortran repair (2026-10-21)

- [V] Source `8734651` retains CMake's per-source preprocessing choice from
  Ninja and uses `-I` for Fortran SYSTEM module directories. Compiler dialect
  detection now ignores misleading parent-directory names.
- [V] The three affected native CMake, compiler-dialect and dependency-freshness
  tests pass, including forced `iIFxVv` temporary paths and independent external
  module lookup. KIN6D `379f3f1` passes both original chart-solver cases through
  native Fo without the delegate override and through CTest. Its binary matches
  the recorded reference SHA256 `b19705b2…`.
- [V] Linux release SHA256
  `c2f25388041ef5b85c700696b372bc8eb11f3480e213e7ecec529d63d7bd3b2c`;
  Darwin `8ef6ffed177dda522b3f5cd9f4de9528e5c3150af642561bc4f23b0b39aa359c`
  passes the combined preprocessing/module smoke. Fx production source matches
  `c8d892e`/`53aa79d`; FortFront is `a34ada8`.
- [V] Full slow-inclusive suite, retries disabled: FO_JOBS=8 gives 183 PASS,
  1 FAIL in the backend child-cleanup oracle; FO_JOBS=16 gives 182 PASS, 2 FAIL
  in owner-release/contained-capture oracles. Their process code is unchanged;
  the original installed driver passes four focused lifecycle controls.
  The backend process-tree subset also fails in a standalone rerun. These runs
  are not full green qualification. Lifecycle causes remain unresolved
  under [#139](https://github.com/lazy-fortran/fo/issues/139).

## Compile-cache qualification (2026-10-20)

- Verified source `291862c` with immutable driver SHA256
  `16c66d34b6ba06cc42fd0548904e1276f2c1615157623a39c1d10e59b1f04fb0`:
  all 184 registered tests, including slow cases, pass with retries disabled.
  A bounded resident lane also reaches green quiescence and stops cleanly.
- Scan entries have a documented 1 GiB LRU budget; Gremlin has an 8 GiB soft
  budget for inactive execution views, with conservative ownership checks and
  no eviction locks. Protected generations/journals can leave budget debt.
  Fo-owned eviction reduced scan disk usage 3.6G to 830M and Gremlin 19G to
  8.9G; filesystem overhead is additional to regular-file content accounting.
  Fx `c8d892e` retires obsolete action lease rows on snapshot mutation.
- Native CMake reuses content-keyed compiler-visible scans and validated
  immutable archives. External-source debug cwd mapping and object-library
  link fragments preserve relocation equivalence. SIMPLE `fffedba`, Release,
  Python tools/interface off: 889 Fo artifacts match byte for byte between
  cold and relocated builds; both trees pass the same 126 fast CTest cases.
  Local samples: cold 238.56s, warm 21.35s, relocated warm 61.45s (zero
  compilations), one-file edit 9.26s (one compilation). Independent CMake
  build samples are 29.69s cold and 0.08s warm; these are advisory timings.
- The edit/reference measurements require the assertion-preserving Ninja
  dirty-rescan repair in [draft upstream PR #2859](https://github.com/ninja-build/ninja/pull/2859).
  Its 437 native tests and the original consumer command pass. No global
  Ninja installation changed. An earlier single Slurm audit measured warm
  21.34s to 13.95s, then exposed the external-source relocation defect fixed
  above; it is not evidence for the final relocation gate.
- State-loss detection compares held inode identities; coverage restart waits
  on observable progress within its existing parent deadline. Both formerly
  flaky cases pass the full gate. Retention finishes before quiescence is
  published, retaining the write-free quiescence contract.

## Focused infrastructure repairs (2026-10-17)

- FPM test-group labels and declared-input attributes retain their text on
  GCC 13 after array growth. The original native GCC 13.3 test reproduced
  five failures; the corrected test passes, including overlapping groups,
  shared-input deduplication and preservation of the slow label. The affected
  model and layering CLI tests also pass on GCC 16.1. This is focused
  qualification. The combined app and original consumer checks now pass on
  Darwin and Linux; the qualified image is deployed on both hosts below.

- Native named FPM tests now link standalone external and mixed
  module/external test helpers together with their USE dependencies. Unrelated
  pure module helpers remain outside the selected closure. The focused backend
  regression changes from three passes/two failures to five passes, including
  warm execution, changed-helper invalidation and undefined-helper rejection.
  The original FortGen and NEO-RT consumers pass with the combined release
  image. Darwin and Linux deployment and installed-consumer checks pass.

- Native FPM `build --tests --case` now builds only the selected test
  closures, without executing tests. An unrelated test's
  compiler error reproduces the old failure; the focused materialization CLI
  regression and one-target, random-zero Gremlin gate pass after the fix.
  Explicitly selecting the invalid case and unscoped slow-inclusive builds
  still fail. This is focused qualification, not a full-suite result.

- Combined main `a61b5fc7` is installed on faepmac1 at
  `/opt/homebrew/bin/fo` (SHA256 `b49a7ff61d427f68ee14fef6987195f7cb126b1236e43bdbb568113e35f295d5`)
  and mailuefterl at `/home/ert/.local/bin/fo`
  (SHA256 `6ffd7a5da229b5fb5b8d961391fb9036efed8928e469bd899d3aea143690d098`).
  Installed hashes match the qualified immutable candidates; version, selected
  test build and execution pass on both hosts. Existing resident pins remain.
  faepkub4's host key is verified, but SSH authentication fails, so its
  deployment remains unavailable.

- Native CI at `2e4d701` exposed the stale 240-second parent cap; CI
  inherits the existing 600-second manifest allowance. At `f484fe3`, the first
  oracle attempt fails after 488.68 seconds, then its default retry consumes
  the remaining 20-minute job budget before failure logs upload. The original
  assertion failure remains unknown. CI now runs this expensive oracle once
  with `--rerun --retries 0 --verbose` and retains its captured `.tmp` transcripts.
  Four native controls verify first-failure handling. Parent/child/job caps
  and every oracle command/assertion remain; the next native CI must expose
  the first failure or qualify the complete oracle and later checks.

## Development branch consolidation (2026-10-17)

- All 105 live remote development heads and 26 local heads are accounted for
  by audited ports, existing replacements and explicit history merges. The
  latest upstream main `fe9501e` is merged. Removed `fo work` and candidate-built
  self-runtime modes remain superseded by the immutable controller-driver
  contract. The old attached-cache warm-configure shortcut was rejected because
  it does not establish current native-model freshness; configure stays conservative.
- Native test `fo exec` builds the selected closure, including declared tests
  and external helpers, while poisoned unrelated applications/tests remain outside
  it. The raw executable-path guard is retained. `fo check` reports the actual
  selected test count, including zero.
- Campaign history uses compatible input/oracle identities and canonical lane
  journal/lifecycle paths; unknown external identities stay conservative. The
  real owner/session directory layout now has a regression oracle. Resuming an
  epoch retains its original seeded default gate instead of inventing another
  requirement after coverage completes. Explicit gates retain their selection.
- Coverage controls distinguish reproductions from campaign credit, reject a
  stale-generation receipt and restore the actual ledger saved while the final
  child was blocked. Durable receipt replay completes the epoch with exactly
  40 executions. Controlled failing reproductions explicitly use zero retries.
- Release build and the final 13-program native gate qualify source `d1b4a69`
  using immutable image SHA256
  `a48023afbb1116dda88e47376c80441b5b9597e2eb0067571c79231bb0ef7dcd`,
  GCC 16.1 on Darwin, FortFront `a34ada8` and Fx `d3f87b2`: **12 PASS,
  1 SKIP**, no FLAKY/FAIL/TIMEOUT. The skip is the Linux `/proc` journal
  durability process oracle; a focused Linux CI step is added, not locally
  qualified. Both workflow YAML files and all 19 shell steps pass static checks.
  Release/native builds have zero introduced warning locations; all 16 new
  fixture array-temporary warnings were removed and their final tests pass.
- Earlier evidence remains distinct: the initial image had an exec FLAKY result
  on HFS+ and a real campaign path failure; a later attempt inherited an incorrect
  600-second timeout override. The path and stable-gate defects are repaired,
  and the final run uses normal fixture timing controls. APFS passes do not prove
  the inferred HFS+ metadata explanation. Full-suite, performance and exact
  Linux crash-cut qualification remain outside this focused gate; the cold
  self-refresh CI limitation remains recorded above.

## Current state (main `8cec6ed`, 2026-10-16)

The 2026-10-15 [backend-neutral project model](doc/CMAKE_GREMLIN_PLAN.md#backend-neutral-project-model-decision-2026-10-15)
(`3df99aa`..`342da71`) is now the only selection path, and the wave-1 to wave-3
packages are on main:

- **All 19 audit bugs are fixed**, each with a negative oracle (table below).
  Further defects found and fixed along the way:
  - Darwin `/var` vs `/private/var` in PASS reuse, lint and CMake presets; the
    project root is now canonicalized at every entry point.
  - An MCP server crash on `changed`, and invalid MCP error JSON.
  - A no-op edit that left a lane stuck in `testing`.
  - A truncated CMake preset fixture and stale tests.
- **One service:** CLI, MCP, `fo check` and Gremlin select through one model
  service (`e9f1494`), and CLI and MCP share one routine per command
  (`32700fd`). One Gremlin option table drives the CLI, request validation and
  the MCP schema.
- **Native CMake split route:**
  - fo builds natively from File API compile groups (`5b0f8ca`), delegating
    only custom-command, generated-source and non-Ninja targets.
  - CTest cases run natively from json-v1 properties (`5a575e9`); delegated
    runs import JUnit.
  - Interpreter track: GORILLA's default profile runs without CMake
    (`4da4632`); its pFUnit coverage profile remains.
- **Gremlin:**
  - Parallel cases (`--jobs k`, `7540964`).
  - A case-keyed PASS index independent of journal compaction (`8cc55e9`).
  - Bounded, stop-aware reproduce; write-free idle quiescence.
  - Clone-based warm generations: 0 payload bytes copied for a one-file edit
    (`c6805f0`).
  - Retention in status (`0bf1c43`).
- **Output:** verdict-first compact JSON with bounded failure excerpts and live
  per-case progress (`648aff0`).
- **Cleanup:** `clean --stale` uses a recorded link graph (`ed8ffaf`); Fx's DAG
  grows without limit (Fx `6d48605`).
- **Code size:** D, F and H removed about 1,900 lines, but the new native
  CMake build/test, parallel Gremlin and storage features added more.
  Production code is 69,092 physical lines at `8cec6ed` (66,082 at
  `342da71`). The remaining #205 candidates are below.
- **Deployed 2026-10-16:**
  - faepmac1: `/opt/homebrew/bin/fo`, release build of `8cec6ed`,
    SHA256 `8c28a11e…`.
  - mailuefterl: `~/.local/bin/fo`, SHA256 `3718ea38…`, after its gates pass on
    Linux.
  - faepkub4 is not updated, because its SSH host key failed verification.
- **Host notes:** mailuefterl's global git config signs commits with a key that
  is unavailable non-interactively, so fixture repositories must disable
  signing (`8cec6ed`). fpm there is in `~/.local/bin`, which non-interactive SSH
  does not put on PATH.


### Since `8cec6ed` (2026-10-16)

- **Wave 4:**
  - Case-level discovery: `exe::case`, fo listing protocol and test-drive adapter, FLAKY with retries and `--strict-flaky`, SLOW warn-then-kill (`aad07e1`, `e64eb50`).
  - One-shot PASS cache shared with MCP, `-E` filter expressions, `--junit`, `--partition` (`3bf775d`).
  - Shell-free tests with one compiled fake command and isolated fixture git (`0dcd3ae`..`d42efad`).
- **Gremlin core:**
  - The lane owner is one `owner_t` with a short event loop; the supervisor shrank from 5,370 to 957 lines.
  - MCP and Gremlin share one execution service, and MCP `gremlin_reproduce` no longer blocks the server.
  - Status JSON is built with the Fx builder (`538a078`..`21e4822`).
- **Fixed regressions and races:**
  - Clone events on live inputs looked like edits (`c608258`).
  - Edits made during a capture were dropped (`839ab98`).
  - Host-slot contention between test fixtures; reproduce's unbounded slot wait (`3929134`).
  - The freshness acknowledgement bound (`8493757`).
  - A gfortran 16 array-constructor heap overflow in a fixture, and an `is_within` out-of-bounds read (`7941a57`).
  - Tag-pinned git dependencies on a clean cache (`aff0bce`).
  - The MCP stderr leak, wall-time label, per-run failure logs, JUnit strict-flaky, dev-dependency acquisition, and the PASS cache's extra CMake configure (`7590a83`..`12e2384`).
- **Upstream:** FortFront `a9e09f92` parses named DO constructs inside DO WHILE bodies (GORILLA `pusher_tetra_poly.f90`).
- **Evidence:** the full default suite on Darwin passes 173/173 at `21e4822` in 349 s wall. Production code is 72,895 physical lines.
- **Deployed:** faepmac1 runs `21e4822`; mailuefterl runs `7941a57` until its next Linux checkpoint passes.
- **Open:**
  - Stop-aware slot acquisition and lanes that outlive crashed fixtures.
  - Task-owned lanes (#142) and Gremlin status deltas (#218).
  - Admission directory garbage collection.
  - Linux slow fixtures on the host-wide admission directory.
  - The pFUnit profile for GORILLA.
  - Windows qualification.
  - Further #205 reduction.

### Since `21e4822` (2026-10-16, end of session)

- **Gremlin lifecycle (`ee2412e`):**
  - Task-owned lanes (`--owner task`, MCP `lifetime:"task"`; #142).
  - Lanes stop when their state directory disappears, and fixtures stop their lanes even after failures.
  - Admission garbage collection.
  - Verdict-first status/wait with failing-case excerpts and `since=` deltas (#218 closed).
- **Process liveness (`20c7374`):** a zombie no longer counts as alive, and the Linux capture monitor reaps every adopted orphan (`test_process_monitor_reaps`).
- **Test isolation (`947a249`):** every test process gets a private Gremlin admission domain, so other sessions' long-running lanes can no longer starve fixtures.
- **AFS:** Fx `eda9d6e` publishes store objects by same-directory copy when cross-directory links fail, and fo reports the store status and cache root on failure (`c54bc9c`). faepkub4 works with its default AFS cache.
- **Evidence at `20c7374`:**
  - Darwin full default suite 179/179 (369 s wall).
  - Linux full default suite 178 pass, 1 flaky (`test_native_contained_capture`).
- **Deployed:**
  - faepmac1 `/opt/homebrew/bin/fo` at `20c7374`.
  - mailuefterl `~/.local/bin/fo` at `20c7374`.
  - faepkub4 `~/.local/bin/fo` at `ee2412e` or later, built natively there.
- **Still open:**
  - The Linux flake in `test_native_contained_capture`.
  - #205 net code reduction (73k lines).
  - The GORILLA pFUnit profile and the other ITpPlasma CMake profiles (#198).
  - Compiler-result reuse (#199).
  - veggies, pFUnit and Fortuno case adapters.
  - Windows qualification (#139, #142).
  - Storage budgets (#168).
  - One-shot run isolation within one checkout (#170, #166).
  - Manifest forms (#201).
  - Optional audits (#145, #190).
  - Test-code reduction (#184).

### CMake consumer corrections (2026-10-16)

- `2f979d1` reuses configured CMake replies for `--no-build` selection, avoiding
  a configure/read race reproduced by the GLISS consumer.
- `614d951` preserves exact cache type, value and HELPSTRING, including wrapped
  comments, literal expansions and trailing spaces. Native initialization and
  explicit-override controls remain distinct.
- Frozen capture now relocates proven aliases of captured build/source roots,
  including `/tmp` versus `/private/tmp`, while preserving unrelated authored
  paths and distinct suffixes. The prior driver reproduces the escaped binary
  root; the repaired driver passes the same compiled native oracle.
- Candidate SHA256
  `ed54363ce4ed44a5d6ec74867f85ec4d692666da5304bba665f2da51936e0c14`
  uses FortFront `07bfbb441ed67866f2244368be80a188933c9b72` and Fx
  `eda9d6e4ec5287e07452f7c5402ccb5ccba8dfec`. Its original FortSym consumer
  builds and passes all four affected resident gates, then stops normally.
  This is focused consumer evidence, not full FortSym-suite qualification.

### Header-only native CMake correction (2026-10-17)

- Native action arrays now retain only compiled sources. Declared
  `HEADER_FILE_ONLY` inputs remain available for dependency tracking without
  leaving unused actions that attempted to archive a nonexistent `.o`.
- The maintained mixed C/Fortran CLI oracle fails on the prior `ed54363c`
  driver and passes on candidate SHA256
  `454c93dffd513b85bc8274c262a2d5364b20e4acf24c45f6ecd55de1769f2daf`.
  It covers headers in static libraries and executables, include-only edits,
  warm rebuilds, native/reference agreement and existing delegation controls.
- The original GLISS consumer at `8e51c65` passes its isolated optimized
  audit: clean lint, Release array-temporary errors and all 178 default tests.
  Candidate dependencies remain FortFront `07bfbb4` and Fx `eda9d6e`.

## Stage 1: fix every known bug

All audit bugs are fixed. Each repair started with an independent negative oracle
that failed on its parent.

| # | Defect | Owner | Status |
|---|---|---|---|
| B1 | `fo fmt` truncates lines over 512 characters and deletes lines past 50,000 when rewriting | #214 | fixed `fc3d21e` |
| B2 | CMake: edits to unclassified project files (test scripts, data, `*.in`, `.fypp`, `.pf`) select nothing and the old PASS is reused | #197 | fixed `337dae0` |
| B3 | `fo check` and the `fo` pipeline treat "changed" as a compile-cache miss and report green after a warm build | #189 | fixed `e9f1494` |
| B4 | Module closure skips `use` statements of submodules and external units in linked libraries, so impact and digests miss them | #189 | fixed `337dae0` |
| B5 | `--only-changed` misses edits in fpm path dependencies outside the project root | #189 | fixed `e9f1494` |
| B6 | Gremlin's first `--only-changed` generation uses cache misses (fpm) or all cases (CMake) instead of the model delta | #189 | fixed `e9f1494` |
| B7 | Source scanner skips `dependencies`, `_deps`, `venv` and other directories at any depth, and `build*`/`SRC` at the root, unlike FPM | #186 | fixed `4204f2a` |
| B8 | Silent truncation: Fx DAG 2,048 nodes, 8,192 scanned units, 512-character scan paths, 4,096-character `-I` list, manifest arrays and flags | #215 | fixed `855c75c`, Fx `6d48605`; some fixed fo arrays remain (#215) |
| B9 | MCP lint and output buffers (16,384/8,192 characters) cut JSON | #185, #215 | fixed `32700fd` |
| B10 | Bare fpm `fo test` and unnamed MCP `test` ignore `[extra.fo.test-defaults] exclude` | #167 | fixed `e9f1494` |
| B11 | CTest names with spaces lose them in the regex (`trim(regex)//ch`) | #194 | fixed `e9f1494` |
| B12 | Unknown top-level `fpm.toml` sections are silently ignored | #201 | fixed `855c75c` |
| B13 | `gremlin reproduce` waits for its build without a time bound or stop check while holding the heavy-work slot | #139 | fixed `8cc55e9` |
| B14 | Journal compaction to 512 records discards reusable PASS receipts in projects with more than about 512 cases | #140 | fixed `8cc55e9` |
| B15 | A CMake header listed by a target the consumer does not link reaches only that target's cases | #197 | fixed `337dae0` (widens) |
| B16 | Malformed MCP/Gremlin cursor values silently become 0 | #150 | fixed `32700fd` |
| B17 | `fo_scaffold`/`fo_doc` shell out with unquoted paths | #163 | fixed `fc3d21e` |
| B18 | An idle lane with a wait ticket writes 16 bytes every 100 ms | #155 | fixed `8cc55e9` |
| B19 | The slow rule is name-based in coverage and the CLI but a label in the model, so CMake `slow` cases are counted differently | #167 | fixed `e9f1494` |

## Stage 2: one service, no duplication (#205, #167, #185, #149)

The ranked survey of 2026-10-15 finds about 2,600 removable lines that need no
scope decision. In order of value:

1. One select→plan→run service on the project model, called by `cmd_test`,
   MCP `test` and Gremlin's `discover_campaign`. It removes the random/fallback
   paths, `backend_test`/`backend_test_names`, the cache-miss selection and
   name-based slow checks (fixes B3, B6, B10 and B19 at the root).
2. Delete the test-running half of `compile_and_run_tests`, which only tests
   reach, and merge `gfortran_test`/`gfortran_test_names`.
3. Make each MCP command (`changed`, `check`, `lint`, `graph`, `info`, `clean`,
   `install`) a thin adapter over the CLI routine; one Gremlin option table drives
   the CLI, the request parser and the MCP schema.
4. Validate receipts with Fx JSON instead of the hand-written C parser. Share
   one C helper set for write-all, fsync, atomic publish, mkdir, rm-rf, lock,
   FNV and process identity.
5. Replace private lowercase/path/list/table/comment helpers with Fx string,
   path and the existing manifest parsers; route lint and fmt through `fo_lint_lex`.
6. Remove about 215 lines with no caller, and move test-only C hooks to `test-support`.
7. Build JSON through the Fx builder so every output is valid by construction.

Supervisor responsibilities (`run_owner` is 617 lines) split into campaign,
model and receipt services as these duplicates disappear (#149).

## Stage 3: Gremlin and tests that are a joy to use

Ideas taken from Go test caching, cargo-nextest, Bazel/Buck2, Vitest/Jest,
pytest-testmon, ccache and ninja, mapped onto fo:

1. **Interface cutoff oracle.** A body-only edit recompiles one file and relinks;
   an interface edit recompiles exactly its dependents.
2. **Parallel Gremlin cases** within `FO_JOBS`, OpenMP share and heavy-lane
   admission, with prompt supersession (#216).
3. **Case-level discovery** inside executables; FLAKY with configurable retries
   and a strict mode; a SLOW warning, then termination (#217).
4. **One PASS cache** keyed by input closure for one-shot `fo test`, MCP and
   Gremlin, replayed as `reused`, with a `--rerun` opt-out (#138, #189). It is
   indexed by case and not limited by journal length (B14).
5. **No undeclared-input blind spots.** Unclassified files widen or are digested;
   compiler, flag and environment changes widen (#175, #189, #197).
6. **One filter expression** shared by CLI, MCP and Gremlin (name, label, layer,
   affected, failed), changeable on a live lane.
7. **Context-economical output** (#218): summary line, deltas, bounded failures,
   compact JSON, typed waits.
8. **Visible progress:** completed peers appear while slow cases run; wall and
   CPU time are attributed correctly (#130).
9. **Cheap warm generations:** no temp copy of existing blobs, clone/reflink
   materialisation, no second whole-tree build view (#165); retention visible in
   CLI/MCP (#168); write-free quiescence with one end-to-end oracle (#155).
10. **Isolation for ordinary runs:** per-test scratch and TMPDIR in one-shot
    `fo test`, as Gremlin already has (#170, #166).
11. **Truthful CTest evidence:** import `--output-junit` instead of scraping the
    console; distinguish fixture-blocked cases (#195); FIXTURES, DEPENDS,
    RESOURCE_LOCK, Unicode and duplicate-name fixtures (#194).
12. **Optional machine outputs** for users' own CI: `--junit PATH` and
    deterministic `--partition k/n`. No scheduler.

Coverage-based selection (testmon style) remains an opt-in audit experiment, never default.

## Decisions (2026-10-15)

- **Native CMake, in two stages.** Now: the split route. CMake only configures;
  fo compiles and links natively from File API compile groups and link
  fragments, and runs tests natively from CTest JSON properties (#193, #194,
  #196). Later: the `fo_cmake_native*` interpreter grows profile by profile
  toward the CMake-absent destination, starting with GORILLA (#192, #198). See
  [the CMake plan](doc/CMAKE_GREMLIN_PLAN.md#backend-neutral-project-model-decision-2026-10-15).
- **Side commands.** `fo bench` moves into the `bench/` audit tool (#190);
  `fo doc` is removed; `fo cover` and the FFC route stay.
- **No shell anywhere.** Product code and tests use native process calls; the
  generated `#!/bin/sh` fake tools become one compiled test-support program (#163).
- **Execution.** Parallel worktree agents (Sonnet first, Opus on failure or a
  rejected review) take exclusive file ownership per wave. The controller
  reviews, reruns focused gates on the integrated candidate and pushes small
  increments to main. Waves: 1 bugs, 2 robustness and de-duplication,
  3 native CMake split route, parallel Gremlin and agent output, 4 case-level
  testing, one PASS cache, storage and native tests.

## Issue map

| Area | Issues |
|---|---|
| Bugs (stage 1) | #214, #215, #189, #197, #186, #167, #194, #201, #139, #140, #150, #163, #155, #185 |
| Less code, one service | #205, #167, #185, #149, #150, #186, #184 |
| Inputs, affected selection, PASS cache | #175, #189, #138, #197 |
| Gremlin execution and lifecycle | #216, #155, #139, #142, #140, #166 |
| Case-level tests and evidence | #217, #131, #130, #195, #194 |
| Agent-facing output | #218, #185 |
| Storage and isolation | #165, #168, #170 |
| CMake configuration and profiles | #192, #193, #196, #198; optional #199 |
| fpm manifest coverage | #201 |
| Stale artifact cleanup | #129 |
| Optional audits (never gates) | #145 (also the real-Fluff deep-lint run), #190 |

Closed by the audit: #161, #211 and #59 (the real-Fluff run moved to #145).
Closed by delivery since: #214, #150, #155, #216, #129, #163 and #218.
In progress: #217 case discovery (M), #138/#189 one-shot PASS cache, filters,
JUnit and partitions (N), #163/#184 shell-free tests (P).
Every open issue has a 2026-10-15 audit comment stating what is delivered, what
remains and the confirmed defects.

## Working rules

Only actual consumer blockers gate the next task. Publish exact locally green
increments promptly, without waiting for GitHub CI, benchmarks or unrelated full
coverage. Use pinned candidate copies, warm caches and a task-private TMPDIR;
stop and remove completed task-owned lanes and worktrees after verifying
ownership. Benchmarks and acquired-project audits are opt-in and advisory.

Linux and Windows testing runs on **mailuefterl** (`ssh mailuefterl`). Source
checkouts: `/home/ert/code/lazy-fortran/{fo,fx,fortfront,ffc}`; preserve FFC's
unrelated local edit. The Windows VM `tpx-win2025` is reached through
`/var/tmp/fo-windows-20261008/run_winrm.py` via `127.0.0.1:15985`, with
UCRT64 at `C:\fo\msys64\ucrt64`. Rebuild from current sources.

Deployment history, evidence roots, exact driver digests and earlier
qualification records are in [the prior plan](https://github.com/lazy-fortran/fo/blob/342da71/PLAN.md).
Keep this plan short and update the owning issue when work moves.
