# Fo delivery goals

Updated 2026-10-15 from a full audit of every open issue at main `342da71`.
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

## Current state (main `342da71`)

The 2026-10-15 [backend-neutral project model](doc/CMAKE_GREMLIN_PLAN.md#backend-neutral-project-model-decision-2026-10-15)
landed in about 30 commits (`3df99aa`..`342da71`), and all four phases have code
on main:

- **P1:** model core; CMake File API and `ctest --show-only` adapter; `fo exec`
  and `fo test` (globs, `-L`/`-LE`, `--layer`, `--list`, `--no-build`) select on
  the model; CMake builds plan targets and runs exact cases in ctest batches.
- **P2:** Gremlin loads the model once per generation, builds once
  (`fo build --tests`), runs cases with `fo test --no-build`, scopes lanes by
  label or layer, and reuses a case PASS while its input-closure digest is unchanged.
- **P3:** `[extra.fo.layers]`, `test-groups` and `test-defaults` for fpm;
  `fo graph --layers` on both backends.
- **P4:** `--only-changed` against Git HEAD on both backends; CLI and MCP share it.
- **Oracle:** a Fortran-generated twin fpm/CMake fixture checks identical
  selections, plans and layering through `fo test` and Gremlin; CI runs the
  bounded project-model gates.

The model was added next to the paths it replaces. Production code is 66,082
physical lines (+36.7% over the `8a0197d` baseline), and the remaining
duplicates have already drifted into bugs B3 and B4 below. The interface-hash
cutoff exists in the native build (`get_interface_key`, `fo_gfortran_build.f90`)
but has no independent oracle yet.

## Stage 1: fix every known bug

Each repair starts with an independent negative oracle that fails on `342da71`.
False greens and data loss come first.

| # | Defect | Owner |
|---|---|---|
| B1 | `fo fmt` truncates lines over 512 characters and deletes lines past 50,000 when rewriting | #214 |
| B2 | CMake: edits to unclassified project files (test scripts, data, `*.in`, `.fypp`, `.pf`) select nothing and the old PASS is reused | #197 |
| B3 | `fo check` and the `fo` pipeline treat "changed" as a compile-cache miss and report green after a warm build | #189 |
| B4 | Module closure skips `use` statements of submodules and external units in linked libraries, so impact and digests miss them | #189 |
| B5 | `--only-changed` misses edits in fpm path dependencies outside the project root | #189 |
| B6 | Gremlin's first `--only-changed` generation uses cache misses (fpm) or all cases (CMake) instead of the model delta | #189 |
| B7 | Source scanner skips `dependencies`, `_deps`, `venv` and other directories at any depth, and `build*`/`SRC` at the root, unlike FPM | #186 |
| B8 | Silent truncation: Fx DAG 2,048 nodes, 8,192 scanned units, 512-character scan paths, 4,096-character `-I` list, manifest arrays and flags | #215 |
| B9 | MCP lint and output buffers (16,384/8,192 characters) cut JSON | #185, #215 |
| B10 | Bare fpm `fo test` and unnamed MCP `test` ignore `[extra.fo.test-defaults] exclude` | #167 |
| B11 | CTest names with spaces lose them in the regex (`trim(regex)//ch`) | #194 |
| B12 | Unknown top-level `fpm.toml` sections are silently ignored | #201 |
| B13 | `gremlin reproduce` waits for its build without a time bound or stop check while holding the heavy-work slot | #139 |
| B14 | Journal compaction to 512 records discards reusable PASS receipts in projects with more than about 512 cases | #140 |
| B15 | A CMake header listed by a target the consumer does not link reaches only that target's cases | #197 |
| B16 | Malformed MCP/Gremlin cursor values silently become 0 | #150 |
| B17 | `fo_scaffold`/`fo_doc` shell out with unquoted paths | #163 |
| B18 | An idle lane with a wait ticket writes 16 bytes every 100 ms | #155 |
| B19 | The slow rule is name-based in coverage and the CLI but a label in the model, so CMake `slow` cases are counted differently | #167 |

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

Closed by this audit: #161 (native fixtures delivered), #211 (store-root guard
delivered) and #59 (deep lint delivered; the real-Fluff run moved to #145).
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
