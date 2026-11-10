# fo architecture

## Product boundary

fo is a Fortran development driver: project build, incremental cache, test/run,
affected and continuous testing, formatting/lint/diagnostics and thin editor/agent
interfaces. It intentionally stays below the level of an environment manager or
scientific workflow platform.

fo does not replace fpm's manifest/registry role, Nix/Spack/container tooling,
CI/repository governance, agent schedulers, proof/synthesis systems or scientific
provenance/checkpoint/capsule infrastructure. The shared content store is a build
cache, not a general-purpose Nix store or scientific object store.

Compiler identity/version, effective flags, linker policy and declared project
dependencies are recorded sufficiently for correct build/test invalidation.
Complete host compiler-helper/sysroot/loader/system-library capture is not a fo
contract. Users requiring hermetic reproducibility provide that environment
externally.

## Build model

fo detects the nearest parent `fpm.toml` or `CMakeLists.txt`. A directory with
both uses the native fpm-manifest backend.

For an fpm project, fo parses the manifest, resolves source roots and
dependencies, scans Fortran modules, constructs a dependency DAG, and compiles
ready nodes through a native OpenMP loop. Compiler processes are launched with
argv vectors, without a shell. Applications, tests, and examples link against
a content-keyed static archive, so unreachable library objects cannot introduce
symbols or dependencies into a target.

Submodule ancestry is part of that DAG. A direct submodule depends on its
ancestor module; `submodule (ancestor:parent) child` depends on both the
ancestor module and the immediate parent submodule. Repeating that rule builds
longer ancestry chains in compiler order without source-filename conventions or
artificial ordering modules.

Each compilation action is keyed by:

```text
SHA-256(source content, compiler identity, effective flags,
        dependency module payloads)
```

Objects and module files are restored from the shared CAS. Module-interface
hashes allow an implementation-only edit to avoid recompiling dependents.
Binary links and successful tests have separate content keys.

GNU Fortran uses `-pipe` to pass compiler stages without intermediate files.
For compiler drivers supporting `-fuse-ld`, the link policy prefers an
available `ld.lld`; a failed LLD invocation is discarded and retried through
the driver's default linker. The selected policy is part of the binary action
key. `FO_LINKER=default`, `auto`, or `lld` overrides automatic selection.

## Parallel safety

OpenMP is the native scheduler. Compilation and test regions use worker-private
argv buffers, filenames, logs, clocks, and temporary paths. Shared progress is
atomic or protected by a named critical region. Cache entries are published
atomically. A project lock serializes build-tree materialization by concurrent
fo processes, while separate projects can share the CAS concurrently.

### Shared filesystems

`FO_COMPILE_CACHE_DIR`, when nonempty, selects a separate root for immutable
compiler action outputs (`store/v2` beneath that root). It defaults to
`FO_CACHE_DIR`. Compiler probes, mutable build state, test results, link outputs,
and Gremlin state continue to use `FO_CACHE_DIR`. Reuse requires the same
compiler, flags, dependency contents, source argument, compilation directory,
and exact include/module arguments;
copying a project to another directory conservatively recompiles its sources.

The shared store (`FO_CACHE_DIR`) uses no lock for correctness; README
"Cache and concurrency" states the protocol. It relies only on the
property below that held everywhere, an exclusive `link(2)`. Readers treat
a vanished, stale or unreadable record or object as a miss, and every object
is hash-verified before use.
Measured with `test-support/fs_probe`:

| Property | BeeGFS (sCluster home) | NFSv3 `noac` | NFSv4.2 `noac,lookupcache=none` | MooseFS (`/temp`) | AFS (OpenAFS home) | local btrfs |
|---|---|---|---|---|---|---|
| Reader during rename-over | ENOENT (377 of 4320 reads) | ESTALE (74 of 4192) | atomic | atomic | EIO (61 of 20544) | atomic |
| `renameat2(RENAME_NOREPLACE)` | EINVAL | EINVAL | EINVAL | yes | EINVAL | yes |
| `flock`/`fcntl` seen by the peer | no (host-local) | yes | yes | yes | yes | yes |
| `O_EXCL` create race | exclusive | exclusive | exclusive | exclusive | NOT exclusive (1 of 200 claimed by both) | exclusive |
| `link(2)` race | exclusive | exclusive | exclusive | exclusive | exclusive (same directory only; cross-directory EXDEV) | exclusive |
| mtime granularity | 1 s | 1 ns | 1 ns | 1 s | coarse (one value over 28 ms) | 1 ns |
| atime on read | strictatime | not updated | not updated | strictatime | strictatime | noatime |
| Read via fd after peer unlink | full | full | full | full | EIO after 292 KiB | full |

Sources, all 2026-10-19 with two hosts or clients: sCluster job 3275607
(nodes 16 and 17; also job 3275343); NFS between two privileged containers
against kernel nfsd on one host (an earlier run saw ESTALE after 64 KiB for
a read after peer unlink on NFSv3); MooseFS and AFS between faepop35 and
faepop36. With default attribute caching an NFS client did not see a peer's
new file for more than 20 s, so mount shared caches with `noac` or
`actimeo=0`. The store never depends on `O_EXCL` for exclusivity: objects
are published by `link(2)` (by copy plus a same-directory link where
cross-directory links fail, as on AFS), temporary names carry the pid, and
every read verifies the hash.

The store stress driver in fx (`test-support/store_stress`) runs concurrent
publishers, readers and evictors, kills writers with SIGKILL, and then checks
that no restored byte differs from its key's content, that every object
hashes to its name, and that a full eviction leaves no file behind. It
passed on all six filesystems above (local btrfs on one host).

No Fortran formatted I/O occurs in the compiler child after a threaded fork.
The process shim launches prebuilt argv vectors with `posix_spawn` or the
platform-equivalent safe path.

## fpm contract

fo reads the established `fpm.toml` format. Common library, executable, test,
example, dependency, preprocessing-macro, link, and Fortran-language settings
are mapped into the native model. Explicit target names remain the public names,
including when the source is nested or has a different filename.

Native tests may receive target-specific command-line arguments through the
FPM-standard `extra` namespace:

```toml
[extra.fo.test-args]
test_oracle = ["test/data/reference.csv", "--strict"]
```

The key is the public test name, and each quoted array element remains one
argument. Test arguments participate in the cached test-result key.

Gremlin can capture private test and runtime fixtures with
`[[extra.fo.inputs]]`. Each row requires a `path` relative to its input root
and a `role` of `test-fixture` or `runtime-fixture`; `root` defaults to
`project`, and `kind` defaults to `file`. A directory declaration captures
the tree recursively. Dependency-root fixtures are materialized below
`.fo-inputs/<root-alias>` in each private execution view:

```toml
[[extra.fo.inputs]]
root = "dependency:fortfront"
path = "examples/f90"
kind = "directory"
role = "test-fixture"
```

A manifest can also declare library tiers, test groups and default test
exclusions. fo parses and validates them; selection, `--layer` and `-L` consume
them in later stages. Plain fpm ignores the whole `[extra]` namespace:

```toml
[extra.fo.layers]
base = { source = ["src/base/**"] }
physics = { modules = ["physics_mod"], uses = ["base"] }

[extra.fo.test-groups]
slow = { tests = ["test/*_slow*"], inputs = ["data/reference.dat"] }

[extra.fo.test-defaults]
exclude = ["slow"]
```

A layer names its members by `source` globs or by `modules`, exactly one of the
two, and lists the layers it may use in `uses`. A group name is a test label;
`tests` are globs and every `inputs` entry must be the `path` of an
`[[extra.fo.inputs]]` row. `test-defaults.exclude` lists the labels left out of
a default run. The only accepted keys are the ones shown. fo rejects the
manifest for an unknown key, a `uses` or `inputs` reference that does not
resolve, a duplicate name or a layer cycle (the diagnostic names the cycle).

Consolidated tests can opt into one dispatcher executable:

```toml
[extra.fo]
dispatcher = "test_suite"
```

Each case source starts with `! fo: dispatcher`. It may be a module exporting
the case procedure, with its original filename retaining the public test name.
The dispatcher uses those modules and accepts exactly one bare public case name
as its argument. fo compiles and links the dispatcher once before running cases
in parallel. Both `fo test <name>` and `fo test --all` report the original names;
the dispatcher itself runs only when explicitly selected. Manifest aliases and
nested test sources follow the same rules. Unmarked test programs retain their
own executables.

fo and fpm deliberately differ in private state:

- fo stores disposable project views in `build/fo`
- fo stores reusable actions in the global SHA-256 CAS
- fpm stores compiler and flag hashes in private `build/<compiler>_<hash>`
  directories
- fo does not fabricate fpm digest files, so a later fpm command safely rebuilds
  or reuses only state fpm owns

Shell output is also intentionally different. fo emits compact progress and
bounded diagnostics for LLM edit loops.

Git and registry dependency acquisition uses fpm as a bootstrap when required.
`fo install` builds release-profile app executables with the native backend and
installs them under `prefix/bin`; example, test, library and module installation
remain unsupported. fo does not replace fpm's package registry or grow a
separate system/environment package manager.

## CMake contract

CMake mode configures `cmake -S . -B build -G Ninja`, forwards `FC`,
`FO_CMAKE_ARGS`, and effective Fortran flags, then calls
`cmake --build build -j <FO_JOBS>`. Tests run through CTest with parallelism,
failure output, named filters, and slow-test label exclusion. fo reads CMake's
File API and CTest registry as a project model for test selection (see below)
but does not replace CMake's target model.

## Gremlin tuning

`FO_GREMLIN_RECEIPT_LIMIT` sets how many campaign receipts a lane retains in
its journal tail. It is clamped to 16 through 512, the default. PASS reuse does
not depend on it: a per-case index keeps the latest reusable PASS of every case.

Lanes of one user share a host pool of `FO_GREMLIN_HOST_SLOTS` work slots
(default: the processor count, at most 256). A lane reserves max(`--jobs`,
`FO_JOBS`) slots, clamped to the pool, while it builds or runs cases and
releases them when quiescent; a lane that does not fit waits with the status
diagnostic `waiting for a host Gremlin work slot held by other lanes`. Lanes
without `FO_JOBS` take the whole pool. Lanes that set different pool sizes take
turns until the holders of the other size are quiescent.

## Test selection

`fo test` selects cases on one project model for both backends: the fpm
manifest and source scan, or CMake targets and CTest cases after configure.

```text
fo test [name|glob ...] [-L RE] [-LE RE] [--layer NAME] [-E EXPR] [--all] [--only-changed] [--no-build]
        [--rerun] [--partition hash:K/N|count:K/N] [--junit PATH]
fo test --list [--json] [same selection]
```

- Names are exact case names or whole-name globs (`*`, `?`, `[...]`). An exact
  name that is no case exits 1 (`unknown test`). A glob that matches nothing
  selects nothing.
- `-L, --label RE` keeps cases with a label matching RE; repeated options must
  all match. `-LE, --exclude-label RE` drops cases whose labels match every
  repeated RE; it also wins over `--all`. Regexes use the CTest subset documented
  in `src/util/fo_pattern.f90`.
- `--layer NAME` keeps cases of a layer; repeated options form a union. A
  case's layer is the highest-rank layer reached from it: fpm, through the
  module uses of its source; CMake (layers are project libraries), through the
  link closure of its command target.
- Default exclusions: the `slow` label (fpm: `test-defaults.exclude` when set;
  CMake with a test preset: the cases the preset filters out). `--all` lifts
  them; `-L slow` lifts the matching label. A default-excluded case named
  exactly needs `--all`.
- `--list` prints the selected cases (labels, layer, reason), the targets that
  must be built for them, and whether the plan widened to every target with the
  reasons. It configures CMake but builds nothing. `--json` emits the same as
  one object.
- Running (CMake, also plain `fo test`): select, plan, build, run. CMake is
  configured once and the loaded model is reused. Only the plan targets are
  built (merged with `FO_CMAKE_BUILD_TARGETS`). A case whose command maps to no
  target, such as a script, widens the plan: it builds the default target set,
  or the `FO_CMAKE_BUILD_TARGETS` targets when that variable is set, plus the
  targets of the mapped cases. A case's command is taken from CTest, or from the
  generated `CTestTestfile.cmake` before its executable exists, so an unbuilt
  tree plans exactly. The cases then run through `ctest -R` with exact
  anchored names in batches, so a fixture runs once per batch. A case the test
  preset filters out and `--all` selects overrides the preset's label filter
  for the run. No label is excluded except through the default exclusions above.
- `--no-build` runs the selected cases without building (CMake: whatever is
  built; a missing executable fails the case). When the CMake build location is
  known, selection reads its existing File API replies without configuring;
  parallel Gremlin children share those immutable replies. A configure preset
  without a build-root hint still needs configuration to discover its binary
  directory; set `FO_CMAKE_BUILD_DIR` to avoid that discovery run.
- `fo build --tests` builds what every test case needs, slow ones included:
  fpm builds its test programs, CMake builds the plan targets of the model's
  cases (cases a test preset filters out excluded), with the widening rule
  above, so an `EXCLUDE_FROM_ALL` test executable is built. `--case NAME`
  (repeatable) builds only what the named cases need; `-L`, `-LE` and
  `--layer` build the plan targets of that `fo test` selection (CMake; fpm
  builds every test program).
  Gremlin builds each generation once this way (a campaign with explicit
  targets and no random cases, and `reproduce`, pass their cases); its case runs and `reproduce`
  use `fo test --no-build` on that build, and a failed build is reported once
  for the generation without running any case.
- A Gremlin lane takes `-L/--label RE`, `-LE/--exclude-label RE` and
  `--layer NAME` (MCP `labels`, `exclude_labels`, `layers`), selected by the
  generation's model with the `fo test` semantics. The lane's campaigns run
  only the scope's cases (not preset-filtered, not explicit-only) and its
  generation builds the scope's plan targets. A scope selecting no case, an
  unknown layer or an invalid pattern is an inventory error. The scope is part
  of the lane policy: a start with another scope on a live lane is refused.
  A `reproduce` of a case outside the lane's build builds that case's targets
  in its own view before running it.
- Gremlin loads the project model once per built generation and takes its
  campaign cases from it. A later generation reuses the model when the content
  of its manifests, locks, CMake scripts, presets and caches, test and program
  mains, and the set of input paths are unchanged (fpm with source layers:
  every source counts), so a library or CMake source edit reuses it.
  `gremlin status --detail full` reports `active_model` (model identity) and
  `active_model_generation` (the generation that loaded it). Case receipts
  carry the case `labels` and `layer`, and a label-excluded case runs with
  `--all`.
- Gremlin prioritizes the cases a new generation's edits affect by the
  `--only-changed` rule below, applied to the project files whose inventory
  entries differ from the previous generation. A changed input outside the
  project files (manifests, include trees, dependency trees, CMake context),
  an incomplete inventory or another fo driver affect every case.
- Each case receipt carries `closure_digest`, the digest of the case's input
  closure: the contents of every file whose change affects the case by that
  rule, the files that affect every case, the case labels and attributes, the
  toolchain identity and the fo driver. A case whose latest lane campaign
  receipt with the same digest is a `PASS` from an earlier generation, and
  which has no receipt in the current generation yet, is not run: its receipt
  is a `PASS` with `"disposition":"reused"` and `reused_from` naming that
  generation. Run cases have `"disposition":"run"`. A failure with the same
  digest after that `PASS` prevents reuse.

- `--only-changed` restricts the selection to the cases the files changed
  against Git HEAD affect (tracked edits, staged or not, deletions and
  untracked files that are not ignored; build trees are skipped). It combines
  with names, labels, layers, `--all` and `--list`, the same on fpm and CMake.
  A case is affected by a changed file that is a declared input of it (a test
  group `inputs` path of `[[extra.fo.inputs]]`, or the CTest `REQUIRED_FILES`
  property; a directory covers the files below it), a source in the module-use
  closure of its test program, or a source of a target it links that no unit
  uses as a module (external procedures, submodules, C sources). A case whose
  command maps to no target is affected by any relevant change. A changed
  build description (`fpm.toml`, `CMakeLists.txt`, `*.cmake`, CMake presets),
  a changed Fortran/C source that belongs to no target, or a missing Git
  baseline widens to the whole selection; fo prints why. Other files
  (documentation, undeclared data) affect nothing. `--list` shows the reason
  per case; nothing affected prints `no affected tests` and exits 0.

- `-E, --filter EXPR` (MCP `filter`, Gremlin lane start `filter` /
  `--filter`) is one filter expression over the selected cases, applied by the
  model selection after names, labels and layers, so every flag above is sugar
  that it narrows further. Predicates: `name(GLOB)` (whole case name),
  `label(RE)` (CTest regex, as `-L`), `layer(NAME)`, a bare `WORD` (carries the
  label WORD, so `!slow` works), `affected()` (the Git delta of
  `--only-changed` reaches the case; it does not restrict the rest, so
  `affected() | name(x)` works) and `failed()` (the case failed, or timed out,
  in the last recorded run of this project). Operators: `!`, `&`, `|` (`&`
  binds tighter) and parentheses, for example
  `-E 'name(core_*) & !slow & affected()'`. The filter does not lift default
  exclusions: add `--all` to select `slow` cases. An invalid expression exits 1
  (`invalid filter`). A Gremlin lane takes a filter at start and keeps it as
  part of its lane policy (like `-L`); `affected()` and `failed()` are refused
  there, and changing the filter of a running lane is not supported (stop and
  start the lane).
- `--partition hash:K/N` or `count:K/N` (1 <= K <= N) keeps part K of N of the
  selected cases, for the user's own CI matrix. `hash` assigns a case by the
  SHA-256 of its name alone, so a case keeps its part when other cases come and
  go; `count` cuts the name-sorted list into N contiguous, balanced blocks.
  The N parts are disjoint and cover the selection. An empty part runs nothing
  and exits 0.
- `--junit PATH` also writes JUnit XML of the run to PATH (atomically): one
  `testcase` per result, `<failure>` for FAIL, `<error>` for TIMEOUT,
  `<skipped>` for skip and untested; reused and flaky cases pass and say so in
  their `system-out`. Markup is escaped and characters XML 1.0 forbids become
  `?`. fo schedules nothing: the CI runs each part itself.
- PASS reuse (`fo test`, MCP `test`). A selected case whose input closure
  digest equals the digest of its recorded PASS is reported `reused` (counted
  in the verdict, `3 reused`, and in the JSON as `reused`) and not run. The
  digest is Gremlin's `closure_digest` (the same code: the case's input closure
  files, the files that affect every case, labels and attributes, the toolchain
  identity and the fo driver), plus the build profile and `--flag` text. An
  undeclared project file (data), a manifest or any input outside the project
  files changes every digest, so every case runs; an incomplete or unknown
  input leaves the case without a digest and it always runs. Only a `PASS`
  records a digest: FAIL, TIMEOUT and FLAKY record their status (for
  `failed()`) and are never reused, and a successful build is never a PASS.
  `--rerun` (MCP `rerun`, or `FO_TEST_RERUN=1`) runs every selected case; `--random` always does.
  `--no-cache` neither reuses nor records (Gremlin's own case runs use it:
  they keep their receipts and the frozen view read-only).
  The record is `build/fo-test-index` (CMake: the model's build directory), one
  line per model case (dropped when the case disappears), replaced atomically;
  a missing or damaged file means nothing is reused. A case that depends on
  state outside its inputs (time, network, environment) needs `--rerun`.

MCP `test` uses the same selection for its names, `filter` and `only_changed`.
`--list`, labels, layers, `-E`, `--partition` and `--only-changed`
are not available with `FO_CMAKE_NATIVE`; `--random` conflicts with the
selection flags, `-E` and `--list`.
`fo check` and the `fo` pipeline run the
cases the sources changed since the last cached build affect, through the same
model mapping.

## Layering check

```text
fo graph --layers [--json]
```

checks the layers of the project model with one rule on both backends: a
source in layer A that uses a module defined by a source in layer B is a
violation when B is outside the `uses` closure of A. fpm layers are
`[extra.fo.layers]`; on CMake every project library is a layer whose `uses` are
its link dependencies, so a library using a module of a library it does not link
violates. Module definitions and uses come from fo's source scan; CMake sources
are mapped to targets through the File API (configure only, nothing is built).
Intrinsic and dependency modules (fpm dependencies, FetchContent or external
CMake targets) are ignored, as are providers in no layer.

Each violation names the source, the module, the provider, both layers and the
missing edge `A -> B`. The output also lists the project sources in no layer
(CMake executables, for instance) and sources the scanner could not read.
The exit status is 0 when clean, 1 on a violation and 2 when no project model
is available. `--json` prints one object with `ok`, `checked`, `layers`,
`violations`, `unlayered` and `unscanned`.

## Agent interfaces

`fo check --json=compact` and MCP check return a bounded JSON result. MCP supports
check, status, diagnostics, cancellation, build, test, graph, info, changed,
clean, lint, format, and release installation. MCP clean preserves the shared
CAS unless `cache=true`. MCP installation accepts a prefix and always requests
the release profile.

### Test output

Every `fo test` run reports one verdict line, such as
`12 pass, 1 FAIL, 2 skip (4.10s wall)`. Counts other than pass are omitted when
zero; `flaky`, `untested` and `reused` (PASS reuse, see above) appear when nonzero. The time is the run's wall
clock (`wall`) when the runner recorded it, otherwise the sum of the per-case
times (`summed`). Per-case slow-test warnings are CPU time and say `CPU`.

- Human text lists failing, flaky, skipped and untested cases (passing cases only
  for named runs, filters, `--random`, `--only-changed` or `--verbose`) and
  **ends** with the verdict line. `--quiet` or `--summary` forces the
  failures-only listing and silences the live lines. A failing case shows the
  head and tail of its output (at most 20 lines and 2 KiB, 240 bytes per line)
  unless `--verbose` is given; when anything failed the next line is
  `log: PATH`.
- While cases run, one line per completed case is written to **stderr**:
  `fo: [3/200] PASS test_x 0.01s`, in completion order, so the peers of a slow
  case are visible while it runs. A failed first run prints `fail (retrying)`:
  failures are rerun alone once, and the report has the final FAIL or FLAKY.
  The report's case order stays the scan order. `--json`, `--quiet`,
  `--summary`, the MCP tool and `FO_NO_PROGRESS` suppress these lines.
- `fo test --json` and the MCP `test` tool (default, or `json="compact"`) return
  the same compact object, **starting** with the verdict. Passing cases are
  counted, not listed:

  ```text
  v        verdict line            pass/fail/skip   counts (untested, flaky if > 0)
  ok       true when exit == 0     wall_s | sum_s   wall clock, or sum of case times
  exit     process exit status     log              retained full log (only on failure)
  cases[]  every non-passing case:
    n name, st status (fail timeout flaky skip untested), s seconds,
    why reason, at first file:line found, msg that diagnostic line,
    out head/tail excerpt, cut true when out is shortened
  at, out, cut   when no case ran (build failure): bounded excerpt of the log
  ```

  The report stays under about 4 KiB for one failing case; only the first ten
  failing cases carry `out`, the others name, status and time. `log` is the full
  log of the last failing run, `build/fo/test-logs/<id>.log` of the project
  (one file per failing run, the newest ten kept): read it, or rerun one case with
  `fo test NAME`.
- `fo test --json=full` and MCP `json="full"` return the complete structured
  report with every failing and passing entry and its whole output, growing with
  the results instead of truncating at a byte limit.

`fo lint --deep [paths...]` adds Fluff diagnostics to the native source checks
and compiler warnings. Install `fluff` on PATH, then use `--json` to receive the
usual native fields plus a lossless `deep_diagnostics` array, including locations
and fix suggestions. Source selections use Fo's inventory and exclude generated
and dependency trees. Ordinary lint, build, test, and Gremlin commands keep their
existing native checks.

Deep lint runs `fluff check --output-format json` once for the selected input set,
using `.fluff.nml` or `fluff.nml` in the project directory. Valid findings return
exit code 1; missing tools, execution failures, malformed diagnostics, and damaged
cache records return exit code 2 with an explicit error. Unchanged analysis can
reuse a typed diagnostics artifact in the shared action-result store. The key
includes selected filenames and source bytes, the manifest, both configuration
files, the Fluff executable bytes, and the command policy. Changing an input
invalidates reuse.

The test-failure-path lint rule follows failure exits through project module
helpers. It scans module procedures in `src/`, `app/`, and `test/`, repeats the
collection until transitive helper calls converge, and distinguishes calls from
ordinary variables or array references with the same name. A helper that cannot
exit nonzero does not suppress the finding. Procedure pointers, generic-only
resolution, and separate submodule bodies remain documented unsupported cases.

The LSP surface reports compiler-backed diagnostics on save. It does not claim
to replace a full semantic Fortran language server.
