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

## Test selection

`fo test` selects cases on one project model for both backends: the fpm
manifest and source scan, or CMake targets and CTest cases after configure.

```text
fo test [name|glob ...] [-L RE] [-LE RE] [--layer NAME] [--all] [--no-build]
fo test --list [--json] [same selection]
```

- Names are exact case names or whole-name globs (`*`, `?`, `[...]`). An exact
  name that is no case exits 1 (`unknown test`). A glob that matches nothing
  selects nothing.
- `-L, --label RE` keeps cases with a label matching RE; repeated options must
  all match. `-LE, --exclude-label RE` drops cases whose labels match every
  repeated RE; it also wins over `--all`. Regexes use the CTest subset documented
  in `src/util/fo_pattern.f90`.
- `--layer NAME` keeps cases of a layer; repeated options form a union.
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
  built (merged with `FO_CMAKE_BUILD_TARGETS`; a case whose command maps to no
  target, such as a script, widens the plan to the default build unless that
  variable scopes it). A case's command is taken from CTest, or from the
  generated `CTestTestfile.cmake` before its executable exists, so an unbuilt
  tree plans exactly. The cases then run through `ctest -R` with exact
  anchored names in batches, so a fixture runs once per batch. A case the test
  preset filters out and `--all` selects overrides the preset's label filter
  for the run. No label is excluded except through the default exclusions above.
- `--no-build` runs the selected cases without building (CMake: whatever is
  built; a missing executable fails the case).

MCP `test` uses the same selection for its names. `--list`, labels and layers
are not available with `--random`, `--only-changed` or `FO_CMAKE_NATIVE`.

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

MCP `test` accepts `json="full"` or `json="compact"` for the complete structured
test report, including every failing and passing entry. The report and its MCP
response grow with the results rather than truncating at a fixed byte limit.

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
