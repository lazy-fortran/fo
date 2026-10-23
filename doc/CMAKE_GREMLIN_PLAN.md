# Native ITpPlasma compatibility goals

Fo's final CMake/CTest destination is faithful standalone execution of the
maintained ITpPlasma project/profile subset. Supported profiles build, run, test
and install without installed CMake/CTest. Unsupported constructs are explicit.
Current delegated support remains useful interim behavior and is labelled as
such; it does not count as completed native compatibility.

This is the fifth delivery priority in [PLAN.md](../PLAN.md). It follows useful
Fo/FFC Gremlin development and standalone FPM, with only actual consumer
dependencies blocking each increment.
[Architectural freedom](GOAL_DRIVEN_DEVELOPMENT.md) applies:
agents choose the implementation as requirements become concrete. The project
model decision below fixes the File API/CTest JSON as the current CMake
authority; slot pools, launchers and module lists remain open.

## Backend-neutral project model (decision 2026-10-15)

Selection, planning, layering and impact are implemented once, on one project
model; only adapters and two execution primitives differ per backend. This
replaces the per-backend branches in `fo_build_backend`, `app/main.f90`
`cmd_test`, the MCP test handler and the Gremlin case list (#167, #205).

- **Model.** Targets (library/executable, sources, link dependencies,
  artifact path), layers (named library tiers with allowed `uses`), cases
  (public name, command target, labels, derived layer, adapter attributes
  carried opaquely), default excluded labels, and an identity digest over the
  adapter inputs. Fo's module graph (`fo_scan`, `fo_dag_bridge`) attaches to
  target sources on both backends.
- **CMake adapter.** CMake is the authority; CMakeLists are not parsed.
  Targets, sources, link dependencies and artifacts come from File API
  `codemodel-v2` (already queried by `fo_cmake_context`); cases with command,
  `LABELS`, `FIXTURES_*`, `PROCESSORS`, `RESOURCE_LOCK` and
  `WORKING_DIRECTORY` come from `ctest --show-only=json-v1` (already run by
  `cmake_registered_names`). Layers are library targets. Default exclusions are
  the cases the selected test preset filters out: `ctest --preset P
  --show-only=json-v1` applies the preset filter (checked with CMake 4.4.4), so
  presets JSON is not parsed. File API has no Fortran module directory; fo does
  not need one because CMake builds. A case whose command is not a known
  artifact maps to all targets (widened plan, reported).
- **fpm adapter.** `fpm.toml` plus `[extra.fo.layers]`
  (`name = { source = [...], uses = [...] }`), `[extra.fo.test-groups]`
  (`name = { tests = [globs], inputs = [...] }`, the group name is a label) and
  `[extra.fo.test-defaults] exclude = [...]`. Group `inputs` name rows of the
  existing `[[extra.fo.inputs]]`; there is no second input declaration form.
  The `*_slow` naming rule becomes the label `slow`. A test's layer is derived
  from its module-use closure; only tiers are declared.
- **Shared operations.** Selection (exact names/globs, `-L`/`-LE`,
  `--layer`, default exclusions, later affected via #189); planning (selected
  cases to the closure of required targets); layering lint (a source must not
  use a module provided by a layer outside its declared `uses` closure; on CMake
  the declared closure is the target link graph, which CMake itself does not
  enforce for Fortran module use); impact mapping (`test_impact_select`
  consumes model cases).
- **Primitives.** `build(targets)`: fpm the native builder restricted to the
  selected executables; CMake the native split route below, with
  `cmake --build --target ...` for targets that need custom commands or
  generated sources. `run(cases)`: fpm the fo runner; CMake the native CTest
  runner below, with `ctest` and exact anchored names as the fallback for
  properties it does not yet implement.
- **Gremlin.** The same service path as `fo test` (#167): the cached model is
  reused per generation unless manifest, CMake, preset or cache inputs change;
  selection is computed once; one build per generation (#196) and case
  children run the `run` primitive only (today each child's `fo test <case>`
  rebuilds, on CMake a full `cmake --build`); CMake impact stops being
  "all registered cases" once cases map to targets (#197); lanes are scoped by `-L`/`--layer` (affected first, the rest of
  the scope as background coverage). Case-result reuse keyed by the case input
  closure belongs to #189/#140 (#199 is compiler-result reuse).
- **Public surfaces.** `fo test --list [--json]` reports selection and build
  plan without building; `fo graph --layers [--json]` reports layers and
  violations. CLI and MCP share the selection call.
- **Oracle.** A Fortran-generated twin fixture (an fpm and a CMake manifest for
  the same sources, layers, labels and tests) must give identical selections,
  plans and layering verdicts through both backends in `fo test` and Gremlin.

**Native CMake (decision 2026-10-15, revised).** Two stages, both native:

1. **Split route (now).** CMake only configures. Fo reads File API
   `codemodel-v2` compile groups (language, fragments, includes, defines,
   source indexes), link and archive fragments, generated-source flags and
   `toolchains-v1` compilers, then compiles and links natively with its own
   scheduler, module scanning, interface cutoff and shared cache. Tests run
   natively from `ctest --show-only=json-v1` properties (command argv,
   WORKING_DIRECTORY, ENVIRONMENT, TIMEOUT, pass/fail regex, WILL_FAIL,
   SKIP_RETURN_CODE, REQUIRED_FILES, DISABLED, FIXTURES, DEPENDS,
   RESOURCE_LOCK, RUN_SERIAL, PROCESSORS). Configure reruns only when CMake
   inputs change. CMake remains the authority on project meaning.
2. **Interpreter track (later).** `fo_cmake_native*` (opt-in
   `FO_CMAKE_NATIVE=1`) is kept and grows profile by profile toward the
   standalone (CMake-absent) destination, starting with GORILLA, each profile
   checked against the split route (#192, #198). It is not deleted.

## Current implementation

At the 2026-10-08 source handoff (`de85cf9`), the delegated route coexists with
an opt-in native Fortran slice selected by `FO_CMAKE_NATIVE=1`. The native slice
includes selected executable/static-library builds, registered target tests,
captured configuration and frozen Gremlin replay. Presets, multi-configuration
generators, explicit toolchains and broader CTest semantics remain unsupported
or outside this slice. The default route still delegates to CMake/CTest.

Split route (stage 1, `src/build/fo_cmake_build.f90`): after configure, fo
reads the File API codemodel and toolchains, compiles C, C++ and Fortran
sources on its own cached scheduler (per-source compiler, fragments, includes,
defines and module directory; Fortran scanned after the compiler's
preprocessor; C/C++ headers part of the action key) and archives and links
with CMake's own fragments. Single-configuration Ninja trees only. Targets
that are not executables or static libraries, or have generated sources,
custom or PRE_LINK/POST_BUILD commands, a source built differently by two
targets or a module defined twice, go to `cmake --build --target T` before the
native targets; `build/.fo-native-cmake/route.log` records every decision.
`FO_CMAKE_DELEGATE_BUILD=1` forces the previous all-CMake build.
`test_cmake_native_build_cli` is the oracle.

`test_native_cmake_cli` contains a numerical reference oracle, cold/warm/edit
and restore checks, frozen Release flags and traps for delegated tool use.
Their presence does not establish current combined-source green. Recheck the
slice against actual GORILLA, then the maintained profile families below;
#192–#198 remain open. The current runtime/platform gates are in
[Fo's handoff](../PLAN.md#current-delivery).

## Observable compatibility

- Preserve explicit project presets/profiles, generators/configurations,
  compilers/toolchains, cache choices, environment and mixed-language behavior.
- Handle the required dependencies, generated sources/headers/version data,
  custom targets/commands, selected targets and installation behavior faithfully.
- Select actual registered test names and scope; large/punctuated/Unicode names,
  ambiguous names and unknown requests retain truthful handling.
- Preserve test cwd/environment, fixture setup/requirements/cleanup, dependency
  ordering, serial/resource constraints, processors and supported launchers.
- Preserve reference verdicts: regex rules, WILL_FAIL, skip/disabled/missing
  files, timeouts and fixture failures cannot be replaced by raw exit codes.
  Native skips complete coverage and gate obligations as distinct skipped counts,
  never as passes, and do not clear a failure or supply a reusable PASS. A
  nonempty set of only skips is not green, since at least one case must pass.
- Preserve last-compilable testing during candidate failure, incremental reuse,
  exact generation evidence, completed receipts and unrelated active work.
- Expose partial/interrupted results and output fidelity honestly. Build-only
  profiles, external checks and unavailable resources are explicit dispositions.

## Required profile families

| Project | Compatibility goal |
| --- | --- |
| SIMPLE | Actual build modes, deterministic test/golden scopes, Python API/data generation, dependency pins and required CPU/accelerator options |
| libneo | Standalone/embedded visibility, numerics/JOREK, compiler/OpenMP/OpenACC profiles, Python generation and numerical/data dependencies |
| NEO-2 | QL/PAR, MPI/MPE, compiler/debug/coverage profiles, configured data fixtures and separate golden-record procedure |
| GORILLA/GORILLA_APPLETS | Numerical/data/OpenMP dependencies, coverage/pFUnit, native/Python tests and sibling/per-case input layout |
| NEO-RT | Standalone/embedded dependency selection, numerical providers, output paths and normal/diagnostic targets |
| KAMEL | Mixed-language subprojects, platform/compiler/OpenMP choices, dependency generation, integration tests and install layout |
| MEPHIT | Presets, custom outputs, external projects, numerical providers and actual test/build-only capabilities |
| vmecpp | CXX-only configuration, current launcher, Python bindings, generated FFTX inputs and required optional profiles |
| PARVMEC/LIBSTELL superbuild | External source roots, imported/interface dependencies, preprocessing and custom outputs |

Record the actually maintained configurations and their exclusions/resources
before declaring group-wide support. A feature disabled to obtain green is an
unverified feature. No universal slow-label exclusion or replacement floating-
point policy may change scientific meaning.

## Goal ownership

- #192: standalone compatibility destination and supported subset.
- #193: faithful project configuration across interfaces.
- #194: faithful selected-test/fixture/resource behavior.
- #195: truthful results and recoverable interrupted evidence.
- #196: inexpensive incremental candidate builds with retained active results.
- #197: relevant edits and conservative affected-test selection.
- #198: actual project/profile parity evidence.
- #199: optional safe shared-result reuse.

These goals may be consolidated or delivered in different internal slices.
No child is a prerequisite for the initial Fo resident loop merely because it
appears here. Large project/performance audits remain independent #145/#190 work.

## Verification and governance

Use small independent public fixtures for each changed behavior and a pinned
reference workflow for compatibility claims. Final native support also runs with
CMake/CTest absent. Reference metadata is evidence, not a substitute for actual
configuration/build/test semantics.

SIMPLE make-test/golden and NEO-2 golden-record approval retain their distinct
requirements. CTest green alone is not their scientific approval. Do not modify
scientific branches, reference data, required reviews/CI or protected hosts as
part of this tool work. Required hardware/data checks need their explicitly
authorized isolated resources; unrun rows remain visible.

Earlier investigations and profile details remain in
[the pre-revision plan](https://github.com/lazy-fortran/fo/blob/f74daf7/doc/CMAKE_GREMLIN_PLAN.md).
Use those as evidence, not fixed architecture instructions.

## CMake-only resident capture blocker

The 2026-10-05 [minimal reproducer](reproducers/cmake-resident-input/README.md)
records the original FPM-manifest parse failure. The delegated resident route
now selects CMake before capture, freezes configuration and declared sources,
and discovers and executes actual registered CTest cases. This remains a route
through installed CMake/CTest, not standalone compatibility.

The supported closure includes project presets, explicit cache overrides and
FetchContent source directories. `FO_CMAKE_BUILD_TARGETS` selects native build
targets while `--target` selects registered test names; neither changes native
project flags. Declared prebuilt `*_BUILD` providers capture their include/lib
inputs and real Git HEAD provenance for the observed CMakeCache source probe.
Explicit target include directories, including system and virtual-environment
headers, are captured and fingerprinted.
Absent include-search directories under ordinary captured project subtrees
remain absent in frozen views; their later creation changes the generation.
Accepted search paths are rechecked without following links during rediscovery.
Missing referenced files, directory symlinks and undeclared external roots
retain their strict input checks. Frozen cache hints retain internal
find-package header paths. Original header trees must also remain unchanged
through configuration and building: project code can force absolute SDK paths
despite those hints. Changed trees fail explicitly, including on a cache hit.
Authored configure inputs also declare external CMake module directories.
Sibling module trees and explicitly declared source roots retain
their parent-relative layout, so a standalone subproject can include `../cmake`
and compile declared shared `../src` files from its frozen view. Module trees are
fingerprinted and revalidated before configuration, building and cached test
reuse, including when project code forces an absolute module path. Module
directories containing the project itself remain unsupported.
Private invocation copies retain authored file modes independently of the
read-only stored bundle, so copied case fixtures can be edited in owned scratch.
Repository-root projects also retain shallow, self-contained Git metadata so
configure-time and runtime revision/dirty-state queries use the frozen sources.
File API references to regular inputs or include subtrees beneath excluded
project directories, such as NumPy headers in `.venv`, capture those inputs
without capturing the whole environment. External header subtrees can use an
explicit cached `*_SOURCE_DIR` declaration. Also set the public include-path
cache variable used by the finder (for example `Python_INCLUDE_DIR`) so a
relocated configure uses the frozen headers rather than rediscovering them.
Arbitrary provider protocols, uncaptured external authored inputs, metadata
capacity overflow and unsupported referenced symlinks fail explicitly.

Valid internal relative source links retain their literal targets. Unused
invalid aliases are omitted from CMake input enumeration; every authored File
API path must still belong to the frozen inventory. FPM enumeration retains
its existing strict policy. Test discovery does not invent tests for build-only
projects, and affected CMake selections conservatively include all registered
cases. CTest owns fixtures, cwd, environment, resource rules and verdicts.

`test_gremlin_cmake` checks preset/build-target preservation, source/provider
edit wakeup, fixture/cwd behavior, last-compilable retention, unused bad aliases
and explicit rejection of referenced external aliases. Actual project/profile
receipts remain necessary before claiming compatibility for another consumer.
