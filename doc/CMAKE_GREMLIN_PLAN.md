# CMake / CTest support for fo and Gremlin

Implementation reference updated 2026-10-04. Umbrella: [#192](https://github.com/lazy-fortran/fo/issues/192).
This is an implementation plan based on source and official-interface review,
not a claim that the new backend, project matrix or measurements already pass.
The ordinary/backend source audit used fo `7a638e8bc3066f04f9240ecf18215b81953cba92`.
Check current source before assigning work; other development continues.

## 1. Current delegated backend

Implementation is active. [PLAN.md](../PLAN.md) now targets a standalone CMake
replacement for a defined ITpPlasma subset. The sections below describe the
existing delegated backend and its compatibility/reference requirements; they
are not a documents-only phase or a restriction against implementing the native
subset. Standalone support is claimed only after native behavioral verification.

```text
ordinary fo commands             Gremlin
          |                 watch / supersede / coverage
          +------------------------+
                                   |
                    shared request/session/evidence services
                                   |
                       CMake backend context
                       /                   \
           cmake configure/build           CTest
             Ninja / Make / ...       registered test semantics
```

CMake owns its language, presets, dependencies, targets, generated sources,
compiler flags, custom commands and generator. `cmake --build` owns incremental
build execution. CTest owns test commands, fixtures, environment, resource
scheduling, timeouts and verdict classification. fo owns developer feedback,
project-input tracking, supported affected-test prioritization, generation
lifetime and durable observations through its existing services.

Delegated execution is the currently supported backend. Its documented File API
provides integration metadata. Native subset implementation must parse and
evaluate the supported semantics into fo's shared target/action graph; it must
not depend on private libCMake internals or treat compile_commands.json as a
complete build/test graph. A regex reader cannot establish compatibility.
A compiler-launcher cache remains an optional delegated-backend speedup.

No environment manager, system-toolchain capture, container service, CI/merge
controller or agent scheduler is introduced. Existing consumer Python/shell/MPI
commands remain valid upstream tests; fo's own Fortran implementation policy
must not be imposed on scientific projects.

## 2. What the current wrapper does not yet establish

At the audit checkpoint, `fo_build_backend.f90` configures `build/` with Ninja,
injects a selected Fortran compiler/flags, and uses generic CTest filters and
fixed-size selection buffers. A missing root CTestTestfile can return success.
Gremlin's `discover_campaign` explicitly rejects non-native test backends.

Keep existing CMake named-target tests and backend code as starting points, but
ordinary CMake delegation is not evidence of complete Gremlin support. In
particular the current wrapper does not represent presets/custom roots,
CTest fixture grouping, exact CTest verdicts, stable incremental generations or
all the ITpPlasma profiles below.

## 3. Delivery map

| Issue | Scope | Main dependencies |
| --- | --- | --- |
| [#193](https://github.com/lazy-fortran/fo/issues/193) | Context, presets, configure/build delegation and File API | #167 shared backend, #150 JSON |
| [#194](https://github.com/lazy-fortran/fo/issues/194) | CTest inventory, preparation, semantics, groups and resources | #193, existing selection/process APIs |
| [#195](https://github.com/lazy-fortran/fo/issues/195) | CTest results, journal import and honest interruption granularity | #194, #140/#183/#154 |
| [#196](https://github.com/lazy-fortran/fo/issues/196) | Stable incremental slots, ownership and last-compilable retention | #193/#194/#195, #166/#170 |
| [#197](https://github.com/lazy-fortran/fo/issues/197) | CMake inputs, configure freshness and conservative test impact | #193/#194, #175/#148/#189 |
| [#198](https://github.com/lazy-fortran/fo/issues/198) | ITpPlasma project/profile parity and rollout | existing #145/#190 audit infrastructure |
| [#199](https://github.com/lazy-fortran/fo/issues/199) | Optional compiler-launcher shared CAS | stable delegated path and shared action service |

The current delegated CMake work is an interim route. It may provide focused
Gremlin coverage while the earlier Fo milestones proceed, but it does not move
native CMake/CTest ahead of resident Gremlin dogfooding, Fo #200, FFC's three
current failure repairs, or standalone Fo-native FPM semantics. Once those
milestones are complete, implement and verify the required ITpPlasma CMake/CTest
profiles with the native semantic subset described in the Fo plan. Preserve the
delegated route's CMake and CTest contracts during that transition.

Within the delegated route, land a usable slice: explicit CMake context, actual
CTest inventory, one supported test invocation, truthful result, then event-driven
repetition. Unknown impact initially selects the whole configured local scope.
Do not wait for complete cross-project precision or compiler-cache integration.

Next add safe persistent isolated slots and richer groups/receipts, then narrower
impact where independently established. Project-profile work proceeds in parallel
against available capabilities. #199 does not block behavior compatibility.

Small changed-behavior tests gate fo increments. Whole scientific matrices,
performance distributions and GPU/toolchain experiments are non-blocking
scheduled/manual/explicit slow audits. An incomplete matrix cannot justify a
claim of full group support, but it does not stop unrelated locally correct fo
changes from landing on main.

## 4. Context, presets and public APIs

A typed context records source/build root, generator, configure/build/test preset,
configuration, toolchain choice, explicit arguments and environment, selected
build targets and CTest scope. Share it among ordinary CLI, Gremlin, MCP, run,
status and ownership-safe clean. Do not force a Fortran compiler on CXX-only
projects such as vmecpp.

Preserve user/preset/existing-build choices. Ninja may be the fresh default only
when no choice exists. Do not override CMAKE_Fortran_COMPILER, C/CXX flags,
MPI wrappers, CMAKE_BUILD_TYPE, scientific floating-point options, launchers or
library prefixes merely because fo has native defaults. CMake Profile/Fast/
Coverage names are project definitions, not aliases for native fo flags.

Let CMake expand preset inheritance, conditions, includes, macros and environment.
Build and test preset environment can differ from configure environment. Preserve
local CMakeUserPresets without publishing its secrets. Source-relative presets
naturally resolve inside an owned slot; unsafe fixed absolute binary directories
need an explicit safe override/preset or a disclosed fallback. Do not silently
rewrite user files or copy a cache to another location.

Use vector arguments and dynamic storage, with explicit unsupported/capacity
errors rather than truncation. Support source/build paths with spaces, custom
output layouts and single/multi-config selection. Resolve runnable artifacts
from the selected configuration, not newest-file searches. Do not automatically
install, update a user's toolchain or remove an unowned build directory.

Query `cmake -E capabilities`. Place client-owned File API requests under
`<build>/.cmake/api/v1/query/client-fo/`. Negotiate codemodel v2, cmakeFiles v1,
cache v2 and toolchains v1 where present. Follow the successful reply index and
its references; retry boundedly if regeneration removes a reply while reading.
An old successful index does not validate a failed new configure. CMake owns
reply files; fo must not delete them. Metadata capabilities are versioned.

| Public interface | Capability treatment |
| --- | --- |
| File API / CTest JSON inventory | Available from the 3.14 generation; negotiate exact objects/schema |
| CTest JUnit output | 3.21+; older compatible result path may use supported Test.xml |
| CTest exact names file | 3.29+; validate names because unknown lines are ignored |
| Test-preset test-directory override | Version-sensitive; 3.30+ supports the documented override |
| CMake instrumentation | Recent optional versioned API; telemetry first, not raw-exit PASS evidence |
| CMake 4.4 Ninja test_prep targets | Optional build preparation; older generators/versions keep a fallback |

The group uses several 3.22/3.24 project floors and some older projects. Do not
require the latest CMake everywhere. Unsupported enhancements reduce a capability,
not basic correct delegation. Imported/interface target detail varies by
codemodel minor version; newer abstract-target data does not turn the File API
into a complete runtime or arbitrary-custom-command graph.

## 5. Incremental build: persistent paths, not a new build tree per edit

CMake/Ninja/Make already decide which actions need rebuilding. Preserve their
build trees and dependency metadata. Do not clean, touch all source files or
configure unconditionally on every edit/status request.

For the isolated Gremlin route, start with two owned persistent slots:

```text
slot A: fixed source-A / build-A     active generation G1; CTest leased
slot B: fixed source-B / build-B     candidate G2; incremental build

G2 fails:  keep A; no tests for failed B
G2 builds: publish B; preempt/reap A's obsolete work; reclaim A only when safe
next edit: reuse compatible inactive A rather than create a third cold universe
```

Each slot is configured independently at its stable paths. CMakeCache.txt,
build.ninja and generator state are not assumed relocatable. Sync changed
captured project inputs into an inactive slot while preserving unchanged
contents/mtimes; same-size/restored-mtime content changes must still be made
visible to the generator. Track removals and external logical source roots.

The first use of each slot is cold. Later builds reuse that slot's own object,
module, depfile and link history; optional compiler caching can share more.
No universal subsecond or compile-count promise is made for arbitrary projects:
always-run custom targets and broad legitimate graph invalidation may cost more.
Use small deterministic fixtures to establish no-op/relevant-change behavior and
measure real project costs separately.

A slot is not modifiable by a competing generation while processes lease it.
CTest fixture preparation may legitimately build another target of the *same*
generation. Avoid a fo lock held across CTest that deadlocks its nested
`cmake --build`; use outer slot ownership to exclude unrelated writers instead.
A user running raw Ninja does not participate in fo locks. Dedicated owned slots
are default; attachment to an existing user build tree is explicit and cannot
claim protection against uncooperative writers.

When no slot is available, coalesce to the newest candidate or wait for owned
release, never overwrite active output or grow unbounded directories. Integrate
with #168/fx leases and cleanup, not a second CMake collector. Preserve complete
project-built executable/library/fixture companions, without capturing the host
OS/runtime environment.

### Source-generating projects

Immutable manifests/pristine source captures and writable working views are
separate. libneo's golden setup writes generated Fortran into its source tree
and builds an EXCLUDE_FROM_ALL target. A blanket read-only directory or random
per-test cwd breaks it. Preserve its declared generated paths and CTest-managed
layout inside an owned view; never modify the user's checkout.

Late generated/reference inputs must be recorded with their observed identity
before granting dependent verification, or explicitly reported as untracked.
Do not mix outputs from a changing reference under one supposedly fixed test key.
CTest fixture group ownership is the isolation unit where tests intentionally
share prepared data. Opaque captured-input mutation or hard-coded external
writes can require serialization/a separate group view or remain unsupported.

### Serialized backend execution

A serialized watch/configure/build/CTest route can ship before complete slots.
It stops/reaps testing before mutating its owned build tree. Its status explicitly
reports no overlapping candidate build/last-compilable retention. If inputs can
change during a run, freshness invalidation prevents that result certifying the
new generation. A request requiring isolated last-good behavior fails clearly
instead of silently downgrading. Such a route remains useful, but is not called
full generation-isolation support.

## 6. CTest stays the test engine

Discover actual tests with the selected CTest scope/configuration/preset and
`--show-only=json-v1`, after required build or dynamic discovery. A registered
test name is not its executable or target name. Some test registration happens
at build/test time; rediscover and update requirements rather than freeze an
incomplete configure-only inventory.

Select exact known IDs. --tests-from-file is a set, not an order guarantee, and
unknown names must be rejected by fo before invoking CTest. Older versions use a
verified escaped anchored selector. Scope-qualified duplicates need supported
exact routing or an error; never silently select unrelated cases. Zero registered
tests means build-only/no-test evidence, not fully verified.

Preserve all behavior through CTest: working directory, environment modifications,
PASS/FAIL regex, WILL_FAIL, SKIP rules, DISABLED/REQUIRED_FILES, timeouts/signals,
labels, configuration, MPI/emulator/launcher commands, fixtures and resources.
Native fo CPU budgets must not overwrite CTest wall-time semantics by accident.

A fixture-required selected test automatically brings setup/cleanup according to
CTest's rules. DEPENDS is different: it orders tests present, not automatic
build preparation or a guarantee that the prerequisite passed. Gremlin groups
related tests conservatively where needed and records any deliberate selection
widening. Project compatibility means preserving the reference scope, not
inventing new dependency meaning.

Use one CTest coordinator per owned slot/group to retain RUN_SERIAL, PROCESSORS,
RESOURCE_LOCK and RESOURCE_GROUPS behavior. Multiple ctest processes do not share
one global resource lock namespace. Cross-lane shared hardware/scratch requires
existing outer resource leases or serialization. Local MPI process ownership is
supported only as the launcher contract allows; remote ranks/Slurm need external
cancellation support/permission or a clear capability limit. No cluster scheduler.

Gremlin prioritizes unresolved failures, affected/mandatory groups and remaining
finite coverage through #138. Actual CTest order respects its constraints. Needed
fixture repeats are separate from unique primary-case epoch credit. Changing
selection must not silently remove slow scientific correctness requirements.

Do not impose the current universal regression/slow/performance exclusion. For
SIMPLE, make test intentionally enables deterministic FP and excludes regression;
make test-golden supplies a specific reference and different Python settings.
Those are distinct explicit profiles. A fast profile is allowed but must identify
what it does not verify. fo does not reset golden records to make a profile pass.

Default `all` can omit EXCLUDE_FROM_ALL tests. Preserve setup build commands or
map verified target dependencies. CMake 4.4 Ninja can optionally expose
`test_prep/<name>` via CMAKE_TEST_BUILD_DEPENDS, including target references and
explicit BUILD_DEPENDS. Not every test name/generator/version supports it; keep
older-version/default-preparation fallbacks. Do not guess preparation from names.

## 7. Results, cancellation and the unavoidable reporting boundary

Import CTest's structured results into the existing journal/coverage services.
JUnit/Test.xml carries CTest's classification; raw process exit is insufficient
for WILL_FAIL, regex failures and skips. Preserve generation, config, scope,
selected inventory, group identity, actual outcomes and output references.
Malformed/truncated reports, absent results and unsupported details remain
explicit. CTest output may be merged/truncated by upstream settings; do not claim
lossless separate streams where they were not recorded.

Do not assume one long CTest invocation flushes a final authoritative JUnit result
after every case. First use singletons/small fixture-closed groups, commit their
valid terminal report into #140 before the next group and reconcile #183
idempotently. On cancellation retain already committed groups. An incomplete
current group without verified terminal data remains unknown/cancelled; console
markers cannot manufacture PASS.

Expose `receipt_granularity` and supported crash durability. The native backend's
per-case guarantee is not silently weakened; an explicit requirement unavailable
on the selected CTest route fails capability validation. A killed CTest may not
have executed cleanup fixtures: quarantine uncertain state until owned recovery.

Recent CMake instrumentation provides per-command/test snippets and optional
output. Its command exit field is not automatically the final CTest verdict;
verify property-aware classification, publication and collection behavior before
using it for credit. It can initially supply optional counters only. Do not
fork CTest or build an interception framework to hide this limitation.

Respect stopOnFailure and skipped/disabled counts. An accepted upstream suite
policy is distinct from every required case actually passing. Reproduction uses
explicit recorded case IDs/configuration, not mutable --rerun-failed history.
Never union different generations into a full green. CLI/MCP use the same facts.

## 8. Inputs and affected-test selection

#197 adapts #175/#189, rather than creating a CMake-only policy. Combine configured
sources/targets, project CMake/configure inputs, presets, known compiler include
relations and declared test scripts/data/reference roots. Use baseline/candidate
content and configuration differences, not post-build cache misses.

Fortran .mod compilation edges and link/test implementation edges differ. Python,
MPI and compiler-subprocess tests can use an executable with no Fortran USE edge.
File API link fragments, imported targets and arbitrary custom commands can be
incomplete. Optional small project hints can narrow known relations; otherwise
run the whole configured local scope and expose the widening reason.

Watch relevant directories for additions, removals, GLOB changes and external
source root replacement. Generated outputs are excluded by ownership/role, not
blanket path guesses. Configure/regeneration changes refresh the model and revoke
old gate requirements. Unknown input does not mean unaffected. Runtime coverage
may rank cases later; it is not a proof that exclusions are safe.

### Git metadata exception

The previous generic rule that metadata-only Git commits never affect execution
has a project-dependent precondition. SIMPLE runs git describe --tags --dirty
--always to generate compiled version.f90. There, the derived Git version is an
execution input. Preserve its value in private sources through a supported
project override or verified controlled Git arrangement, and watch relevant
changes. Do not replace it with 'unknown' because a snapshot lacks .git.
For projects not consuming Git in generated code/tests, metadata stays ignorable.
This is ordinary build-input correctness, not a scientific provenance platform.

## 9. ITpPlasma source/profile inventory

These are static inspected sources, not validated build/test rows. #198 must
complete the maintained-repository inventory using repository listings as well as
code search, which may omit forks. Private identities/data never enter the public
manifest; private configurations use local-only rows.

| Public project and inspected revision | Concrete requirements to retain |
| --- | --- |
| [SIMPLE](https://github.com/itpplasma/SIMPLE/tree/212b6e6b38ab76660146b46db0e0c9845357c01e) | C/Fortran and optional CXX/CUDA; generated Git version; dependency_versions.json; GNU/NVHPC; deterministic FP, Python, OpenACC/CUDA/CGAL; project-specific Makefile test/golden scopes |
| [libneo](https://github.com/itpplasma/libneo/tree/3a520885a756651ec52344b944e6eae3b06bf795) | standalone/embedded visibility; lightweight/numerics/JOREK; Python/f2py; custom compiler profiles; fixture-driven golden target and source generation |
| [NEO-2](https://github.com/itpplasma/NEO-2/tree/77f30acf1d8157c7763d90623454cc2849deb1c4) | QL/PAR, MPI/MPE, COMMON/MyMPILib; SuiteSparse/GSL; custom Coverage; binary config inputs; TEST scope, NetCDF data/env and regex verdicts |
| [GORILLA](https://github.com/itpplasma/GORILLA/tree/78c402e58dc7665d7bb4c9c575e6a13e4dbbfcc3) | Fortran/OpenMP/NetCDF/BLAS/LAPACK; optional pFUnit coverage; registered oracle names differ from executables |
| [GORILLA_APPLETS](https://github.com/itpplasma/GORILLA_APPLETS/tree/8c0e10087f46325ae940f42767e939aa06a8deee) | native/Python tests, sibling GORILLA source, per-case environment and WORK_DIR, one script with different case parameters |
| [NEO-RT](https://github.com/itpplasma/NEO-RT/tree/84115376478208b6bb55777c92b12120f146bab2) | standalone libneo vs NEO-2; custom OBJS/output paths; spline/fortnum/fortplot; suppressed dependency tests; diagnostics |
| [KAMEL](https://github.com/itpplasma/KAMEL/tree/ae74d4c0da3a9f12ee8a117ee03150c79077bed4) | C/CXX/Fortran subprojects KiLCA/KIM/QL-Balance/PreProc; OpenMP/platform flags; dependency recipes; Python regression |
| [MEPHIT](https://github.com/itpplasma/MEPHIT/tree/e249a336af2f1b303896edeced45a6094b9df844) | presets, custom outputs/copied scripts/data, ExternalProject CODE/Triangle/MFEM, C/CXX/Fortran scientific libraries; actual registered test scope needs inventory |
| [vmecpp](https://github.com/itpplasma/vmecpp/blob/main/CMakeLists.txt) | inspected CMake file blob 9dbf919b2dca730b8fee66f860a0d67ee761355d; repository pin to resolve for audit; CXX20, ccache, pybind11, FFTX/GLOB, optional Enzyme CTest profile |
| [PARVMEC superbuild](https://github.com/itpplasma/benchmark_vmec/tree/72b3a98e006d8a0a5a26bebd5401b7e9aa138886/tools/parvmec_superbuild) | explicit external LIBSTELL/PARVMEC roots, interface targets, preprocessor flags and custom outputs; do not assume top-level benchmark repo is one CTest suite |

Further public CMake candidate roots found include spline, quadpack, vode, field,
biotsavart_vector_potential, BOOZER_MAGFIE, rabe, letter-canonical, tiago and GLISS.
Classify code/tests and draft-autodiff/tests embedded CMake separately. Inspect
forked/wrapped scientific repos such as VMEC2000, educational_VMEC, firm3d,
simsopt/NEAT and other listing entries rather than assume indexed matches are
exhaustive. Archived/non-CMake/embedded-only roots need explicit dispositions,
not invented tests. This is the first inventory task in #198.

Each audit row records exact repository/dependency/tool pins, root/subroot,
configuration/preset/options, reference make/CMake/CTest commands, env/data,
registered test IDs, setup groups, resource needs and feature outcomes. Compare
against the upstream reference workflow including its setup, numerical tolerances
and golden reference policy. Upstream build scripts are not automatically
replaced by a generic bare CTest invocation.

CPU defaults and small libraries come first; deterministic SIMPLE/NEO-2 and
libneo fixture profiles next; MPI, NVHPC/OpenACC/CUDA, optional geometry/MFEM/
Enzyme and data-heavy cases use appropriate isolated runners later. Missing
hardware/data/licenses/toolchains are BLOCKED/NOT_RUN, not PASS. A feature turned
off means a different profile, not evidence it is supported.

Full group feature parity means every maintained agreed profile works or has an
explicit outstanding blocker. It does not mean supporting every conceivable
CMake script or testing the Cartesian product of unused options. A CTest-only
run does not satisfy independent Python/golden/CI procedures outside its registry;
those can be explicit bounded existing-command oracles through the shared adapter,
not a new scientific workflow engine.

## 10. Optional compiler cache and measurements

[#199](https://github.com/lazy-fortran/fo/issues/199) uses the documented compiler
launcher interface, only for supported command/output forms. It reuses the same
fx store, does not replace CMake's DAG and preserves existing launchers unless
explicitly configured otherwise. Unknown flags/response files/module ownership/
device outputs or incomplete input keys execute the original compiler unchanged.

Fortran cache hits must restore all required .o/.mod/.smod/depfile companions
privately and preserve generator dependency semantics and diagnostics. Shared
module directories cannot be indiscriminately harvested/overwritten. Key actual
path effects where normalization is unproved. No build-cache hit is a test PASS.
This optimization never blocks basic delegated CMake or group behavior parity.

#190 extends the existing optional benchmark tool: cold slot setup, warm no-op,
real leaf/interface/header/config edits, first verdict/local gate, compiler/link
counts, storage and one/two-slot costs. Acquisition/bootstrap is separate from
edit latency. A metadata touch is not a recompilation measurement. CMake's
supported instrumentation may supply counters on recent installations without
becoming a new mandatory dependency or receipt authority.

## 11. Compatibility reporting and handoff

Expose per-context facts, using shared typed capabilities rather than many
unrelated user modes: delegated build available; actual CTest scope; named
selection support; impact precision/fallback; generation isolation; last-good
retention/overlap; receipt granularity; resource/cancellation boundaries; cache
mode; prerequisite failures. Exact field names belong to the shared API design,
not this document's pseudocode.

Do not advertise all capabilities from one boolean 'CMake supported'. Safe broad
selection is supported. Serialized operation is useful but not isolated overlap.
A build-only project is supported for building, not verified by an empty suite.

Implementers read the owning issue, PLAN.md and CLAUDE.md, select one cohesive
slice and its bounded independent fixture, reuse warm tools, then publish a
locally verified fo increment. Do not wait for CI or matrix completion. Scientific
consumer repositories keep their human approval/CI/golden gates unchanged.

## Official interface references

- [File API](https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html)
- [CMake CLI and capabilities](https://cmake.org/cmake/help/latest/manual/cmake.1.html)
- [Presets](https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html)
- [CTest CLI, JSON inventory and JUnit](https://cmake.org/cmake/help/latest/manual/ctest.1.html)
- [CTest fixtures](https://cmake.org/cmake/help/latest/prop_test/FIXTURES_REQUIRED.html)
- [CTest DEPENDS](https://cmake.org/cmake/help/latest/prop_test/DEPENDS.html)
- [Ninja generator / Fortran](https://cmake.org/cmake/help/latest/generator/Ninja.html)
- [Compiler launcher](https://cmake.org/cmake/help/latest/prop_tgt/LANG_COMPILER_LAUNCHER.html)
- [CMake 4.4 test-preparation capabilities](https://cmake.org/cmake/help/latest/release/4.4.html)
- [Versioned instrumentation](https://cmake.org/cmake/help/latest/manual/cmake-instrumentation.7.html)
