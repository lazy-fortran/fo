# Native ITpPlasma compatibility goals

Fo's final CMake/CTest destination is faithful standalone execution of the
maintained ITpPlasma project/profile subset. Supported profiles build, run, test
and install without installed CMake/CTest. Unsupported constructs are explicit.
Current delegated support remains useful interim behavior and is labelled as
such; it does not count as completed native compatibility.

This is milestone 6 in [PLAN.md](../PLAN.md). It follows useful Fo/FFC Gremlin
development and standalone FPM, with only actual consumer dependencies blocking
each increment. [Architectural freedom](GOAL_DRIVEN_DEVELOPMENT.md) applies:
agents choose the implementation as requirements become concrete. No File API,
slot-pool, launcher, parser representation or module list is mandatory.

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
records an observed FPM-manifest parse failure before CMake generation capture.
Public delegated test/exec results do not establish resident support. The next
repair must handle the CMake input inventory and frozen dependency/profile
closure as well as backend selection, then recheck the original consumer.
