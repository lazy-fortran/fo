# fo development priorities

fo is a native Fortran build and test driver. The current priorities are
deliberately narrower than the early logos design.

## Current

- Keep common fpm project behavior compatible without adopting fpm's private
  cache metadata or verbose output.
- Build and test fpm projects through the native OpenMP DAG scheduler.
- Preserve direct CMake and CTest operation.
- Keep every warm edit, build, and test path faster than the maintained
  OpenMP fpm fork.
- Resolve Git and registry dependencies without requiring fpm at runtime.
- Carry a dependency's own build settings through to the consumer. The `link`
  list now propagates from the resolved path-dependency closure and from
  dev-dependencies, which a package with a C or C++ shim needs: fo compiles that
  shim into the same archive, so without its libraries the link fails on a
  symbol the caller never wrote. Other manifest settings have not been audited
  for the same gap.
- Complete native installation for libraries, modules, applications, and
  examples.
- Extend differential coverage over real Fortran repositories.
- [x] Centralize compiler dialect policy for the four supported native lanes:
  GNU Fortran, NVIDIA `nvfortran`, Intel LLVM `ifx`, and LLVM Flang. Module
  output, free-form, OpenMP, profile, linker, and fpm-flag translation now
  live behind one policy object. Legacy `ifort` is intentionally unsupported.
- [ ] Re-establish the nvfortran lane on the current FortML dependency graph
  and all FortML tests with nvfortran 26.5. The compiler selection remains in
  `FO_FC` (with `FC` as the standard fallback) and is passed to fpm bootstrap
  via fpm's `FPM_FC` variable; the current gate is blocked by the FortFront
  semantic-submodule link failure and FortAD's `fortad_lower.f90` ICE.
- [ ] Add an executable ifx CI/cluster gate. The ifx dialect is implemented and
  independently policy-tested, but no ifx executable is installed on the
  current cluster.

## Cross-repository open work (2026-08-06)

fo sits under every other repository here, so its defects surface as
confusing failures elsewhere. The full picture, by repository:

| Repository | Open work |
|---|---|
| fo | Cold builds now retain unparseable sources and both backend tests pass; the compiler-boundary regression is below. |
| fortfront | 15 sources still unparseable; two lexer gaps (comment inside a continuation, character literal continued across lines); 25 failing tests. |
| fortfem | PR #63 unmerged, CI red for reasons that do not reproduce under the runner's own gfortran; `main` red on a line-truncation error the branch fixes. |
| fortnum | PR #63 merged, fortad is the default engine, Enzyme demoted to test oracle. Three vector-Newton routines still Enzyme-only, blocked in fortad. |
| fortad | Forward-mode vectorisation gap; slice packing on wide operators; re-verify the vector-Newton routines now that hoisting terminates. |
| fortad-bench | Tapenade not wired in; build time unmeasured; two result caveats unresolved. |

The dependency order matters when picking work up: fo pins fortfront `main`,
so a fortfront parser gap still produces a scan diagnostic. The line scanner
now recovers that unit into the build graph, where the compiler either accepts
the legal source or reports the real error.

## Correctness debt (2026-08-05)

### Compiler dialect provenance and portability boundary (2026-08-06)

The compiler abstraction follows fpm's `compiler_t` separation of compiler
identity from compiler-specific flags, using the pinned source snapshot
`eaffbb36086abdb16c0d052961a3e7240cb22b0a` in `.provenance/upstream/fpm` as
the provenance reference. fo keeps this smaller and explicit: one
`compiler_dialect_t` owns module-directory, profile, OpenMP, linker, and
manifest-flag translation, while the native scheduler and cache remain
compiler-neutral. This prevents GNU flags such as `-J`, `-ffree-form`, and
`-funroll-loops` from leaking into nvfortran, ifx, or Flang commands.

The fo executable itself still cannot be declared a cold nvfortran build.
FortFront source units now compile with nvfortran, but its executable/link gate
has unresolved semantic-analyzer module-procedure symbols; the downstream
FortAD source transformer also triggers an `nvfortran 26.5` internal compiler
error in `fortad_lower.f90`. The earlier FortFront
`parser_expression_stacks.f90` crash was isolated and removed. The native
compiler policy and the GNU FortML path-only build/test gate pass independently.
Legacy `ifort` remains unsupported and is not interchangeable with the
supported Intel LLVM `ifx` lane.

### Unscannable sources stay in the build (fixed 2026-08-06)

Commit `f1a8e56` makes `scan_dir` recover a source that FortFront cannot parse
with the line scanner instead of removing it. The unit therefore remains in
the DAG and reaches the Fortran compiler. This preserves bootstrap behavior
for legal syntax outside FortFront's current coverage while ensuring genuinely
invalid source fails at its own compiler diagnostic.

The regression uses a module containing `integer :: value =`, verifies that
the native build fails, and requires the compiler log to name that exact source.
It fails against the parent of `f1a8e56` because the source is dropped, and
passes on current `main`. From empty build and cache directories, bare `fo`,
`test_backend`, `test_backend_gfortran`, and cold builds of FortFront and the
FortFEM FortAD integration branch all pass. FortFEM's continued-character
literal still produces a FortFront diagnostic, then is recovered and compiled.

### Test-result truncation (fixed, recorded for the lesson)

`parse_test_results` filled a fixed 256-entry array and stopped, dropping
every later result including failures. `fo test` on a 738-target project
printed `Tests: 256 passed`, exited 1, and showed nothing about what had
failed. The buffer now grows on demand.

The lesson generalises: a cap that silently discards results is worse than
no cap, because the output still looks like a complete answer. Any future
bound on reported output must say what it dropped.

## Later

- Remote content-addressed cache transport.
- Additional compiler backends after their module-file behavior has independent
  correctness tests.
- Richer editor diagnostics over the existing MCP and LSP surfaces.

Capsules, CRIU checkpoints, Nix-like generations, web and voice surfaces, and
an operating-system layer are not implemented by this repository. The retired
design is identified as historical material in `doc/LINUX.md`.

## Cross-repository handoff (2026-08-03)

The implementation baseline for the handoff is `af075f4`. The roadmap commits
are pushed on current `main`.

fo is the workflow owner for ffc's cheap build/test/lint and bounded
conformance commands. Routine compiler progress uses deterministic random
subsets, never a whole-corpus run. The sample count increases only after
repeated 100%-clean subsets. The ffc XFAIL-first gate is documented in
[ffc/ROADMAP.md](https://github.com/lazy-fortran/ffc/blob/main/ROADMAP.md).

Relevant open contracts:

- [#59](https://github.com/lazy-fortran/fo/issues/59) consumes fluff's stable
  JSON output. Fluff #262 / PR #269 must finish with honest failing-test
  behavior before this integration is complete.
- [#103](https://github.com/lazy-fortran/fo/issues/103) owns structured
  FortFront diagnostic mapping, a prerequisite for [#56](https://github.com/lazy-fortran/fo/issues/56)
  and the fx language-service path.
- [#114](https://github.com/lazy-fortran/fo/issues/114) and [#119](https://github.com/lazy-fortran/fo/issues/119)
  are correctness and observability work for failure-path analysis and
  complete machine-readable test reporting.

Every workflow change needs an independent behavioral oracle, focused tests,
`FO_JOBS=1` verification on memory-constrained hosts, and a bounded sample
when corpus behavior is involved.
