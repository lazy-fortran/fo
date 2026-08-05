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

## Cross-repository open work (2026-08-05)

fo sits under every other repository here, so its defects surface as
confusing failures elsewhere. The full picture, by repository:

| Repository | Open work |
|---|---|
| fo | Silent source drop, below. Cannot cold-build fortfront. `test_backend` and `test_backend_gfortran` fail from a cold cache. |
| fortfront | 19 sources still unparseable; two lexer gaps (comment inside a continuation, character literal continued across lines); 25 failing tests. |
| fortfem | PR #63 unmerged, CI red for reasons that do not reproduce under the runner's own gfortran; `main` red on a line-truncation error the branch fixes. |
| fortnum | PR #63 merged, fortad is the default engine, Enzyme demoted to test oracle. Three vector-Newton routines still Enzyme-only, blocked in fortad. |
| fortad | Forward-mode vectorisation gap; slice packing on wide operators; re-verify the vector-Newton routines now that hoisting terminates. |
| fortad-bench | Tapenade not wired in; build time unmeasured; two result caveats unresolved. |

The dependency order matters when picking work up: fo pins fortfront `main`,
so a fortfront parser gap becomes an fo scan failure, and an fo scan failure
becomes a silently missing object in every downstream build. Fixing the
silent drop below makes all of these fail loudly instead, which is why it
should come first.

## Correctness debt (2026-08-05)

### A source fo cannot scan is silently dropped from the build

This is the most consequential open bug in the repository, and it is a
correctness bug rather than a convenience one.

`scan_dir` calls `scan_file` per source. When that fails it prints the
diagnostic, removes the unit from the list, and leaves `ierr` at zero. The
file then has no module name and no program name, so `build_dag_from_units`
gives it no node, so it is never compiled. The compiler never sees it and
never reports its syntax error, and the build is declared successful with
the file missing entirely.

Two files reproduce it:

```
fpm.toml                 name = "p"
src/ok.f90               a valid module
src/broken.f90           a module containing the statement `x =`
```

`fo check` prints a parse diagnostic and then reports
`Build: OK (1 modules, 0 cached, 1 changed, 1 affected)` and exits 0. Only
`ok.f90` was compiled. Delete `ok.f90` and it reports `OK (0 modules)`.

Consequences already observed:

- `fo` cannot cold-build fortfront. Each dropped source appears at link
  time as an undefined reference to a symbol it defined
  (`parse_range`, then `keyword_should_parse_as_identifier`, then
  `get_standardizer_input_mode`, one per fix), never as a parse error.
- `fo test` on fo itself fails `test_backend` and `test_backend_gfortran`
  from a cold cache, because `fo_gfortran_build.f90` is dropped and its
  symbols go undefined. Confirmed present at the commit before the
  test-result buffer fix, so it is not a regression from that work.
- A warm scan cache hides all of it, which is why this survived so long.
  Always `rm -rf build` before trusting a result here.

What was tried and reverted. Making a scan failure a hard build error is
the obvious fix and it is wrong as stated: fo then cannot build itself,
because fortfront cannot yet parse some of fo's own sources. The
distinction matters and must be preserved by any fix: a missing `app/` or
`example/` directory also returns a nonzero status from `scan_dir` and is
entirely routine, so a fix cannot simply treat any nonzero status as fatal.

The likely correct fix is to keep an unscannable unit in the build with a
node of its own, so it is still handed to the compiler and the compiler
reports the real error. That also degrades gracefully while fortfront has
parser gaps: a file fo cannot scan but gfortran can compile still builds.

This is the same family as fortfront's silent-source-drop issues (#2966,
#2967, #2972, #2974, #2977) and should be tracked with them.

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

The implementation baseline for the handoff is `af075f4`; the roadmap commits
are pushed on current `main`.

fo is the workflow owner for ffc's cheap build/test/lint and bounded
conformance commands. Routine compiler progress uses deterministic random
subsets, never a whole-corpus run; the sample count increases only after
repeated 100%-clean subsets. The ffc XFAIL-first gate is documented in
[ffc/ROADMAP.md](https://github.com/lazy-fortran/ffc/blob/main/ROADMAP.md).

Relevant open contracts:

- [#59](https://github.com/lazy-fortran/fo/issues/59) consumes fluff's stable
  JSON output; fluff #262 / PR #269 must finish with honest failing-test
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
