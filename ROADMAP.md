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

## Later

- Remote content-addressed cache transport.
- Additional compiler backends after their module-file behavior has independent
  correctness tests.
- Richer editor diagnostics over the existing MCP and LSP surfaces.

Capsules, CRIU checkpoints, Nix-like generations, web and voice surfaces, and
an operating-system layer are not implemented by this repository. The retired
design is identified as historical material in `doc/LINUX.md`.

## Cross-repository handoff (2026-08-03)

The current documentation commit is `989f16a`; the implementation baseline
for the handoff is `af075f4`.

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
