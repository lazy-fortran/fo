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
