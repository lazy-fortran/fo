# Historical Linux architecture

This file previously described an unimplemented logos operating-system,
capsule, checkpoint, CRIU, Nix-store, BLAKE3, FastCDC, web, and voice design as
if it were fo's current architecture. Those components are not present in this
repository and are not part of the fo command contract.

The implemented architecture is documented in [FO.md](FO.md): a portable
Fortran build and test driver with a SHA-256 content-addressed cache, a native
OpenMP scheduler, compact CLI and MCP diagnostics, and direct CMake support.

Historical design discussion remains available in Git history.
