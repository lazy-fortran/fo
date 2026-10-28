# Cache, parallelism and linking

## Cache keys

A compile action is identified by the SHA-256 hash of the source content, the
compiler identity, the effective flags and the interfaces of the modules it
uses. Because module interfaces are hashed, an implementation-only change does
not recompile dependents. Links and successful test runs have their own keys.
The shared store lives under `~/.cache/fo` by default; set `FO_CACHE_DIR` to use
another location.

For the CMake backend, a compile action is keyed by:

- the content of the source and of every file its `include`, `#include "x"` or
  `#include <x>` lines resolve to (an include through a macro makes the action
  uncacheable);
- the compiler identity;
- the compile options in order, with output and module directories dropped and
  include directories replaced by their position (the files they resolve to are
  keyed by content);
- the interface keys of the modules it uses;
- for C and C++, the content of every header listed in the compiler's dependency
  file (a fresh build tree first runs the preprocessor to write that file);
- the source path passed to the compiler, since compilers embed it in the object
  (gfortran does so in runtime error messages even without `-g`).

With debug information the compile directory is part of the key, and options
such as `-march=native` add the host CPU identity. If a key cannot be complete,
the action is not cached. Compiler and system modules outside the include
directories (intrinsic modules, system `.mod` files) count as part of the
toolchain identity rather than being keyed by content.

Results are therefore tied to the project location by default. With
`FO_CACHE_RELOCATABLE=1`, sources below the project root are passed relative to
it and, with `-g`, fo adds `-fdebug-prefix-map=<root>=.` and
`-gno-record-gcc-switches`. Identical trees at different paths (worktrees, CI
checkouts) then share results and produce byte-identical objects. A renamed
file is still a different action.

## Size limits

`FO_CACHE_MAX_SIZE` caps the action store (default `10G`; suffixes `K`, `M`,
`G`, `T`; `0` disables the cap). At most every five minutes a build that
publishes results checks the store size. Above the cap it removes the least
recently used actions and unreachable objects until the store is at 80 % of the
cap. Entries used or published within the grace period (`FO_CACHE_GRACE`,
default 3600 seconds) are never removed, so the cap is soft while more than
that is in active use. Staging files left by killed builds are removed after a
day.

The source scan cache has its own cap, `FO_SCAN_CACHE_MAX_SIZE` (default `1G`,
same suffixes, `0` disables eviction), enforced the same way. Scans of CMake
sources are keyed by content after preprocessing, so changed includes still
invalidate them and relocated sources can reuse them. The cap is approximate
between sweeps; readers and evictors take no lock, and a lost entry only causes
a rescan.

The continuous mode (`fo gremlin`) keeps per-session working trees with a
separate soft budget, `FO_GREMLIN_CACHE_MAX_SIZE` (default `8G`, `0` disables
eviction). When a session is idle, fo removes the oldest inactive trees down to
80 % of the budget, sparing recent and in-use ones. `fo clean --gremlin` runs
this pass immediately. This state is host-local.

## Shared filesystems

The store can be shared between hosts, including filesystems with host-local
locks (such as BeeGFS) and NFS. Correctness does not depend on locks: objects
are published by exclusive link and verified by hash on every read, action
records are replaced by rename and reread on `ESTALE`, and eviction spares
everything inside the grace period. A concurrent eviction or a reader racing a
replacement therefore causes a cache miss and a rebuild, never a wrong result.
Keep the grace period longer than any single build step and the clocks of the
sharing hosts within a few seconds of each other.

## Parallelism

The module graph and the selected tests run in OpenMP loops. Each worker has
its own command, log and temporary-file state. Cache publication is atomic, and
a project lock protects the build tree across processes. `FO_JOBS=N` limits
parallelism; the default is the number of available CPUs.

## Compiler flags and linking

GNU Fortran builds enable `-Warray-temporaries` by default. To disable it:

```bash
fo build --flag "-Wno-array-temporaries"
```

GNU Fortran compilation uses `-pipe`. With compiler drivers that support it,
fo links with LLD when `ld.lld` is available and retries with the driver's
default linker if that fails. `FO_LINKER=default` disables LLD and
`FO_LINKER=lld` requests it. Linking always goes through the Fortran compiler
driver, so the compiler runtime and OpenMP libraries are selected correctly.
