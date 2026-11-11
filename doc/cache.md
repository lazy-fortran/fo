# Cache, parallelism and linking

## Cache keys

A compile action is identified by the SHA-256 hash of the source content, the
compiler identity, the effective flags and the interfaces of the modules it
uses. Because module interfaces are hashed, an implementation-only change does
not recompile dependents. Links and successful test runs have their own keys.
The shared store lives under `~/.cache/fo` by default; set `FO_CACHE_DIR` to use
another location.

For fpm projects, a compile action is keyed by:

- the content of the source and of every file its `include` or `#include` lines
  resolve to, next to the including file or in an include directory of the
  manifests (`[library] include-dir`);
- where gfortran finds an include differently, the file it reads by its path
  and content: a Fortran `include` inside an included file is looked up next
  to the compiled source, and a header found in neither place comes from the
  compiler's own search list, which includes `CPATH`;
- the name and content of `.mod` and `.smod` files lying in the directory of
  the source, which gfortran reads before those of the module directory;
- the compiler identity: its command, the path and content of the compiler
  driver and its version line;
- fo's baseline flags and the flags of the project and the command line, and
  the values of `COMPILER_PATH`, `GCC_EXEC_PREFIX` and
  `MACOSX_DEPLOYMENT_TARGET` when set;
- the host CPU identity when a flag ends in `=native`;
- the interface keys of the modules it uses;
- the source path and the module and include options as the compiler gets them;
- the project directory, unless the action is relocatable (see below).

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

## Relocatable results

Results are tied to the project location by default: the compiler gets absolute
paths and writes them into the object (`__FILE__`, run-time error messages,
debug information), so the key covers them. With `FO_CACHE_RELOCATABLE=1`,
identical trees at different paths (worktrees, CI checkouts, other machines
with the same compiler at the same path) share results and produce
byte-identical objects. A renamed file is still a different action.

For CMake projects, sources below the project root are then passed relative to
it and, with `-g`, fo adds `-fdebug-prefix-map=<root>=.` and
`-gno-record-gcc-switches`.

For fpm projects compiled with gfortran, fo then runs the compiler in the
project root with every path below the root relative to it:

```
gfortran -c ... -J build/fo/mod -I build/fo/mod -I include -o build/fo/obj/src_m.f90.o src/m.f90
```

Paths outside the root, such as a path dependency elsewhere or an external
module directory, stay absolute. The key covers these arguments as passed and
no longer the project directory. What a build then gives differs from the
default in these ways:

- `__FILE__`, run-time messages (`At line 4 of file src/m.f90`) and compiler
  diagnostics name files relative to the project root, as fpm does. A fetched
  dependency is named `build/dependencies/<name>/src/x.f90`.
- fo adds `-fdebug-prefix-map=<root>=.` and starts the compiler with `PWD` set
  to the root. Debug information then has the compile directory `.` and the
  source names relative to it, so a debugger finds the sources when it is
  started in the project root (in gdb otherwise with `directory <root>`).
- A dependency outside the project is shared by consumers at different paths
  and compiled again when it moves itself.
- The key also covers the path, relative to the root when it lies below it,
  of every file the includes resolve to, because that name is what `__FILE__`
  in the header and debug information show. With the default key, a header of
  equal content that moves to another include directory is not noticed.
- In fo's log, the output of these compiles follows a line
  `fo: compile directory: <root>`, and the diagnostics of `fo check --json`,
  the MCP tool and build errors name the files with that directory prepended.

An action keeps absolute paths and the default key, and so stays tied to its
checkout, when its result is not the same elsewhere or fo cannot tell:

- coverage and profile builds (`--coverage`, `-fprofile-*`, `-ftest-coverage`),
  whose objects write their counters to an absolute path;
- `-flto`, `-foffload` and `-gsplit-dwarf`;
- flags that read or write files the key does not cover (`-I`, `-include`,
  `-isystem`, `-specs`, `-fplugin`, `-M...`, `-fdump-...`, `-fstack-usage`,
  `-fopt-info`, `-fdiagnostics-format` and similar), every long option
  (`--...`, among them `--include-directory=DIR`) and any flag word that is not
  an option. Declare include directories in `[library] include-dir` to have
  their headers keyed by content;
- a source or include that uses `__DATE__`, `__TIME__` or `__TIMESTAMP__`,
  since no two compiles of it agree;
- a flag or path that contains the project root;
- compilers other than gfortran (flang writes the absolute source path into
  the object whatever it is passed);
- test programs, whose key also covers what they are linked with.

Switching the variable in a built tree compiles the tree again, and so does a
change of `CPATH`, `C_INCLUDE_PATH` or the compiler variables above. A test
verdict of one mode is not reused in the other, and Gremlin records these
variables in its context.

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
