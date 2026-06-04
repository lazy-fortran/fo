# fo on Linux: native-performance architecture

fo does not need an OS. Every pillar in the logos design (the store, the
capsules, the checkpoint, the economy, the memory, the surfaces, the
parallelism) maps onto Linux kernel primitives that match or exceed what a
custom single-level store can deliver. This document specifies how.

The argument for logos was three features that "require" owning the hardware:
transparent whole-system checkpoint, byte-reproducible execution, and the
self-modifying development loop. None of the three actually requires a custom
OS. Linux provides the mechanisms; fo provides the policy.

## Consequence

fo targets Linux as its production platform, not as a stopgap before logos.
fo stays in this repository beside the compiler chain it drives; the two are
tightly coupled through the Fortran module DAG and `.mod` file format. The
logos OS work (`bootstrap/os/`) becomes optional: a research target for the
Smalltalk-image aesthetic, not a prerequisite for the store, the economy, or
the agent loop.

## Reuse vs. build

fo reuses mature infrastructure where it exists and builds only what no
existing tool provides.

Reuse:

- **Nix store** (or a Nix-compatible content-addressed layout): 20 years of
  production hardening on closures, gc, and reproducible builds. fo's
  `fo.lock` is a flake lock by another name.
- **CRIU**: process checkpoint and restore. fo wraps CRIU for arbitrary
  process trees and feeds page images into the store for incremental dedup.
- **git**: source history. fo reads the working tree that git manages.
- **fpm**: Fortran package manifest and build driver. fo wraps fpm.
- **Slurm/HTCondor**: cluster job scheduling. fo submits capsules as jobs.
  (Their "checkpoint" support is a trigger for CRIU, not a store-integrated
  incremental mechanism. The store integration is fo's work.)

fo must build:

- **Fortran `.mod` DAG and content-addressed cache.** No existing compilation
  cache (ccache, sccache) supports Fortran. gfortran emits `.mod` files
  alongside `.o` files; a `use other_module` depends on `other_module.mod`,
  a binary format neither ccache nor sccache understands. The cache key is
  `hash(source + flags + compiler + all upstream .mod hashes)`. This DAG
  resolution is Fortran-specific.
- **Affected-test selection.** Which tests' dependency closures include the
  edited module? Rerun exactly those. No existing tool does this for Fortran.
- **Checkpoint-to-store integration.** CRIU dumps raw process images. fo
  chunks them (FastCDC), hashes them (BLAKE3), deduplicates them into the
  store, and manages the incremental chain. This integration layer does not
  exist.
- **Capsule lineage and the economy.** The provenance graph (which capsule's
  output is which capsule's input) and the keep-vs-regenerate cost model. Nix
  has derivations but not scientific-run lineage or a cost model.
- **inotify watch loop.** Edit, recompile the changed module, rerun affected
  tests, report. The tight inner loop for the AI agent.

## Content-addressed store

The store is fo's foundation. It holds source, build artifacts, capsule
outputs, checkpoint pages, and scientific data as content-addressed objects.

Chunking: FastCDC (Xia et al., IEEE TPDS 2020) delivers 3-12x over Rabin-based
CDC at equal dedup ratios. VectorCDC (USENIX FAST 2025) pushes to 6.5-29.9
GB/s on AVX-512 by vectorizing the boundary decision with SIMD. Chunking, not
hashing, is now the pipeline bottleneck: xxHash3 runs at 31.5 GB/s, SHA-256 at
0.3 GB/s, BLAKE3 (tree-parallel) at multi-GB/s on a single core.

fo uses BLAKE3 for content identity (cryptographic, tree-parallel, fast enough
that chunking dominates) and FastCDC or VectorCDC for boundary detection. The
warm index is an in-RAM hash table; the cold tier is append-only object packs
with object-skip dedup. The store is a library, not a daemon.

Backing storage: `mmap` with `MAP_POPULATE | MAP_HUGETLB` for the warm index.
Background drain to NVMe via `io_uring` with registered buffers and fixed
files. io_uring yields 2x over synchronous I/O only with architectural
commitment (fiber-based submission, batching, ring-per-thread; Jasny et al.,
2024), so fo's store layer is async-native from the start, not a sync design
with io_uring bolted on.

## RAM-first tiering

The logos design calls for RAM-first storage: all I/O hits memory first, spills
to disk in the background. Linux delivers this without owning the MMU.

Hot tier: `mmap` with `MADV_HUGEPAGE` for transparent 2 MB pages. Explicit 1 GB
huge pages via `hugetlbfs` for the simulation working set when the data is
large and long-lived. `memfd_create` for anonymous memory-backed objects that
never touch a filesystem path. `MAP_POPULATE` to prefault pages and avoid
minor-fault overhead on first access.

Growable heap: `mremap` grows a mapping without copying. The current F2
runtime's 8 MB fixed slab (#938) becomes an `mmap` region that `mremap` extends
on demand. No ceiling, no fragmentation from repeated alloc/free.

NUMA: `mbind` and `set_mempolicy` pin allocations to the local node.
Structure-of-arrays layout (leftmost index innermost) means the prefetcher sees
unit stride per NUMA domain. `numactl --localalloc` for the simple case;
explicit `mbind(MPOL_BIND)` per array for the simulation allocator.

Tiering policy: a background thread drains cold objects from RAM to NVMe,
guided by access-time metadata in the warm index. The drain path uses
`io_uring` multishot write with registered buffers. The kernel's own page
cache handles read-back; `MADV_WILLNEED` prefetches objects the DAG resolver
knows it will need before the compiler asks.

## Checkpoint and restore

The logos design's single-level store depends on transparent checkpoint of
every running process. CRIU does this on Linux today. The gap is not mechanism
but integration with the content-addressed store.

CRIU checkpoints and restores entire process trees, including memory mappings,
file descriptors, signal state, and TCP connections. Google uses it for
container live migration in Borg. It works across hosts when network state is
managed.

fo's checkpoint integrates CRIU with the store:

1. `userfaultfd` with `UFFD_FEATURE_WP_ASYNC` tracks dirty pages between
   checkpoints. The kernel auto-resolves write-protection faults without
   generating messages; fo reads the dirty bitmap from `/proc/pid/pagemap`
   at checkpoint time. No page-fault overhead on the hot path.
2. Dirty pages are chunked (FastCDC), hashed (BLAKE3), and inserted into the
   store. Unchanged chunks are references, not writes.
3. The checkpoint object records the page table, register state, fd table, and
   the set of chunk references. Restore maps the chunks back, calls
   `UFFDIO_COPY` to populate pages, and resumes the process.
4. Incremental checkpoints are small: only the dirty fraction since the last
   checkpoint. The Young-Daly interval applies as in the logos design.

For Fortran processes that fo launched: fo owns the address space layout and can
checkpoint without CRIU, using `process_vm_readv` on the child plus the dirty
bitmap. This is lighter weight and avoids CRIU's dependency tree.

For arbitrary processes (an MPI job, a Python script): CRIU handles the full
process tree. fo wraps CRIU, feeds the page images into the store, and manages
the incremental chain.

The result is the same as the logos single-level store: a content-addressed
image of the running system, resumable on the same or a different host.

## Reproducible execution

The logos design claims byte-identical execution requires owning the hardware.
It does not. Reproducibility is a property of controlled inputs, not of
hardware ownership.

Build reproducibility: 95% of Debian's 30,000+ packages build reproducibly
(Lamb and Zacchiroli, IEEE Software 2022, Best Paper). The techniques are
systematic, not exotic: `SOURCE_DATE_EPOCH` for timestamps, `-ffile-prefix-map`
for build paths, `disorderfs` (a FUSE shim) for directory ordering. fo applies
all three by default for every build.

Compiler reproducibility: same compiler version + same flags + same source +
same dependency hashes = same output. This is the capsule contract. fo records
all four in the capsule object and verifies the hash on rebuild. gfortran with
`-ffile-prefix-map` and `SOURCE_DATE_EPOCH` produces identical output across
hosts.

Runtime reproducibility for scientific computation: the hard cases are floating
point (operator reordering under `-O2+`) and threading (reduction order). fo
pins: `-ffp-contract=off` and explicit reduction order for FP; deterministic
thread scheduling via `sched_setaffinity` + `SCHED_FIFO` + cgroups v2
`cpuset` for threads. For simulations that need bit-identical replay, `rr`
(Mozilla's record-replay debugger) records a full execution trace and replays
it deterministically, including thread interleavings.

ASLR: `personality(ADDR_NO_RANDOMIZE)` per process, or
`setarch $(uname -m) -R` as a wrapper. Reproducible address layout without
a kernel rebuild.

The trusted bootstrap chain already validates under QEMU TCG on Linux. That
does not change. The new claim is stronger: fo capsules are reproducible on
bare Linux without QEMU, given controlled inputs.

## Namespaces

The logos design specifies Plan9-style namespaces: per-process views over
tapes, objects, and resources. Linux mount namespaces are explicitly modeled on
Plan9 namespaces and provide the same capability.

`unshare(CLONE_NEWNS)` gives a process its own mount table. Bind mounts
project any directory subtree onto any mount point. `overlayfs` layers a
writable upper dir over a read-only lower (the store's immutable objects).
User namespaces (`CLONE_NEWUSER`) make all of this unprivileged.

fo creates a mount namespace per capsule run: the capsule's input closure is
bind-mounted read-only; the output directory is a fresh writable layer; the
store is visible but immutable. The process sees exactly its declared inputs.
No stray dependencies, no ambient authority.

FUSE is available but not the default path. FUSE overhead is small for
sequential data (within 2% of ext4 with writeback cache), but metadata
operations degrade by 50-83% (Vangoor et al., ACM TOS 2019). Kernel 6.9+
adds FUSE passthrough mode for data-plane bypass. For fo's store, the native
mmap path is faster; FUSE is reserved for external filesystem interop (NFS,
S3, SSHFS).

## Sandboxing and trust boundaries

The logos design confines `iso_c_binding` to the HAL, keeping the Fortran
source above the line auditable. On Linux, the trust boundary is enforced by
the kernel, not by language convention.

Landlock LSM (Linux 5.13+) restricts filesystem access, TCP bind/connect,
signals, and UNIX sockets per process, unprivileged, stackable up to 16
layers. Every enforced layer must grant access; the intersection is the
effective policy. Landlock requires only `prctl(PR_SET_NO_NEW_PRIVS)`, no
root, no capabilities.

seccomp-bpf filters syscalls. A capsule run gets a whitelist: read, write,
mmap, mremap, brk, exit_group, and the computation syscalls. No execve, no
network, no device access. The filter is inherited by children.

Unprivileged user namespaces (`CLONE_NEWUSER`) drop all capabilities inside
the namespace. Combined with Landlock and seccomp, a capsule run has no more
authority than a logos `--mode=standard` process: it can compute, read its
declared inputs, and write its declared outputs. Nothing else.

The trusted boundary is: fo (the build driver) is trusted. Capsule payloads
are untrusted. The kernel enforces the boundary. No custom OS required.

## Build performance

The logos design's priority 0 is compile speed: sub-second edit-compile-test
for the AI agent loop.

Content-addressed caching: fo hashes `source + flags + compiler_version +
dependency_mod_hashes` to a store key. Cache hit skips compilation entirely.
<<<<<<< HEAD
<<<<<<< HEAD
ccache and sccache use the same content-addressed model for C/C++/Rust but do
not support Fortran: they cannot track `.mod` file dependencies. fo builds
this cache from scratch, unified with the Nix-compatible object store.
=======
This is the same model as ccache/sccache but unified with the object store
rather than a side cache.
>>>>>>> cc74e02 (docs(fo): fo on Linux via kernel primitives, no custom OS needed)
=======
ccache and sccache use the same content-addressed model for C/C++/Rust but do
not support Fortran: they cannot track `.mod` file dependencies. fo builds
this cache from scratch, unified with the Nix-compatible object store.
>>>>>>> 590011e (docs(fo): reuse Nix/CRIU/git, build Fortran .mod cache and checkpoint integration)

Module DAG parallelism: independent modules compile in parallel. fo resolves
the module dependency graph from `use` statements (a DAG walk, not a full
parse), schedules compilation with maximum parallelism, and links. Ninja's
approach: build graph is a DAG, the executor saturates cores, and file
timestamps are supplemented by content hashes for correctness.

File watching: `inotify` on source directories detects edits instantly. fo
recomputes the affected module set (reverse-dependents in the DAG) and
recompiles only those. Affected-test selection follows the same graph: only
tests whose dependency closure includes the edited module are re-run.

Distributed compilation: distcc pump mode on a local cluster achieves 10x
elapsed-time speedup on large builds (Matev, CHEP 2019: 13 min to 90 s on 80
cores). fo's cache-first model reduces the need: most rebuilds are incremental
and hit cache. distcc is the escape valve for clean builds and CI.

Measured baseline from the interim `fo.py`: gfortran rebuilds F2 (75 fixtures)
in under a second on the host. The Fortran-native fo at F5 will not be slower.

## Parallelism

The logos design maps parallelism to structures: arrays get data parallelism,
trees and graphs get task parallelism, hashes get conflict-free concurrency.

On Linux, all three are native:

Array data parallelism: `do concurrent` (Fortran 2008), OpenMP SIMD, coarray
Fortran over shared memory (GCC's `libcaf_single` and OpenCoarrays for
multi-image). gfortran `-O3 -march=native` auto-vectorizes dense loops.
`-fopenmp-simd` enables explicit SIMD directives without the OpenMP threading
runtime. For GPU offload, `do concurrent` on NVIDIA via nvfortran/flang, or
OpenACC.

Task parallelism: POSIX threads for the build driver and store. Fortran 2018
`event` and `co_broadcast` for coarray synchronization. Structured concurrency
for the agent loop: parent tasks own child lifetimes, cancellation propagates
down.

Conflict-free hash concurrency: content-addressed writes are idempotent. Two
threads writing the same chunk produce the same blob id. The warm index uses
a concurrent hash map (lock-free or sharded); insertion of an existing key is
a no-op.

## Observability

eBPF provides zero-overhead tracing without instrumenting fo's source.

`uprobe` on fo's entry points (compile, link, cache_lookup, store_put) traces
the build pipeline. `kprobe` on `vfs_read`, `vfs_write`, `do_mmap` traces the
kernel-side cost. `BPF_MAP_TYPE_RINGBUF` streams events to userspace at
millions of events per second with sub-microsecond overhead. `libbpf` CO-RE
(Compile Once, Run Everywhere) makes the probes portable across kernel
versions.

fo ships a `bpftrace` script set for the build loop: time per module, cache hit
rate, store put throughput, dirty-page rate between checkpoints. The scripts
are development tools, not runtime dependencies.

## The self-modifying loop

The logos design values the Smalltalk/Oberon image: edit the compiler inside
the running system. On Linux, the loop is:

1. Edit source (in `$EDITOR`, or the agent writes to a file).
2. fo detects the edit (`inotify`), recompiles the changed module, runs
   affected tests.
3. The new compiler binary replaces the old one in the store (content-addressed;
   the old version is still reachable by its hash).
4. fo re-runs the bootstrap chain with the new compiler. If the chain is green,
   the new binary becomes the live tool.

The wall-clock time for step 2-4 is under a second for an incremental edit.
The feedback loop is tighter than any in-image REPL because the host CPU runs
at full speed, not through an interpreter or a VM monitor.

The aesthetic difference: in logos, the system image contains the compiler and
the editor and the running program in one address space. On Linux, they are
separate processes connected by the store. The functional difference is zero.
The performance difference favors Linux: the host kernel's scheduler, memory
manager, and I/O stack are decades of engineering that a fresh OS cannot match.

## AI agent feedback loop

The build loop exists for an AI agent that edits and rebuilds constantly.
The agent needs instant, compact feedback after every edit without context
window bloat. Two mechanisms deliver this on Linux without a custom shell.

Claude Code hooks: `PostToolUse` on `Edit|Write` runs `fo check
--changed-only --json` after every file edit. The result injects into the
conversation as `additionalContext`. `FileChanged` with `asyncRewake: true`
on `*.f90` files runs the same check when any source changes on disk and
re-wakes Claude with the result. No polling; the harness pushes.

MCP resource subscription: `fo mcp-server` exposes a single tool (`fo`,
dispatched by `action` argument) and a subscribable resource
(`fo://diagnostics`). When an MCP-capable agent subscribes, fo watches
files via `inotify` and pushes `notifications/resources/updated` on
diagnostic changes. The agent re-reads the resource to get the delta.

The feedback payload is constant-size: one line on green, the failing test
name and assertion on failure. The `inotify` watch, the `.mod` DAG
recomputation, and the affected-test selection all run on Linux with no
special kernel support beyond `inotify_add_watch`.

OpenAI tried MCP for Codex IDE integration and switched to a custom
JSON-RPC protocol because MCP lacked streaming progress and approval
flows. fo's MCP surface is simpler (tool returns result, resource pushes
URI) and the spec's resource subscription primitive is sufficient.

## What fo provides

Everything in the logos spec map except the boot firmware and the HAL:

```text
build cache + incremental rebuild     store + inotify + module DAG
content-addressed object store        BLAKE3 + FastCDC + mmap
capsules (reproducible runs)          store + mount namespaces + Landlock
checkpoint and restore                CRIU + userfaultfd + store
compute-vs-store economy              capsule lineage graph + cost model
RAM-first tiering                     mmap + huge pages + io_uring drain
namespaces                            mount namespaces + bind mounts
sandboxing                            Landlock + seccomp-bpf + user ns
parallelism                           coarrays + OpenMP + pthreads
memory (brain)                        Markdown vault + store
discovery tree                        balanced tree index in the store
surfaces (shell, web, voice, MCP)     terminal + WebSocket + REST + stdio
reproducibility                       SOURCE_DATE_EPOCH + capsule hashes
observability                         eBPF uprobes + bpftrace scripts
agent feedback                       inotify + hooks + MCP resource sub
```

## What logos-the-OS still provides

Three things, none of which blocks fo:

1. The Smalltalk-image aesthetic: one address space, no process boundaries,
   modify everything live. A research project, not a production requirement.
2. A minimal trusted computing base: only Fortran above the HAL line, no Linux
   kernel in the TCB. Relevant for formal verification of the bootstrap chain,
   not for daily scientific work.
3. The bare-metal audit reference: byte-identical execution under QEMU TCG
   with own firmware. fo validates the same property via capsule hashes on
   Linux.

<<<<<<< HEAD
<<<<<<< HEAD
## Implementation order

fo stays in this repository. The compiler chain, the store, and the build
driver share the Fortran `.mod` format and the content-addressed object
layout; separating them creates cross-repo friction for no gain.

The logos OS work (`bootstrap/os/`) stays as an optional long-horizon target.
`PLAN.md` and `HAL.md` continue to specify the bare-metal path.

Each step is independently useful:

1. **Nix-compatible store library.** Integrate with the Nix store layout for
   closures and gc. Add BLAKE3 hashing and FastCDC chunking for large objects.
   mmap-backed warm index.
2. **Fortran build core.** `.mod` DAG resolution, content-addressed cache,
   incremental rebuild, inotify watch, affected-test selection. Replaces the
   interim `tools/fo.py` (#904).
3. **Capsules.** Mount-namespace isolation, Landlock + seccomp sandbox,
   lineage recording, reproducibility verification. Wraps fpm for the package
   manifest.
4. **Checkpoint.** CRIU wrapper with userfaultfd dirty tracking, FastCDC +
   BLAKE3 chunking of page images, incremental store-backed chain.
5. **Surfaces.** CLI (`fo build/run/test`), MCP server, web canvas, voice
   bridge.
6. **Economy.** Cost model over the capsule lineage graph, tiering policy, gc
   over the Nix-compatible store.
=======
## Migration path
=======
## Implementation order
>>>>>>> 590011e (docs(fo): reuse Nix/CRIU/git, build Fortran .mod cache and checkpoint integration)

fo stays in this repository. The compiler chain, the store, and the build
driver share the Fortran `.mod` format and the content-addressed object
layout; separating them creates cross-repo friction for no gain.

The logos OS work (`bootstrap/os/`) stays as an optional long-horizon target.
`PLAN.md` and `HAL.md` continue to specify the bare-metal path.

Each step is independently useful:

<<<<<<< HEAD
Implementation order:

1. Store library: BLAKE3 + FastCDC chunker + warm index + mmap backing.
2. Build core: module DAG, content-addressed cache, incremental rebuild,
   inotify watch, affected-test selection.
3. Capsules: mount-namespace isolation, Landlock + seccomp sandbox, lineage
   recording, reproducibility verification.
4. Checkpoint: userfaultfd dirty tracking, incremental CRIU integration,
   store-backed page images.
5. Surfaces: CLI (`fo build/run/test`), MCP server, web canvas, voice bridge.
6. Economy: cost model over the capsule graph, tiering policy, gc.

Each step is independently useful. Step 1-2 replace the interim `fo.py`.
Step 3 is the Nix-like reproducibility layer. Step 4 is the checkpoint
innovation. Step 5-6 are the agent integration.
>>>>>>> cc74e02 (docs(fo): fo on Linux via kernel primitives, no custom OS needed)
=======
1. **Nix-compatible store library.** Integrate with the Nix store layout for
   closures and gc. Add BLAKE3 hashing and FastCDC chunking for large objects.
   mmap-backed warm index.
2. **Fortran build core.** `.mod` DAG resolution, content-addressed cache,
   incremental rebuild, inotify watch, affected-test selection. Replaces the
   interim `tools/fo.py` (#904).
3. **Capsules.** Mount-namespace isolation, Landlock + seccomp sandbox,
   lineage recording, reproducibility verification. Wraps fpm for the package
   manifest.
4. **Checkpoint.** CRIU wrapper with userfaultfd dirty tracking, FastCDC +
   BLAKE3 chunking of page images, incremental store-backed chain.
5. **Surfaces.** CLI (`fo build/run/test`), MCP server, web canvas, voice
   bridge.
6. **Economy.** Cost model over the capsule lineage graph, tiering policy, gc
   over the Nix-compatible store.
>>>>>>> 590011e (docs(fo): reuse Nix/CRIU/git, build Fortran .mod cache and checkpoint integration)

## References

Xia et al., "The Design of Fast Content-Defined Chunking for Data
Deduplication Based Storage Systems", IEEE TPDS 31(9), 2020.

Wei et al., "VectorCDC: Vectorizing Content-Defined Chunking for Fast and
Efficient Deduplication", USENIX FAST 2025 / ACM TOS 2026.

Jasny et al., "io_uring: Analysis of Mechanisms and Performance Gains in
Storage and Network Applications", arXiv:2512.04859, 2024.

Vangoor et al., "To FUSE or Not to FUSE: Performance of User-Space File
Systems", ACM TOS 15(2), 2019.

Lamb and Zacchiroli, "Reproducible Builds: Increasing the Integrity of
Software Supply Chains", IEEE Software 39(2), 2022.

Matev, "Speeding Up Software Builds with Distributed Compilation", EPJ Web
of Conferences 245, 05001, 2020.

Linux kernel documentation: userfaultfd (v6.12), Landlock LSM, io_uring,
namespaces(7), seccomp(2), cgroups(7).
