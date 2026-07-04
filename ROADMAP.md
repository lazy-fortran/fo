# fo Roadmap

Architectural items closed as tracking issues and moved here for reference.

## Issue #1 — Linux-native fo: build cache, checkpoint, capsule economy

fo targets Linux as its production platform. Every pillar maps onto Linux kernel primitives.

### Reuse vs. build

**Reuse:** Nix store layout, CRIU, git, fpm, Slurm/HTCondor.

**Build (no existing tool):**
- Fortran `.mod` DAG and content-addressed cache
- Affected-test selection
- Checkpoint-to-store integration (CRIU + FastCDC + BLAKE3 dedup)
- Capsule lineage and economy
- inotify watch loop

### Implementation order

1. Nix-compatible store library
2. Fortran build core (DAG, cache, incremental rebuild, inotify, affected-test selection)
3. Capsules (isolation, sandbox, lineage, reproducibility)
4. Checkpoint (CRIU wrapper + userfaultfd + store dedup)
5. Surfaces (CLI, MCP server, web canvas, voice bridge)
6. Economy (cost model, tiering, gc)

### Linux kernel mechanisms

| Pillar | Mechanism |
|--------|-----------|
| Store | Nix-compatible layout + BLAKE3 + FastCDC/VectorCDC + mmap |
| RAM-first tiering | huge pages + mremap + MADV_HUGEPAGE + io_uring drain |
| Checkpoint | CRIU + userfaultfd WP_ASYNC + store dedup |
| Reproducibility | SOURCE_DATE_EPOCH + -ffile-prefix-map + capsule hashes + personality(ADDR_NO_RANDOMIZE) |
| Namespaces | mount ns + bind mounts + overlayfs per capsule |
| Sandboxing | Landlock LSM + seccomp-bpf + unprivileged user ns |
| Build speed | Fortran .mod cache + module DAG + inotify |
| Parallelism | coarrays + do concurrent + OpenMP SIMD + pthreads |
| Observability | eBPF uprobes + bpftrace scripts |

### Research basis

FastCDC 3-12x over Rabin (Xia et al., IEEE TPDS 2020). VectorCDC 6.5-29.9 GB/s AVX-512 (USENIX FAST 2025). io_uring 2x with architectural redesign (Jasny et al., 2024). Reproducible builds: 95% of Debian (Lamb/Zacchiroli, IEEE Software 2022).

## Issue #2 — Slurm/MPI adapters for fo capsules

The capsule is the cluster scheduling unit. fo wraps Slurm and MPI and records the run into the capsule: generated Slurm script, module environment, MPI launcher, rank topology, per-rank logs, input and output hashes.

Commands: `fo submit --slurm/queue/attach/collect/repro`.

Parallelism maps to structures: arrays to data parallel, tree/graph to task/async, hash/content-addressed to conflict-free. In-node coarray/do-concurrent and async parallelism tracked separately.

## Issue #3 — fpm/Nix/Spack interop

Bridge to existing Fortran and HPC tooling. An fpm project keeps working as plain fpm; fo adds lock, store, and export on top.

Inputs: `fpm.toml`, `fo.lock`, optional `flake.nix`, optional `spack.yaml` / `spack.lock`.
Outputs: fpm-compatible build/run/test, Nix flake export, Spack environment export, capsule metadata.

Commands: `fo init --from-fpm ; fo lock ; fo build ; fo test ; fo nix export ; fo spack export`.

Non-goals: replacing fpm or its registry; requiring Nix/Spack; a new manifest format; supporting cmake.

## Issue #4 — Hierarchical balanced discovery tree

Agent-level instance of summarization for tools, skills, resources, and caps. Balanced 5-9 fanout, summaries first, schema on demand, search over enumerate.

Meta-tools `fo_list` / `fo_describe` / `fo_search` / `fo_call` / `fo_use_skill` as the only entry points. On-demand tool loading cuts tool-definition context by roughly 85% while improving selection accuracy. `fo mcp-server` exposes the same tree externally.

## Issue #5 — Snapshot, replication, backup, remote cache transport

Transport layer over the store and capsules. Push a run to a remote, delete locally, pull back, verify, reproduce.

Concepts: snapshot (named set of roots and refs), backup (remote copy of a closure), replication (push/pull/mirror of objects by hash), restore (activate old profile, snapshot, or capsule closure).

Dedup at the warm repo. Cold tier is write-only: object-skip plus immutable/WORM, append-only. Verification rehashes every object against its key. Discipline: 3-2-1-1-0.

Remotes: local directory, SSH, S3-compatible object storage.

Commands: `fo snapshot create/diff`, `fo backup create/verify/restore`, `fo remote add`, `fo push`, `fo pull`, `fo mirror`.

## Issue #6 — Computation capsules

Compute-store duality: a value held as bytes in the store or as a recipe (a capsule) in the lineage graph. The hash names both, so a stored output is a cached evaluation of its lineage.

Decision rule: store iff storage_cost_lifetime < recompute_cost x expected_reuse.

Capsule = source/toolchain/flags/inputs/command/env/resources/outputs/lineage, always kept small.

Rails: drop only verified-reproducible outputs. Never inputs, code, or capsule. Never non-deterministic or externally-sourced data. Pin overrides.

Commands: `fo capsule init/build/run/repro`, `fo sweep`, `fo compare`.
