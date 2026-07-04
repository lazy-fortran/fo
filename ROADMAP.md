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
