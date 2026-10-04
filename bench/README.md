# Optional performance audits

Benchmarks are not a development, main, merge or unrelated issue-completion gate.
Use them explicitly or in independent scheduled/manual CI, while focused
correctness testing and implementation continue. Hardware timing targets are
advisory; actual stale builds, wrong outputs or ownership failures are separate
correctness defects to reproduce and repair.

[Issue #190](https://github.com/lazy-fortran/fo/issues/190) owns the real-edit
benchmark and non-blocking workflow implementation.
[Issue #145](https://github.com/lazy-fortran/fo/issues/145) owns automatic pinned
third-party acquisition/reference comparisons. The ordinary CI workflow
excludes this harness. A separate weekly/manual workflow runs its bounded
harness checks and preserves raw JSONL evidence. This initial workflow still
exercises the existing smoke workloads; real-edit workloads and pinned project
acquisition remain follow-up work under #190/#145.

## Existing tool and important limitations

The standalone native Fortran benchmark tool currently produces eight metrics
and JSONL raw samples/exit statuses. Failed commands retain their output paths.
`report --complete` checks that fixed inventory. Invalid evidence and measured
command failures remain errors; timing target overruns print `WARN` and do not
change the exit status. The thresholds are advisory.

The `metadata_touch_leaf` and `metadata_touch_core` workloads **touch file
metadata without changing source bytes**. They measure revalidation/cache reuse,
not real recompilation. The small `bigmod` and `many_tests` fixtures are smoke
examples, not large-codebase evidence. Targets such as 100 or 200 ms are not
measured Gremlin edit-to-verdict results. #176 completed native harness migration,
not these new performance questions.

## Explicit current invocation

Use a disposable audit worktree: the existing harness touches workload files and
retains their build trees. Select an exact fo driver and record its SHA256; do
not benchmark an ambiguous installed/newest binary or overwrite a global tool.
Build the standalone tool with that driver. For example, from the disposable fo
repository root, with FO_AUDIT_DRIVER set to its verified absolute pathname:

```sh
(cd bench && "$FO_AUDIT_DRIVER" build)
audit_output="$(mktemp -d /var/tmp/fo-benchmark-report.XXXXXX)"
bench/build/fo/bin/fo-bench run \
  --fo "$FO_AUDIT_DRIVER" \
  --reps 7 \
  --output "$audit_output/results.jsonl" \
  --workloads "$PWD/bench/workloads"
bench/build/fo/bin/fo-bench report --complete "$audit_output/results.jsonl"
```

Record the report status honestly, but do not wait on it to continue unrelated
implementation. `BENCH_REPS` and `BENCH_OUTPUT` supply defaults. The current
runner disables self-refresh and creates an isolated cache below /var/tmp;
workloads retain their private build trees. Preserve failed-run logs referenced
by the report. This is a manual tool invocation, not a scheduled service.

## Planned real-edit protocol

Distinguish metadata touch, warm no-op, cold build, warm CAS with fresh outputs,
private-body edit, public-interface fan-out, a test-body edit, declared include/
C/header/path/runtime-fixture changes, broken-then-repaired candidate and return
to already cached content. Record before/after byte hashes and independently
expected executable outputs. Repeated A/B edits may already have B cached: use
fresh semantic revisions or controlled warm-baseline caches for recompilation
measurements; label deliberate rollback reuse separately.

Measure both ordinary fo and Gremlin with paired same-host/profile/input runs.
Retain raw samples, sample count/dispersion, exact driver/project/dependency/
compiler identities, effective capacity and filesystem/hardware context. Use
one monotonic clock domain for edit, debounce, capture, build-ready, first test,
first durable verdict and local-gate timestamps. Full completion cancelled by a
new generation is partial/censored evidence, not full verification.

Count compiler/link invocations, cache hits/misses, restored/materialized/stored
bytes and actually executed unique tests. A compile cache hit is not a test PASS.
Report unsupported counters as unknown. Logical/allocated/write bytes are not
SSD NAND endurance measurements. Preserve all real failures; no fabricated zero
latency for failed or unrun cases and no percentile claims from tiny samples.

## Planned third-party acquisition

A versioned allowlist such as bench/projects.toml will pin full source and
recursive dependency commits, prerequisites, reference/fo argv, test inventories,
reversible edits and resource bounds. Initial rows are ffc with matching
FortFront/liric/runtime, fortran-lang/fpm and selected package fixtures, then
stdlib's preprocessed fpm branch or explicit Fypp/CMake preparation.

Fetch only during an explicit audit or its isolated CI workflow. Reuse verified
owned downloads; never clone corpora per test, mutate user repositories, replace
tools or expose credentials. Compare upstream reference behavior first and keep
missing/unsupported/upstream-failing/fo-failing/cancelled dispositions distinct.
A reference fpm executable is separate from fpm under test. Offline replay must
report missing pins instead of silently downloading or passing.

Normal fo/test/default Gremlin commands must not acquire audit projects or enroll
performance workloads. Ordinary declared project dependency resolution remains
supported and separate. Simply naming a benchmark *_slow does not exclude it
from a full Gremlin inventory: use an explicit audit scope. Tiny deterministic
runner/schema/cleanup tests remain local correctness tests; real timings, large
matrices and network acquisition do not.

See [PLAN.md](../PLAN.md) for current implementation priorities and the shared
input/session/storage architecture; audits do not wait for that migration to
collect a baseline, and migration does not wait for audits to finish.
