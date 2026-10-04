# Benchmarks

Build fo from this worktree, then build the standalone Fortran benchmark tool:

```sh
fo build
(cd bench && "$PWD/../build/fo/bin/fo" build)
```

Run from the repository root with an explicit fo executable. Results are
append-free JSONL evidence; failed commands retain their combined output in the
`output_paths` field.

```sh
bench/build/fo/bin/fo-bench run \
  --fo "$PWD/build/fo/bin/fo" \
  --reps 7 \
  --output /var/tmp/fo-benchmark.jsonl \
  --workloads "$PWD/bench/workloads"
bench/build/fo/bin/fo-bench report /var/tmp/fo-benchmark.jsonl
```

`BENCH_REPS` and `BENCH_OUTPUT` provide defaults for `--reps` and `--output`.
The runner sets self-refresh off and uses a process-specific cache under
`/var/tmp`; each workload keeps its own build tree for incremental measurements.
