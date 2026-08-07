# fo roadmap

Snapshot: 2026-08-07. fo is the native Fortran build, test, lint, dependency,
and affected-test-selection driver. It must make work and failures visible.
Speed never permits dropping a source, result, dependency edge, or diagnostic.

## Current truth

The implementation baseline is `32ef96d`; it has no completed remote check
suite yet. It adds ancestor and immediate-parent edges for nested submodules and a
child-first gfortran compile/run oracle. The last checked ancestor was
`e3cff007` in [run 31122586327](https://github.com/lazy-fortran/fo/actions/runs/31122586327),
which failed/cancelled during dependency bootstrap. The cold-scan repair keeps
a source that FortFront cannot parse in the DAG and proves at the compiler
boundary that an invalid source fails by naming that source.

Current main does not have a green remote gate:
[run 31107864451](https://github.com/lazy-fortran/fo/actions/runs/31107864451)
failed the git-dependency bootstrap test and a later run was cancelled. Restore
that exact gate before expanding the workflow.

### Current compiler handoff (2026-08-07)

The FortFront dependency used for this handoff is semantic code `c0a32743`,
with documentation head `0689f81c` (source handoff `d8c8769`, including the
#2973 legacy-I/O AST oracle). Its focused
GNU and `nvfortran` 26.5 cold-build evidence covers the 381-target lane. The
latest regression run [31147308041](https://github.com/lazy-fortran/fortfront/actions/runs/31147308041)
has a successful Ubuntu job, including the #2975 nested-associate
owner-boundary regression; Windows retains the documented nine-test
portability baseline. No aggregate FortFront PASS is claimed here.

The remote NVHPC handoff is toolchain evidence, not a green fo GPU gate:
`faepkub4:/var/tmp/ert` has verified NVHPC 23.9 and 26.5 installations with
85 GiB free; the driver-matched NVHPC 23.9 OpenACC smoke passed on the
acluster Tesla T4 (CUDA 12.2), and the scluster Slurm smoke (job 1033712)
allocated an NVIDIA RTX PRO 6000 Blackwell Max-Q and printed
`GPU_SMOKE_PASS` under driver 590.48.01. No local or remote NVIDIA compute
process was active after the smoke. This records toolchain/device availability
only and does not close fo's full multi-compiler or GPU application gates.

[#119](https://github.com/lazy-fortran/fo/issues/119) is only partly fixed. The
former 256-result array now grows, but machine-readable output can still
truncate at 16 KB. #119 closes only when every child result and the final exit
status are present.

## Immediate ffc prerequisites

The canonical compiler plan is in the
[ffc roadmap](https://github.com/lazy-fortran/ffc/blob/main/ROADMAP.md). fo owns
these prerequisites:

1. The parent/ancestor dependency repair and adversarial compile/run oracle are
   landed in `32ef96d`; ffc's 18 `_order.f90` shims were removed in `763ba0c`.
   Keep the DAG contract in the next clean gate and never use filename ordering
   as a replacement.
2. Finish #117's formatter correctness oracle before applying ffc's formatting
   sweep; then finish #119 with unbounded/streamed JSON and an oracle beyond both old caps.
   Counts, records, exit status, and human output must agree.
3. Represent each compiler test as one immutable raw observation. XFAIL/SKIP
   manifests classify it after execution. A manifest change never rebuilds or
   reruns the case.
4. Support changed-code and capability/failure-cluster selection, then
   deterministic non-overlapping duration-balanced shards whose exact union
   covers a locked corpus epoch.
5. Expose phase, normalized signature, duration, peak RSS, compiler/dependency
   closure, cache key, and selection reason in structured output.

Submodule discovery is a language rule, not an ffc filename heuristic. Missing
parents and cycles fail with diagnostics that name the missing or cyclic unit.
Parent implementation
changes must invalidate the required descendants while an unrelated module
edit must not rebuild them.

## Build and cache invariants

- Every discovered source appears exactly once in the DAG or produces a hard
  diagnostic. A fallback scanner may recover scheduling metadata but cannot
  turn compiler-invalid source into success.
- Cache keys cover source and dependency closure, exact compiler/tool binary,
  flags, target, declared environment, generated inputs, and schema version.
  Partial, timed-out, OOM, interrupted, or infrastructure-failed actions are
  not reusable.
- Module and submodule outputs are atomic. A consumer never observes a
  truncated `.mod`/`.smod` or stale output from a failed producer.
- GNU Fortran, NVIDIA `nvfortran`, Intel LLVM `ifx`, and LLVM Flang use distinct
  compiler policies. Legacy `ifort` is outside the supported lane.
- Test output is streamed or grows to input size. Any intentional display cap
  reports exactly what was omitted while the machine result remains complete.
- The installed binary, bootstrap binary, and CI binary record their revision
  and compiler policy so a stale executable cannot masquerade as a new test.

The cache model follows hermetic action principles: reusable results identify
all tools and inputs
([Bazel hermeticity](https://bazel.build/concepts/hermeticity),
[remote caching](https://bazel.build/remote/caching)).

## Efficient test selection

Selection is safe only after a full baseline records dependency and coverage
data. For each change, fo selects:

- tests directly depending on changed source/module interfaces.
- tests covering changed compiler code.
- all representatives of affected standard features and failure clusters.
- ABI/schema producer-consumer contracts.
- a small diversity bucket for discovery.

Known failures run first, then slow tests, so useful evidence arrives early.
LLVM lit documents both orderings, per-test coverage/timing, filters, and
explicit sharding ([lit manual](https://llvm.org/docs/CommandGuide/lit.html)).

Random samples estimate rates and discover flakes. They never prove zero.
Nightly shards are non-overlapping and their exact union is the completion
gate. A report always states the locked manifest digest, eligible population,
selection rule, selected count, and unique coverage accumulated in the epoch.

## Open issue map

| Workstream | Open issues |
| --- | --- |
| test completeness | [#119](https://github.com/lazy-fortran/fo/issues/119) |
| diagnostics and editor path | [#103](https://github.com/lazy-fortran/fo/issues/103), [#56](https://github.com/lazy-fortran/fo/issues/56) |
| deep lint integration | [#59](https://github.com/lazy-fortran/fo/issues/59) |
| formatter correctness | [#117](https://github.com/lazy-fortran/fo/issues/117) |
| synthesis/proof pipeline, deferred | [#120](https://github.com/lazy-fortran/fo/issues/120) |
| superseded runner retirement | [#62](https://github.com/lazy-fortran/fo/issues/62) |

#59 waits for fluff #262's negative-control verification. #103 stabilizes the
structured FortFront diagnostic mapping before #56 grows the debounced LSP
path. #120 starts after the standard and FortFront Synthesis contracts exist.
It is not part of current ffc conformance.

## Verification and delivery

Every workflow change needs an independent behavioral oracle. Repository-state
checks alone are insufficient.

- DAG changes build and run a deliberately adversarial multi-file project from
  an empty build directory, and also check a real diagnostic for a missing or
  cyclic dependency.
- result transport tests exceed old size/count limits and compare every record
  and exit status to the child processes that ran.
- cache changes mutate one declared input at a time and prove required misses,
  then mutate an unrelated input and prove a safe hit.
- selection changes are backtested against historical defects and rejected if
  they omit a test that caught one.
- compiler-policy changes run a cold compile/link/execute oracle on the touched
  lane.

Use focused tests while editing, then one `FO_JOBS=1` bare `fo` run before the
final commit. Heavy full builds serialize on constrained hosts. Merge small
green changes early and update this roadmap whenever a build/test contract or
cross-repository gate changes.
