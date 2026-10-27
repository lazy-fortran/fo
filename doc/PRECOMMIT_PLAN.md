# Fast staged pre-commit checks

`fo hook pre-commit --bounded [--json]` validates the **index**, including partially staged
files. Git `write-tree` freezes its identity; a private index materializes that
exact tree in an owned sibling directory. Initialized submodules are checked out
at their staged gitlink commits. Unstaged and untracked project files are never
substituted for staged files. The working tree and real index are unchanged.
Relative sibling dependencies retain their normal paths. Temporary execution
uses the caller's private TMPDIR under `/var/tmp`; owned views are removed.

The small MVP reuses the native formatter, backend, project impact selection,
input inventory and shared store. No new scheduler or test registry:

1. Check staged Fortran formatting with the same implementation as `fo fmt
   --check`. It never rewrites or re-stages files. Format intentionally, inspect
   the diff, re-stage exactly the committed paths, repeat the same commit.
2. Look up a build-and-fast-test receipt for the staged view's exact logical
   input closure, compiler/configuration/environment and Fo driver identity.
   Git's tree ID accompanies the result. An incomplete input inventory never
   yields a cache hit. Build artifacts alone never prove tests passed.
3. On a miss, the staged build is mandatory. The bounded commit command shares
   one approximately ten-second wall budget across configuration, compilation,
   tests, cancellation and cleanup. A failure or missing verification blocks
   the commit and directs explicit preparation with `fo hook pre-commit`.
4. Run fast affected cases in model order within the remaining normal budget,
   using the same conservative impact model as `fo test --only-changed`.
   Unknown impact widens selection. A failing completed test blocks. Slow,
   excluded cases are **DEFERRED to merge_group CI**. A killed or unrun fast
   case is never PASS and blocks the bounded commit command. No wait for active Gremlin work,
   graph ranking, sampling or full suite in the hook. Fo Git acquisition is
   restricted to local files; acquire dependencies before committing. Both hook modes add the disconnected flags to the existing CMake configuration
   identity and disable FetchContent downloads and updates. A project that is
   already configured contributes its populated dependency source roots to that
   identity, so the staged copy builds against the dependencies the project holds
   instead of acquiring new ones. Acquire prerequisites
   with normal `fo build` before explicit hook preparation;
   arbitrary project configure scripts retain responsibility for their own network contract.

The existing shared store holds a small atomic receipt only after the build and
**every** fast model case passed. Gremlin publishes the same receipt at green
quiescence only after checking every fast case's current closure digest against
its PASS index; a scoped lane is insufficient by itself. A hook's affected subset
cannot create a full-tier receipt. This deliberately conservative reuse may miss
when CMake inputs cannot be relocated; correctness takes precedence over a hit.
The snapshot itself and native model loading still cost time on a receipt hit;
millisecond latency is a target to measure, not a promised constant.

The explicit manual preparation command retains a 10-second test budget, a
60-second mandatory build cap and a 1-second fast-test warning threshold. The
bounded commit command cancels owned work after eight seconds, reserving about
two seconds for process drainage and snapshot cleanup. Raising manual budgets
does not extend the commit budget. Configure manual preparation per repository:

```sh
git config fo.hookBudget 10
git config fo.hookBuildCap 60
git config fo.hookFastThreshold 1
```

When both manifests exist, a CMake cache whose home directory matches the
checkout establishes CMake as its build authority. The hook reuses populated
dependency source roots from that build, including header-only roots referenced
by its File API metadata. Set `FO_CMAKE_BUILD_DIR` for a nondefault build directory;
staged build outputs remain private. Hook builds enable Fo's relocatable object
cache so successive staged views can reuse compilation and executable results.

Projects can apply their native fast-test label policy with Git configuration.
For example, a project whose fast CTest target excludes reference comparisons
can use:

```sh
git config fo.hookExcludeLabels 'slow|regression|performance|scalability'
```

`fo.hookIncludeLabels` applies the corresponding inclusion filter. Each
comma-separated entry is a label expression, with the same matching rules as
`fo test -L` and `fo test -LE`. The result reports label exclusions.

Fast test invocations record elapsed time and warn by name above the threshold;
this is advisory, not a new classification database. Mark slow tests using the
existing declarations (they remain available explicitly outside the hook):

```toml
# fpm.toml; plain fpm ignores this optional metadata.
[extra.fo.test-groups]
slow = { tests = ["test_expensive", "test_reference_*"] }
```

```cmake
set_tests_properties(test_expensive PROPERTIES LABELS slow)
```

A CMake project can also keep a whole label out of the commit tier, which suits
cases that need a reference checkout or a network and therefore can never pass
inside a commit. Each comma-separated entry is handled as one `-LE` argument of
`fo test`, so the usual CMake idiom is one alternation:

```sh
git config fo.hookExcludeLabels "slow|regression|performance|scalability"
git config fo.hookIncludeLabels "unit"
```

JSON contains the staged tree, build verdict, cache hit, fast PASS count,
deferred count/names, slow warnings, exit code and a human summary. Names are
bounded to 32 per list with explicit truncation flags; diagnostics are bounded.
A one-line human verdict always includes the deferred count. Partial coverage
is never called full verification. Before marking a PR ready, require a green
Gremlin generation on its exact head covering the full affected set. The repo's
native CMake/CTest or fpm merge-group CI remains the final guarantee.

## Hook chain and tool choices

The shared global hook invokes the bounded mode after staged hygiene, then the repo's
own hook/framework. No Fo installation is required to use CMake/fpm or commit:
missing Fo or gitleaks prints one hint and skips that tool. Missing optional
framework executables warn too. A cached release hint is advisory, refreshed
only during installation, with a 24-hour validity; hooks never fetch releases.

Runtime ranges below are planning estimates for a few staged files, not measured
guarantees. Install/pin tools ahead of commits; no network setup in hooks.

| Repo/files | Per commit | Typical warm time |
| --- | --- | --- |
| Fortran fpm/CMake | staged native format check; exact receipt or build + fast affected tests | overall target ~10 s; larger builds need explicit preparation |
| Python | `ruff check`, `ruff format --check`; focused tests only if cheap | ~0.05–1 s |
| Shell | `shellcheck`, `shfmt -d` on staged scripts | ~0.05–1 s |
| YAML/Actions | YAML parse/lint; `actionlint` on staged workflows | ~0.05–1 s |
| All | staged secret/size/type/whitespace checks | ~0.05–1 s |

[pre-commit](https://pre-commit.com/) is mature, pins hook environments and checks
staged contents by temporarily stashing unstaged changes. First-run environment
installation belongs outside the commit loop. [prek](https://prek.j178.dev/running-hooks/)
is a compatible alternative with the same staged/stash semantics.
[Lefthook](https://lefthook.dev/configuration/run/) passes staged filenames;
[`stage_fixed`](https://lefthook.dev/configuration/stage_fixed/) opts into `git add`
after fixes. Filenames alone do not establish staged **contents**. Fo isolates
an [index tree](https://git-scm.com/docs/git-write-tree) instead; [Git hook semantics](https://git-scm.com/docs/githooks)
define exit status as the local commit veto. Check-only formatting avoids partial
staging surprises. Hooks are optional local feedback, never the CI guarantee.

[Bazel](https://bazel.build/remote/caching) keys actions by declared inputs,
commands and environment. [Pants](https://www.pantsbuild.org/stable/docs/using-pants/advanced-target-selection)
selects changed targets and transitive dependents. [Nx](https://nx.dev/docs/features/ci-features/affected)
combines affected selection with cache inputs. The common lesson is conservative
input closure and separate test results, rather than mtime or a cached build.

Fortran choices: [fprettify](https://github.com/fortran-lang/fprettify) changes
whitespace; [findent](https://sourceforge.net/projects/findent/) handles indentation
and fixed/free conversion. [Fortitude](https://fortitude.readthedocs.io/en/stable/linter/)
provides fast configurable static checks and opt-in fixes. Fo's native `fmt` and
cheap `lint` avoid extra runtime dependencies; deeper AST analysis stays opt-in.
[LFortran AST/ASR checks](https://docs.lfortran.org/en/usage/) are useful compiler
checks but require the project's supported language/features and module closure;
they are not a mandatory second compiler in the pre-commit layer.
[CTest labels](https://cmake.org/cmake/help/latest/prop_test/LABELS.html) provide the
existing CMake slow-tier tagging mechanism.
