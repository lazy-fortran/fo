# fo architecture and compatibility

## Build model

fo detects the nearest parent `fpm.toml` or `CMakeLists.txt`. A directory with
both uses the native fpm-manifest backend.

For an fpm project, fo parses the manifest, resolves source roots and
dependencies, scans Fortran modules, constructs a dependency DAG, and compiles
ready nodes through a native OpenMP loop. Compiler processes are launched with
argv vectors, without a shell. Applications, tests, and examples link against
a content-keyed static archive, so unreachable library objects cannot introduce
symbols or dependencies into a target.

Submodule ancestry is part of that DAG. A direct submodule depends on its
ancestor module; `submodule (ancestor:parent) child` depends on both the
ancestor module and the immediate parent submodule. Repeating that rule builds
longer ancestry chains in compiler order without source-filename conventions or
artificial ordering modules.

Each compilation action is keyed by:

```text
SHA-256(source content, compiler identity, effective flags,
        dependency module payloads)
```

Objects and module files are restored from the shared CAS. Module-interface
hashes allow an implementation-only edit to avoid recompiling dependents.
Binary links and successful tests have separate content keys.

GNU Fortran uses `-pipe` to pass compiler stages without intermediate files.
For compiler drivers supporting `-fuse-ld`, the link policy prefers an
available `ld.lld`; a failed LLD invocation is discarded and retried through
the driver's default linker. The selected policy is part of the binary action
key. `FO_LINKER=default`, `auto`, or `lld` overrides automatic selection.

## Parallel safety

OpenMP is the native scheduler. Compilation and test regions use worker-private
argv buffers, filenames, logs, clocks, and temporary paths. Shared progress is
atomic or protected by a named critical region. Cache entries are published
atomically. A project lock serializes build-tree materialization by concurrent
fo processes, while separate projects can share the CAS concurrently.

No Fortran formatted I/O occurs in the compiler child after a threaded fork.
The process shim launches prebuilt argv vectors with `posix_spawn` or the
platform-equivalent safe path.

## fpm contract

fo reads the established `fpm.toml` format. Common library, executable, test,
example, dependency, preprocessing-macro, link, and Fortran-language settings
are mapped into the native model. Explicit target names remain the public names,
including when the source is nested or has a different filename.

Native tests may receive target-specific command-line arguments through the
FPM-standard `extra` namespace:

```toml
[extra.fo.test-args]
test_oracle = ["test/data/reference.csv", "--strict"]
```

The key is the public test name, and each quoted array element remains one
argument. Test arguments participate in the cached test-result key.

Consolidated tests can opt into one dispatcher executable:

```toml
[extra.fo]
dispatcher = "test_suite"
```

Each case source starts with `! fo: dispatcher`. It may be a module exporting
the case procedure, with its original filename retaining the public test name.
The dispatcher uses those modules and accepts exactly one bare public case name
as its argument. fo compiles and links the dispatcher once before running cases
in parallel. Both `fo test <name>` and `fo test --all` report the original names;
the dispatcher itself runs only when explicitly selected. Manifest aliases and
nested test sources follow the same rules. Unmarked test programs retain their
own executables.

fo and fpm deliberately differ in private state:

- fo stores disposable project views in `build/fo`
- fo stores reusable actions in the global SHA-256 CAS
- fpm stores compiler and flag hashes in private `build/<compiler>_<hash>`
  directories
- fo does not fabricate fpm digest files, so a later fpm command safely rebuilds
  or reuses only state fpm owns

Shell output is also intentionally different. fo emits compact progress and
bounded diagnostics for LLM edit loops.

Git and registry dependency acquisition still uses fpm as a bootstrap when no
compiled dependency artifacts are available. Installation currently delegates
to fpm with the release profile. Removing those last runtime dependencies is
tracked work.

## CMake contract

CMake mode configures `cmake -S . -B build -G Ninja`, forwards `FC`,
`FO_CMAKE_ARGS`, and effective Fortran flags, then calls
`cmake --build build -j <FO_JOBS>`. Tests run through CTest with parallelism,
failure output, named filters, and slow-test label exclusion. fo does not parse
or replace CMake's target model.

## Agent interfaces

`fo check --agent` and MCP check return a bounded JSON result. MCP supports
check, status, diagnostics, cancellation, build, test, graph, info, changed,
clean, lint, format, and release installation. MCP clean preserves the shared
CAS unless `cache=true`. MCP installation accepts a prefix and always requests
the release profile.

MCP `test` accepts `json="full"` or `json="compact"` for the complete structured
test report, including every failing and passing entry. The report and its MCP
response grow with the results rather than truncating at a fixed byte limit.

## Work modes

`fo work start --mode serial` registers the main-session editor, starts one
Gremlin campaign, and spawns zero coding workers. `fo work start --mode
parallel --max-workers N --tasks tasks.json` starts the same campaign and
admits independent ready tasks up to the session worker limit. `fo work status`
reports the task graph, active worker count, blocked reasons, and worker logs;
`fo work cancel` requests cancellation of that work session and its campaign.
MCP exposes the same operations as `work_start`, `work_status`, and
`work_cancel` on the existing `fo` tool.

The task file is a JSON array. Each task has a unique `id` and an `argv` array;
optional `worktree`, `depends_on`, `files`, `apis`, `abis`, and `resources`
fields declare its checkout, dependency edges, ownership, and resource slots.
For example:

```json
[
  {"id":"parser","worktree":"/tmp/parser-wt",
   "files":["src/parser.f90"],"apis":["parse_source"],
   "resources":["build"],"argv":["worker-adapter","--task","parser"]},
  {"id":"tests","depends_on":["parser"],"files":["test/parser.f90"],
   "resources":["test"],"argv":["worker-adapter","--task","tests"]}
]
```

The adapter is launched directly from its argv vector; fo does not interpolate
it through a shell or select a paid provider. File, API, and ABI ownership
overlaps and dependency edges prevent simultaneous admission. Declared `cpu`,
`memory`, `build`, and `test` resources use shared count-based leases, with
capacities set by `FO_WORKER_CAPACITY`, `FO_WORK_CPU_CAPACITY`,
`FO_WORK_MEMORY_CAPACITY`, `FO_WORK_BUILD_CAPACITY`, and
`FO_WORK_TEST_CAPACITY`. Each worker gets `FO_JOBS=1` and `FO_TEST_JOBS=1` to
bound nested fo pools. A task's exit status and log are evidence for the
controller; worker processes cannot integrate or promote their changes.

The test-failure-path lint rule follows failure exits through project module
helpers. It scans module procedures in `src/`, `app/`, and `test/`, repeats the
collection until transitive helper calls converge, and distinguishes calls from
ordinary variables or array references with the same name. A helper that cannot
exit nonzero does not suppress the finding. Procedure pointers, generic-only
resolution, and separate submodule bodies remain documented unsupported cases.

The LSP surface reports compiler-backed diagnostics on save. It does not claim
to replace a full semantic Fortran language server.

`fo_compiler_service` defines a compiler-library request/result boundary and
explicit LFortran and Fortfront adapters. Both adapters are unavailable stubs:
the native build path does not instantiate or invoke them. Wiring either one
requires independent module-file, diagnostic, runtime-library, and concurrency
correctness tests first.
