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

## Verification pipeline

`fo prove`, `fo derive`, `fo generate`, and `fo verify` implement the cheap
always-on verification tier: text-level `!@` directives in source comments, no
parse tree, exactly like the linter. Assumptions, claims, properties, and
derivations are declared as:

- `!@assume <name>: <expr>` — a fact taken as given for the file
- `!@claim [class] <name>: <expr>` — a proof obligation
- `!@property <name>: <expr>` — a runtime property
- `!@derive <name>: <expr> => <target>` — generate a scalar kernel

Each obligation is content-addressed by its directive text, the file-scoped
assumptions it depends on, the `[extra.fo.verification]` policy, and the
backend identity. An unrelated implementation edit does not change the key, so
the cached proof is reused (no rerun). Changing an assumption changes the key,
so dependent proofs and generated kernels are invalidated. Proof results are
stored in the shared content-addressed CAS via `cache_store_action` /
`cache_restore_action`, keyed by the claim key.

External provers are invoked when present:

- Why3: `fo prove` emits a `.mlw` contract under `build/fo/generated/` and runs
  `why3 prove`; a non-zero exit leaves the obligation `UNKNOWN`.
- Lean: `fo prove` emits a `.lean` identity and runs `lean`.

A missing tool never silently converts a `PROVED` requirement into a skipped
check: it produces an explicit `UNKNOWN` status with an exact rerun command and
a log/certificate path. The numeric probe backend compiles a small Fortran
probe that samples the obligation over a deterministic grid. It can `DISPROVE`
(with a minimal counterexample) but never `PROVE`, so agent JSON distinguishes
proof evidence (backend `why3`/`lean`) from numerical probe evidence (backend
`probe`).

Policy:

```toml
[extra.fo.verification]
require-proof = ["generated-kernel-equivalence", "array-bounds"]
allow-unknown = ["special-function-identity"]
property-test-unknown = true
lean = "auto"
why3 = "auto"
```

`fo check --proofs` folds verification into the ordinary bounded workflow:
after build+test it runs the obligations and fails when a required proof is
missing or disproved. `fo clean --proof-cache` drops the generated proof
artifacts and certificates (the shared CAS is preserved). MCP exposes `prove`,
`derive`, and `verify` actions returning the same JSON as `fo prove --json`.
