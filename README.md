# fo

fo is a build and test driver for Fortran projects. It reads `fpm.toml`, builds
the module dependency graph in parallel with OpenMP, caches compilation results
by content, and prints compact diagnostics. Projects that use CMake are built
with CMake and tested with CTest.

Status: under active development; commands and options may change.

## Features

- Native backend for the common `fpm.toml` contract: library, application, test
  and example targets; path, Git and local registry dependencies; profiles.
- Content-addressed build cache (SHA-256 of source, compiler identity, flags and
  dependency module interfaces) that can be shared between projects and hosts.
- Test runner with per-test CPU-time budgets, retries with a separate FLAKY
  verdict, per-case execution and selection of tests affected by changed modules.
- Continuous mode (`fo gremlin`, or bare `fo`) that rebuilds and retests as the
  working tree changes.
- Lint, formatting, module graph output and a JSON status report.
- MCP server and a diagnostics-only LSP server for editors and coding agents.
- `--native` mode that compiles single sources with
  [ffc](https://github.com/lazy-fortran/ffc).

## Install

fo builds with [fpm](https://fpm.fortran-lang.org) and a Fortran compiler with
OpenMP support (gfortran is the tested default):

```bash
git clone https://github.com/lazy-fortran/fo
cd fo
fpm install --profile release --prefix ~/.local
```

Dependencies ([fx](https://github.com/lazy-fortran/fx) and
[fortfront](https://github.com/lazy-fortran/fortfront)) are fetched by fpm.

## Usage

Run fo inside a directory that contains `fpm.toml` or `CMakeLists.txt`:

```text
fo build [--profile NAME]  build applications and examples
fo test [NAME ...]         build and run tests
fo test --only-changed     run tests affected by changed modules
fo run NAME [ARGS...]      build and run an application or example
fo verify                  static checks, build, tests, lint, format check
fo check --json=compact    one bounded JSON object with build and test status
fo changed                 changed modules and their reverse dependents
fo graph [--dot]           module dependency graph
fo lint                    native source checks and compiler warnings
fo fmt [--check]           format sources or check formatting
fo clean [--cache]         remove the build tree (and the shared cache)
fo install [--prefix DIR]  install release builds of the applications
fo info                    backend, compiler and cache information
fo mcp-server              MCP JSON-RPC server on stdin/stdout
fo lsp                     LSP diagnostics for unsaved buffers
```

`fo help` lists all commands and options. Build output goes to `build/fo`;
running fpm in the same tree afterwards is safe.

Gremlin started through MCP stops when its server process exits unless
`lifetime: persistent` is requested. CLI watchers remain persistent by default;
on Linux or macOS, use `fo gremlin start --owner task` to stop a temporary watcher
with its parent process. Explicitly stop temporary watchers when their work ends
if the parent or server remains shared across tasks.

## Documentation

- [doc/testing.md](doc/testing.md): test budgets, retries, cases and build profiles
- [doc/fpm-compatibility.md](doc/fpm-compatibility.md): supported `fpm.toml`
  features and CMake projects
- [doc/cache.md](doc/cache.md): cache keys, size limits, shared filesystems,
  parallelism and linking
- [doc/FO.md](doc/FO.md): architecture, test selection and agent interfaces
- [bench/README.md](bench/README.md): optional performance audits

## Related projects

- [fpm](https://github.com/fortran-lang/fpm): the manifest format fo reads
- [ffc](https://github.com/lazy-fortran/ffc): compiler used by `--native` mode
- [fluff](https://github.com/lazy-fortran/fluff): optional backend for `fo lint --deep`

## License

MIT, see [LICENSE](LICENSE).
