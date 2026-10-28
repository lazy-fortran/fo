# fpm compatibility and CMake projects

## fpm projects

fo implements the common [fpm](https://fpm.fortran-lang.org) project contract
with its own cache and output. Supported manifest features:

- library, application, test and example source trees
- automatic and explicit `[[executable]]`, `[[test]]` and `[[example]]` target
  names
- path, Git, registry and development dependencies
- link libraries and compiler flags
- C preprocessor macros
- Fortran source form, implicit typing and implicit external settings
- the OpenMP metapackage
- `build.auto-executables = false` with explicit `[[executable]]` selection
- release, debug and sanitizer profiles

`fo run` accepts the fpm-style options `--target`, `--example`, `--profile` and
`--flag`.

Build artifacts live under `build/fo`; applications and examples are also
available in `build/fo/app`. This fixed path avoids depending on fpm's
compiler-flag hash directories. fo does not write fpm's digest metadata, so
running fpm after fo is safe.

### Dependencies

Git dependencies are fetched with Git and built by fo. Namespaced registry
dependencies are resolved from a local fpm registry tree, without fpm or
network access. fo reads `[registry] path` from
`~/.local/share/fpm/config.toml`; `FO_FPM_CONFIG_FILE` selects another
configuration file (absolute path). A registry package lives under
`path/namespace/name/version`. An explicit `v` selects that version; without
`v` the latest numeric version is chosen. Remote registries, authentication
and publishing are not supported and fail with an explicit message.

### Installation

`fo install` builds the applications with the release profile and installs
them under `prefix/bin`. Installing examples, tests, libraries and module files
is not supported.

### fo-specific settings

Settings that fpm does not know live under `[extra.fo]`, which fpm ignores.
See [testing.md](testing.md) for test budgets, retries and arguments, and
[FO.md](FO.md#fpm-contract) for input fixtures, library tiers and test groups.

## CMake projects

If no `fpm.toml` is found, fo searches parent directories for
`CMakeLists.txt`. It configures with Ninja, builds with `cmake --build -j` and
runs tests with CTest. `FO_CMAKE_ARGS` passes extra configure arguments and
`FO_JOBS` sets build and test parallelism.

A directory that contains both files uses CMake if the top-level
`CMakeLists.txt` unconditionally enables testing with `include(CTest)` or
`enable_testing()`; a call inside `if()`, `foreach()` or a function does not
count, since it may register no tests. Otherwise fo uses `fpm.toml`.
`FO_BACKEND=cmake` or `FO_BACKEND=fpm` selects the backend explicitly.

## Native ffc mode

```text
fo build --native [-o PROGRAM] SOURCE...
fo run --native SOURCE [ARGS...]
```

This mode compiles the given sources with
[ffc](https://github.com/lazy-fortran/ffc) 0.1.0 or newer instead of a project
build.
