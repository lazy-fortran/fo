#!/bin/bash
# The same native test selection runs in a build container or a heavy Slurm tier.
set -euo pipefail
src=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$src"
if [ -n "${SLURM_JOB_ID:-}" ]; then
    root=$(dirname "$src")
    tier=$(cat "$root/tier.txt")
    . "$root/task.env"
    . /home/ert/opt/kin6d-env.sh
    export PATH=/home/ert/.local/bin:$PATH
else
    tier=${1:?usage: ci/run.sh TIER}
    root=$(mktemp -d "${RUNNER_TEMP:-/tmp}/native-ci.XXXXXX")
    export FC=/usr/bin/gfortran CC=/usr/bin/gcc CXX=/usr/bin/g++
    export OPENBLAS_NUM_THREADS=1 MPLBACKEND=Agg
fi
unset DISPLAY
export CMAKE_TOOLCHAIN_FILE=$src/ci/cluster-toolchain.cmake
if [ -d "$root/mirrors" ]; then
    export GIT_CONFIG_GLOBAL=$root/gitconfig
    : > "$GIT_CONFIG_GLOBAL"
    git config --global protocol.file.allow always
    shopt -s nullglob
    for mirror in "$root"/mirrors/*/*.git; do
        slug=${mirror#"$root/mirrors/"}; slug=${slug%.git}
        git config --global --add "url.file://$mirror.insteadOf" "https://github.com/$slug.git"
        git config --global --add "url.file://$mirror.insteadOf" "https://github.com/$slug"
        git config --global --add "url.file://$mirror.insteadOf" "git@github.com:$slug.git"
    done
fi
export GITHUB_WORKSPACE=$src
export GITHUB_ENV=$root/github-env GITHUB_PATH=$root/github-path GITHUB_OUTPUT=$root/github-output
: > "$GITHUB_ENV"; : > "$GITHUB_PATH"; : > "$GITHUB_OUTPUT"
export SCI_CPUS=${SCI_CPUS:-8}
export CMAKE_BUILD_PARALLEL_LEVEL=$SCI_CPUS CTEST_PARALLEL_LEVEL=$SCI_CPUS
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
export RUNNER_TEMP=$root/tmp
mkdir -p "$RUNNER_TEMP" ci-out
# Runner home is noexec; keep the installed driver in job-private executable scratch.
export FO_INSTALL_PREFIX=$RUNNER_TEMP/fo-install
# Compiler probes and isolated Python builds must execute outside noexec /tmp.
export TMPDIR=$RUNNER_TEMP
{ hostname; /usr/bin/gfortran --version | head -1; git rev-parse HEAD; } | tee ci-out/toolchain.txt
load_job_environment() {
    while IFS='=' read -r key value; do
        [[ $key =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] && export "$key=$value"
    done < "$GITHUB_ENV"
    while IFS= read -r item; do [ -z "$item" ] || export PATH="$item:$PATH"; done < "$GITHUB_PATH"
}
case "$tier" in
  ci-build-test)
    export GIT_TERMINAL_PROMPT="0"
    echo Build
    (
# Freeze the source inputs before the sole native bootstrap.
receipt="$RUNNER_TEMP/fo-bootstrap-receipt.sha256"
compiler=$(realpath "$FC")
{
  find "$src/src" "$src/app" "$src/test-support" -type f -print0
  if [ -d "$src/include" ]; then find "$src/include" -type f -print0; fi
  printf '%s\0' "$src/fpm.toml" "$src/test/test_random_tests.f90" "$compiler"
  if [ -f "$src/fpm.lock" ]; then printf '%s\0' "$src/fpm.lock"; fi
  if [ -d "$src/build/dependencies" ]; then
    find "$src/build/dependencies" -type f \( \
      -path '*/src/*' -o -path '*/include/*' -o -name fpm.toml \) -print0
  fi
} | sort -zu | xargs -0 -r sha256sum > "$receipt"
fpm build --verbose 2>&1 | tee "$RUNNER_TEMP/fo-bootstrap.log"
    )
    load_job_environment
    echo 'Install fo onto PATH'
    (
# test_scaffold verifies that a freshly scaffolded project builds by
# running `fo check` inside it, so `fo` has to be resolvable. Install
# the binary just built rather than skipping the assertion: a test
# that silently does nothing when a tool is absent is worse than one
# that fails.
fpm install --prefix "$FO_INSTALL_PREFIX"
echo "$FO_INSTALL_PREFIX/bin" >> "$GITHUB_PATH"

    )
    load_job_environment
    echo 'Verify fo is the binary just built'
    (
test -x "$FO_INSTALL_PREFIX/bin/fo"
"$FO_INSTALL_PREFIX/bin/fo" --version

    )
    load_job_environment
    echo 'Pin exact fo image for Gremlin driver oracle'
    (
umask 077
driver="$FO_INSTALL_PREFIX/bin/fo"
digest="$(sha256sum "$driver" | cut -d' ' -f1)"
pinned="$RUNNER_TEMP/fo-driver-pins/$digest"
mkdir -p "$(dirname "$pinned")"
install -m 0555 "$driver" "$pinned"
test "$(sha256sum "$pinned" | cut -d' ' -f1)" = "$digest"
echo "FO_DRIVER_A=$pinned" >> "$GITHUB_ENV"

    )
    load_job_environment
    echo 'Record native core identity for self-refresh fixture'
    (
receipt="$RUNNER_TEMP/fo-bootstrap-receipt.sha256"
sha256sum --check --status "$receipt"
# The one native app link identifies its archive and module directory.
archive=$(awk '
  /app_main[.]f90[.]o/ && /libfo[.]a/ && / -o / {
    for (i=1; i<=NF; i++) if ($i ~ /\/libfo[.]a$/) { path=$i; count++ }
  }
  END { if (count != 1) exit 1; print path }
' "$RUNNER_TEMP/fo-bootstrap.log")
archive=$(realpath "$src/$archive")
# fpm may give the archive a different hash directory from its module outputs.
module_dir=$(awk '
  / -c / && /fo_cli_entry[.]f90([[:space:]]|$)/ {
    for (i=1; i<=NF; i++) {
      if ($i == "-J") { path=$(i+1); count++ }
      else if ($i ~ /^-J.+/) { path=substr($i, 3); count++ }
    }
  }
  END { if (count != 1 || path == "") exit 1; print path }
' "$RUNNER_TEMP/fo-bootstrap.log")
module_dir=$(realpath "$src/$module_dir")
test -f "$module_dir/fo_cli_entry.mod"
compiler=$(realpath "$FC")
{
  cmp "$FO_INSTALL_PREFIX/bin/fo" "$FO_DRIVER_A"
  printf '%s\0' "$archive" "$compiler" "$FO_DRIVER_A" "$FO_INSTALL_PREFIX/bin/fo"
  find "$module_dir" -maxdepth 1 -type f -name '*.mod' -print0
  find "$src/build/dependencies" -type f \( \
    -path '*/src/*' -o -path '*/include/*' -o -name fpm.toml \) -print0
  if [ -f "$src/fpm.lock" ]; then printf '%s\0' "$src/fpm.lock"; fi
} | sort -zu | xargs -0 -r sha256sum >> "$receipt"
printf '%s\n' "FO_SELF_REFRESH_CORE_ARCHIVE=$archive" \
  "FO_SELF_REFRESH_CORE_MODULE_DIR=$module_dir" \
  "FO_SELF_REFRESH_IDENTITY_RECEIPT=$receipt" \
  "FO_SELF_REFRESH_COMPILER=$compiler" >> "$GITHUB_ENV"

    )
    load_job_environment
    echo 'Prepare focused test programs once'
    (
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS="$SCI_CPUS"
export FO_CACHE_DIR="$RUNNER_TEMP/fo-test-preparation-cache"
export TMPDIR="$RUNNER_TEMP/fo-test-preparation-tmp"
mkdir -p "$TMPDIR"
# Runtime groups keep separate state while reusing this exact prepared tree.
focused_tests=(
  test_project_model test_fpm_project_model test_cmake_project_model
  test_cmake_capture_paths test_cmake_capture_inputs test_cmake_parent_modules
  test_project_selection_cli test_project_layering_cli test_gremlin_case_reuse
  test_gremlin_cmake test_atomic_link_artifacts test_cmake_native_build_cli
  test_gremlin_journal_recovery test_gremlin_coverage_restart
  test_gremlin_campaign_resolution test_mcp_system test_self_refresh
  test_install_help test_mcp_gremlin_stale test_mcp_cancel_error test_self_refresh_profile
)
prepare_args=(build --tests)
for name in "${focused_tests[@]}"; do prepare_args+=(--case "$name"); done
"$FO_DRIVER_A" "${prepare_args[@]}"

    )
    load_job_environment
    step_model() {  # Test project model and scoped Gremlin behavior
    (
export PATH="$FO_INSTALL_PREFIX/bin:$PATH"
export FO="$FO_DRIVER_A" FO_BIN="$FO_DRIVER_A"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS=1
export TMPDIR="$RUNNER_TEMP/fo-focused-tests"
mkdir -p "$TMPDIR"
"$FO_DRIVER_A" test --no-build test_project_model test_fpm_project_model \
  test_cmake_project_model test_cmake_capture_paths test_cmake_capture_inputs \
  test_cmake_parent_modules \
  test_project_selection_cli test_project_layering_cli test_gremlin_case_reuse \
  test_gremlin_cmake test_atomic_link_artifacts test_cmake_native_build_cli

    )
    }
    step_recovery() {  # Test campaign, journal and coverage recovery
    (
export PATH="$FO_INSTALL_PREFIX/bin:$PATH"
export FO="$FO_DRIVER_A" FO_BIN="$FO_DRIVER_A"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS=1
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
export FO_TEST_WALL_TIMEOUT=300
export TMPDIR="$RUNNER_TEMP/fo-recovery-tests"
mkdir -p "$TMPDIR"
"$FO_DRIVER_A" test --no-build test_gremlin_journal_recovery \
  test_gremlin_coverage_restart test_gremlin_campaign_resolution

    )
    }
    step_dispatch() {  # Test public fo test dispatch
    (
export PATH="$FO_INSTALL_PREFIX/bin:$PATH"
export FO="$FO_INSTALL_PREFIX/bin/fo"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0
FO_TEST_WALL_TIMEOUT=180 \
FO_CACHE_DIR="$RUNNER_TEMP/fo-mcp-system" \
  "$FO_INSTALL_PREFIX/bin/fo" test --no-build test_mcp_system

    )
    }
    step_self_refresh() {  # Test Gremlin self refresh
    (
export TMPDIR="$RUNNER_TEMP/fo-self-refresh-oracle-tmp"
mkdir -p "$TMPDIR"
# Keep the command bounds and report the first failure without retrying.
FO_SELF_REFRESH_FIXTURE="$RUNNER_TEMP/fo-self-refresh-fixture" \
FO_BIN="$FO_INSTALL_PREFIX/bin/fo" \
FO_RUN_SELF_REFRESH_ORACLE=1 \
FO_TEST_WALL_TIMEOUT=1800 \
FO_DISABLE_SELF_REFRESH=1 \
FO_SELF_REFRESH=0 \
FO_CACHE_DIR="$RUNNER_TEMP/fo-self-refresh-oracle-cache" \
  "$FO_INSTALL_PREFIX/bin/fo" test --no-build --rerun --retries 0 --verbose \
    test_self_refresh test_self_refresh_profile

    )
    }
    step_install_help() {  # Test install help side effects
    (
export FO_RUN_INSTALL_HELP_ORACLE="1"
export FO_DISABLE_SELF_REFRESH="1"
export FO_SELF_REFRESH="0"
export HOME="$RUNNER_TEMP/fo-install-help-home"
export FO_CACHE_DIR="$RUNNER_TEMP/fo-install-help-cache"
export FO_INSTALL_HELP_PREFIX="$RUNNER_TEMP/fo-install-help-prefix"
export TMPDIR="/var/tmp"
mkdir -p "$HOME" "$FO_CACHE_DIR"
driver="$FO_DRIVER_A"
FO_BIN="$driver" "$driver" test --no-build test_install_help

    )
    }
    step_mcp_stale() {  # Test stale MCP recovery
    (
driver="$FO_DRIVER_A"
FO="$driver" FO_BIN="$driver" FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 \
  "$driver" test --no-build test_mcp_gremlin_stale

    )
    }
    step_mcp_cancel() {  # Test MCP cancellation and diagnostics
    (
driver="$FO_DRIVER_A"
FO="$driver" FO_BIN="$driver" FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 \
  "$driver" test --no-build test_mcp_cancel_error

    )
    }
    step_c_fixtures() {  # Test standalone asynchronous process C fixtures
    (
export FO_TEST_CLI="$FO_DRIVER_A"
export TMPDIR="$RUNNER_TEMP/fo-process-fixtures"
mkdir -p "$TMPDIR"
verifier_dir="$(mktemp -d)"
trap 'rm -rf "$verifier_dir"' EXIT
fixture_dir=test-fixtures/c
process_dependencies=(src/proc/fo_gremlin_state.c
                      src/proc/fo_execution_scratch.c src/util/fo_fs.c
                      src/util/fo_sys.c)
cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function \
  "$fixture_dir/async_filter.c" "${process_dependencies[@]}" -o "$verifier_dir/filter-native"
"$verifier_dir/filter-native"
for target in AARCH64 ARM X86_64 UNSUPPORTED; do
  cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function \
    "-DFO_ASYNC_TEST_$target" "$fixture_dir/async_filter.c" \
    "${process_dependencies[@]}" \
    -o "$verifier_dir/filter-$target"
  "$verifier_dir/filter-$target"
done
cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function \
  "$fixture_dir/async_process_nested.c" "${process_dependencies[@]}" \
  -o "$verifier_dir/nested"
"$verifier_dir/nested"
cc -std=gnu11 -O2 -Wall -Wextra -Wno-unused-function \
  "$fixture_dir/async_process_nested_cancel.c" "${process_dependencies[@]}" \
  -o "$verifier_dir/nested-cancel"
"$verifier_dir/nested-cancel"

    )
    }
    # The groups keep separate scratch and caches, so they run concurrently;
    # the serial sum dominated the merge-queue time.
    run_lane() {
        local name=$1 step
        shift
        for step in "$@"; do
            echo "::group::$step"
            "$step"
            echo "::endgroup::"
        done > "$RUNNER_TEMP/lane-$name.log" 2>&1
    }
    lanes=(model recovery self_refresh)
    run_lane model step_model &
    pids=($!)
    run_lane recovery step_recovery step_dispatch step_mcp_stale step_mcp_cancel \
        step_c_fixtures &
    pids+=($!)
    run_lane self_refresh step_self_refresh step_install_help &
    pids+=($!)
    failed=0
    for i in "${!lanes[@]}"; do
        status=0
        wait "${pids[$i]}" || status=$?
        echo "Lane ${lanes[$i]}: exit $status"
        cat "$RUNNER_TEMP/lane-${lanes[$i]}.log"
        [ "$status" -eq 0 ] || failed=1
    done
    exit "$failed"
    ;;
  *) echo "unknown CI tier: $tier" >&2; exit 2 ;;
esac
