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
fpm build
    )
    load_job_environment
    echo 'Install fo onto PATH'
    (
# test_scaffold verifies that a freshly scaffolded project builds by
# running `fo check` inside it, so `fo` has to be resolvable. Install
# the binary just built rather than skipping the assertion: a test
# that silently does nothing when a tool is absent is worse than one
# that fails.
fpm install --prefix "$HOME/.local"
echo "$HOME/.local/bin" >> "$GITHUB_PATH"

    )
    load_job_environment
    echo 'Verify fo is the binary just built'
    (
test -x "$HOME/.local/bin/fo"
"$HOME/.local/bin/fo" --version

    )
    load_job_environment
    echo 'Pin exact fo image for Gremlin driver oracle'
    (
umask 077
driver="$HOME/.local/bin/fo"
digest="$(sha256sum "$driver" | cut -d' ' -f1)"
pinned="$RUNNER_TEMP/fo-driver-pins/$digest"
mkdir -p "$(dirname "$pinned")"
install -m 0555 "$driver" "$pinned"
test "$(sha256sum "$pinned" | cut -d' ' -f1)" = "$digest"
echo "FO_DRIVER_A=$pinned" >> "$GITHUB_ENV"

    )
    load_job_environment
    echo 'Test project model and scoped Gremlin behavior'
    (
export PATH="$HOME/.local/bin:$PATH"
export FO="$FO_DRIVER_A" FO_BIN="$FO_DRIVER_A"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS=1
export TMPDIR="$RUNNER_TEMP/fo-focused-tests"
mkdir -p "$TMPDIR"
"$FO_DRIVER_A" test test_project_model test_fpm_project_model \
  test_cmake_project_model test_project_selection_cli \
  test_project_layering_cli test_gremlin_case_reuse test_gremlin_cmake \
  test_atomic_link_artifacts test_cmake_native_build_cli

    )
    load_job_environment
    echo 'Test campaign, journal and coverage recovery'
    (
export PATH="$HOME/.local/bin:$PATH"
export FO="$FO_DRIVER_A" FO_BIN="$FO_DRIVER_A"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 FO_JOBS=1
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
export FO_TEST_WALL_TIMEOUT=300
export TMPDIR="$RUNNER_TEMP/fo-recovery-tests"
mkdir -p "$TMPDIR"
"$FO_DRIVER_A" test test_gremlin_journal_recovery \
  test_gremlin_coverage_restart test_gremlin_campaign_resolution

    )
    load_job_environment
    echo 'Test public fo test dispatch'
    (
export PATH="$HOME/.local/bin:$PATH"
export FO="$HOME/.local/bin/fo"
export FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0
FO_TEST_WALL_TIMEOUT=180 \
FO_CACHE_DIR="$RUNNER_TEMP/fo-mcp-system" \
  "$HOME/.local/bin/fo" test test_mcp_system

    )
    load_job_environment
    echo 'Test Gremlin self refresh'
    (
export TMPDIR="$RUNNER_TEMP/fo-self-refresh-oracle-tmp"
mkdir -p "$TMPDIR"
# Cold profiles plus release install exceed the ordinary 600-second
# test cap on CI. These are safety bounds, not timing assertions.
# Each nested command has a 900-second cap (317 s measured install).
# Run this expensive oracle once so its first failure reaches the log
# collector instead of a cold retry consuming the job's safety budget.
FO_BIN="$HOME/.local/bin/fo" \
FO_RUN_SELF_REFRESH_ORACLE=1 \
FO_TEST_WALL_TIMEOUT=1800 \
FO_DISABLE_SELF_REFRESH=1 \
FO_SELF_REFRESH=0 \
FO_CACHE_DIR="$RUNNER_TEMP/fo-self-refresh-oracle-cache" \
  "$HOME/.local/bin/fo" test --rerun --retries 0 --verbose \
    test_self_refresh

    )
    load_job_environment
    echo 'Test install help side effects'
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
FO_BIN="$driver" "$driver" test test_install_help

    )
    load_job_environment
    echo 'Test stale MCP recovery'
    (
driver="$FO_DRIVER_A"
FO="$driver" FO_BIN="$driver" FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 \
  "$driver" test test_mcp_gremlin_stale

    )
    load_job_environment
    echo 'Test MCP cancellation and diagnostics'
    (
driver="$FO_DRIVER_A"
FO="$driver" FO_BIN="$driver" FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0 \
  "$driver" test test_mcp_cancel_error

    )
    load_job_environment
    echo 'Test standalone asynchronous process C fixtures'
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
    load_job_environment
    ;;
  *) echo "unknown CI tier: $tier" >&2; exit 2 ;;
esac
