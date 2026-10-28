#!/bin/bash
# Bootstrap Fo with LLVM Flang VERSION, then run its focused tests and the
# Flang consumer fixtures. Needs FPM_NATIVE from ci/flang-setup.sh.
set -euo pipefail
llvm=${1:?usage: ci/flang-cell.sh LLVM_VERSION}
: "${FPM_NATIVE:?run ci/flang-setup.sh first}"
# Bisect steps run a saved copy of this script against the checked-out tree.
src=${FO_SRC:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}
work=${RUNNER_TEMP:-/tmp}/flang-$llvm
rm -rf "$work"
mkdir -p "$work/bin" "$work/tmp"
ln -s "/usr/bin/flang-$llvm" "$work/bin/flang"
export PATH="$work/bin:$PATH" GIT_TERMINAL_PROMPT=0
export FPM_CC="clang-$llvm" FPM_CXX="clang++-$llvm" FO_FC=gfortran FO_JOBS=2
export OMP_NUM_THREADS=1 FO_DISABLE_SELF_REFRESH=1 FO_SELF_REFRESH=0
export TMPDIR="$work/tmp" FO_CACHE_DIR="$work/cache"
cd "$src"

OMP_NUM_THREADS="$FO_JOBS" "$FPM_NATIVE" install \
  --compiler "flang-$llvm" --prefix "$work/install"
digest=$(sha256sum "$work/install/bin/fo" | cut -d' ' -f1)
driver="$work/fo-driver-pins/$digest"
mkdir -p "$(dirname "$driver")"
install -m 0555 "$work/install/bin/fo" "$driver"
test "$(sha256sum "$driver" | cut -d' ' -f1)" = "$digest"
"$driver" --version
export FO_DRIVER_A="$driver" FO="$driver" FO_BIN="$driver"
# The original job installed into ~/.local/bin, which was on PATH.
export PATH="$work/install/bin:$PATH"

echo "::group::Compiler and project-model focused tests (Flang $llvm)"
"$driver" test --rerun --retries 0 --verbose \
  test_compiler_dialect test_project_model test_fpm_project_model \
  test_section_capability test_native_compiler_path_cli \
  test_mcp_system test_mcp_protocol_contract
echo "::endgroup::"

echo "::group::Real native consumers with Flang $llvm"
export FO_FC="flang-$llvm"
cd test-fixtures/flang_smoke
"$driver" build
"$driver" test --rerun --retries 0 --verbose
"$driver" exec --no-build flang_smoke
cd cmake
export FO_CMAKE_ARGS="-DCMAKE_Fortran_COMPILER=/usr/bin/flang-$llvm"
"$driver" build
"$driver" test --rerun --retries 0 --verbose
echo "::endgroup::"
