#!/bin/bash
# Install LLVM Flang/Clang VERSION and a qualified OpenMP fpm v0.13.0.
# Writes FPM_NATIVE to $GITHUB_ENV when that file is set.
set -euo pipefail
llvm=${1:?usage: ci/flang-setup.sh LLVM_VERSION}
curl --fail --location https://apt.llvm.org/llvm-snapshot.gpg.key |
  sudo tee /etc/apt/trusted.gpg.d/apt.llvm.org.asc >/dev/null
echo "deb https://apt.llvm.org/noble/ llvm-toolchain-noble-$llvm main" |
  sudo tee "/etc/apt/sources.list.d/llvm-$llvm.list"
sudo apt-get update
sudo apt-get install -y "flang-$llvm" "clang-$llvm" "libomp-$llvm-dev" gfortran
"flang-$llvm" --version

source_sha=001cff6cf1145f215baa0888ba27acc06cea30a89b79d1a8be97db3fffbc8cd2
binary_sha=bd2b6fe90f836cb4dc8d135dde97ceede6545133ed1d073c65da6951180cd1e5
prebuilt=/opt/ci-tools/fpm-v0.13.0-openmp
tool_dir=${RUNNER_TEMP:-/tmp}/fpm-v0.13.0-openmp
if test -x "$tool_dir/fpm" &&
   test "$(sha256sum "$tool_dir/fpm" | cut -d' ' -f1)" = "$binary_sha"; then
  echo 'fpm acquisition: already qualified in this job'
else
  rm -rf "$tool_dir"
  mkdir -p "$tool_dir"
  # The release's GCC12 binary is serial; qualify the image's dynamic build.
  if test -x "$prebuilt/fpm" && test -s "$prebuilt/compiler.txt" &&
     test -f "$prebuilt/fpm-0.13.0.F90" &&
     test "$(sha256sum "$prebuilt/fpm" | cut -d' ' -f1)" = "$binary_sha" &&
     test "$(sha256sum "$prebuilt/fpm-0.13.0.F90" | cut -d' ' -f1)" = "$source_sha"; then
    install -m 0555 "$prebuilt/fpm" "$tool_dir/fpm"
    test "$(sha256sum "$tool_dir/fpm" | cut -d' ' -f1)" = "$binary_sha"
    echo 'fpm acquisition: pinned Ubuntu GNU13 image build'
    cat "$prebuilt/compiler.txt"
  else
    echo 'fpm acquisition: exact official release source bootstrap'
    curl --fail --location --max-time 120 -o "$tool_dir/fpm-0.13.0.F90" \
      https://github.com/fortran-lang/fpm/releases/download/v0.13.0/fpm-0.13.0.F90
    echo "$source_sha  $tool_dir/fpm-0.13.0.F90" | sha256sum --check --strict -
    mkdir "$tool_dir/mods"
    gfortran --version
    # Official v0.13.0 README single-source recipe, with dynamic OpenMP.
    timeout 600s gfortran -O2 -fopenmp "$tool_dir/fpm-0.13.0.F90" \
      -J "$tool_dir/mods" -o "$tool_dir/fpm"
    chmod 0555 "$tool_dir/fpm"
  fi
fi
echo 'fpm source: fortran-lang/fpm v0.13.0 (90bb83a70e9bcf04d941fb43cca014ae1c0fc5ea)'
echo "fpm source SHA256: $source_sha"
sha256sum "$tool_dir/fpm"
"$tool_dir/fpm" --version
if [ -n "${GITHUB_ENV:-}" ]; then echo "FPM_NATIVE=$tool_dir/fpm" >> "$GITHUB_ENV"; fi
