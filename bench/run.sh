#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKLOADS="$SCRIPT_DIR/workloads"
REPS="${BENCH_REPS:-7}"
RESULTS_FILE="${BENCH_OUTPUT:-/dev/stdout}"

command -v fo >/dev/null 2>&1 || { echo '{"error":"fo not found in PATH"}'; exit 1; }
command -v jq >/dev/null 2>&1 || { echo '{"error":"jq not found in PATH"}'; exit 1; }

median() {
    sort -n | awk '{a[NR]=$1} END {
        if (NR%2==1) print a[(NR+1)/2]
        else print (a[NR/2]+a[NR/2+1])/2
    }'
}

bench_one() {
    local workload="$1" tool="$2" metric="$3"
    shift 3
    local times=()
    local t0 t1 dt

    for _ in $(seq 1 "$REPS"); do
        t0=$(date +%s%N)
        "$@" >/dev/null 2>&1 || true
        t1=$(date +%s%N)
        dt=$(awk "BEGIN {printf \"%.6f\", ($t1-$t0)/1000000000}")
        times+=("$dt")
    done

    local med
    med=$(printf '%s\n' "${times[@]}" | median)
    printf '{"case":"%s","tool":"%s","metric":"%s","median_s":%s,"n":%d}\n' \
        "$workload" "$tool" "$metric" "$med" "$REPS"
}

emit() {
    if [ "$RESULTS_FILE" = "/dev/stdout" ]; then
        cat
    else
        tee -a "$RESULTS_FILE"
    fi
}

if [ "$RESULTS_FILE" != "/dev/stdout" ]; then
    : > "$RESULTS_FILE"
fi

# --- many_tests ---
if [ -d "$WORKLOADS/many_tests" ]; then
    cd "$WORKLOADS/many_tests"
    fo build >/dev/null 2>&1 || true

    bench_one many_tests fo check_json fo check --json | emit
    bench_one many_tests fo test fo test | emit
    bench_one many_tests fo check fo check | emit
    cd "$SCRIPT_DIR/.."
fi

# --- bigmod ---
if [ -d "$WORKLOADS/bigmod" ]; then
    cd "$WORKLOADS/bigmod"
    fo build >/dev/null 2>&1 || true

    bench_one bigmod fo check_json fo check --json | emit
    bench_one bigmod fo build fo build | emit

    # incremental: touch a leaf and rebuild
    bench_one bigmod fo incremental_leaf \
        bash -c 'touch src/leaf_1.f90 && fo build' | emit
    bench_one bigmod fo incremental_core \
        bash -c 'touch src/core.f90 && fo build' | emit

    cd "$SCRIPT_DIR/.."
fi

# --- diagnostics ---
if [ -d "$WORKLOADS/diagnostics" ]; then
    cd "$WORKLOADS/diagnostics"

    bench_one diagnostics fo diag_latency fo check --json | emit
    cd "$SCRIPT_DIR/.."
fi
