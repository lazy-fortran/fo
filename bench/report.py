#!/usr/bin/env python3
"""Summarize benchmark JSON lines: medians, targets, and pass/fail."""
import json
import sys

TARGETS = {
    ("many_tests", "check_json"): 0.100,
    ("many_tests", "check"): 0.500,
    ("bigmod", "check_json"): 0.100,
    ("bigmod", "incremental_leaf"): 0.200,
    ("diagnostics", "diag_latency"): 0.200,
}


def main():
    if len(sys.argv) < 2:
        print("usage: report.py <results.jsonl>", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    with open(path) as f:
        lines = [json.loads(line) for line in f if line.strip()]

    if not lines:
        print('{"error":"no results"}')
        sys.exit(1)

    all_pass = True
    print(f"{'case':<16} {'metric':<20} {'median_s':>10} {'target':>10} {'status':>8}")
    print("-" * 70)
    for entry in lines:
        case = entry["case"]
        metric = entry["metric"]
        median = entry["median_s"]
        target = TARGETS.get((case, metric))
        if target is not None:
            ok = median <= target
            status = "PASS" if ok else "FAIL"
            if not ok:
                all_pass = False
        else:
            status = "-"
        target_str = f"{target:.3f}" if target else "-"
        print(f"{case:<16} {metric:<20} {median:>10.3f} {target_str:>10} {status:>8}")

    print()
    if all_pass:
        print("All targets met.")
    else:
        print("Some targets exceeded.")
        sys.exit(1)


if __name__ == "__main__":
    main()
