#!/bin/bash
# Nightly Flang triage, run inside GitHub Actions with GH_TOKEN set.
#   failure          find failing cells and the last green commit, bisect, report
#   success CELLS    close the branch's nightly-failure issue once CELLS cover it
#   step CELLS       one `git bisect run` step: any cell failure marks the commit bad
set -euo pipefail
repo=${GITHUB_REPOSITORY:?}
branch=${GITHUB_REF_NAME:?}
run_url="${GITHUB_SERVER_URL:-https://github.com}/$repo/actions/runs/${GITHUB_RUN_ID:-0}"
label=nightly-failure
title="Nightly failure on $branch"
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

open_issue() {
    gh issue list -R "$repo" --label "$label" --state open --limit 50 \
        --json number,title --jq ".[] | select(.title == \"$title\") | .number" | head -n 1
}

# Cells recorded in an issue's markers, one per line.
issue_cells() {
    gh issue view "$1" -R "$repo" --json body,comments \
        --jq '[.body] + [.comments[].body] | .[]' |
        grep -o 'nightly-failing-cells: [0-9,]*' | cut -d' ' -f2 | tr ',' '\n' |
        grep -v '^$' | sort -u
}

# Cell names `Flang NN bootstrap + consumers`; prints "NN URL" for failed ones.
failed_cells() {
    gh api "repos/$repo/actions/runs/$1/jobs" --paginate \
        --jq '.jobs[] | select((.conclusion == "failure" or .conclusion == "timed_out"
              or .conclusion == "cancelled") and (.name | test("^Flang [0-9]+ bootstrap")))
              | "\(.name | capture("^Flang (?<v>[0-9]+)").v) \(.html_url)"'
}

run_cells() {
    local cell
    for cell in $(echo "$1" | tr ',' ' '); do
        FO_SRC=$(pwd) "$tools/flang-cell.sh" "$cell" || return 1
    done
}

# Newest successful run at an ancestor of HEAD that ran all needed cells.
last_green() {
    local needed=$1 sha id title
    while read -r id sha title; do
        [ "$sha" != "$(git rev-parse HEAD)" ] || continue
        git merge-base --is-ancestor "$sha" HEAD 2>/dev/null || continue
        local covered
        covered=$(echo "$title" | sed -n 's/^nightly: Flang \([0-9,]*\)$/\1/p')
        local cell ok=1
        for cell in $(echo "$needed" | tr ',' ' '); do
            [[ ",$covered," == *",$cell,"* ]] || ok=0
        done
        [ "$ok" = 1 ] && { echo "$sha $id"; return; }
    done < <(gh run list -R "$repo" --workflow nightly.yml --status success --limit 50 \
        --json headSha,databaseId,displayTitle --jq '.[] | "\(.databaseId) \(.headSha) \(.displayTitle)"')
    # Before the first green nightly, merge-queue runs that still held the
    # Flang cells count.
    while read -r id sha; do
        git merge-base --is-ancestor "$sha" HEAD 2>/dev/null || continue
        local missing
        missing=$(gh api "repos/$repo/actions/runs/$id/jobs" --jq "[.jobs[] |
            select((.name | test(\"^Flang [0-9]+ bootstrap\")) and .conclusion == \"success\")
            | .name | capture(\"^Flang (?<v>[0-9]+)\").v] as \$ok |
            \"$needed\" | split(\",\") | map(select(. as \$c | \$ok | index(\$c) | not)) | length")
        [ "$missing" = 0 ] && { echo "$sha $id"; return; }
    done < <(gh run list -R "$repo" --workflow ci.yml --event merge_group --status success \
        --limit 30 --json headSha,databaseId --jq '.[] | "\(.databaseId) \(.headSha)"')
}

case "${1:-}" in
step)
    run_cells "${2:?}" || exit 1
    ;;
failure)
    mapfile -t failed < <(failed_cells "${GITHUB_RUN_ID:?}")
    [ "${#failed[@]}" -gt 0 ] || { echo 'no failed Flang cell'; exit 0; }
    cells=$(printf '%s\n' "${failed[@]}" | cut -d' ' -f1 | sort -un | paste -sd, -)
    head=$(git rev-parse HEAD)
    # Bisect steps check out older trees; keep this revision's tools.
    tools=$RUNNER_TEMP/nightly-tools
    mkdir -p "$tools"
    cp "$here/flang-cell.sh" "$here/nightly-triage.sh" "$tools/"
    for cell in $(echo "$cells" | tr ',' ' '); do "$here/flang-setup.sh" "$cell"; done
    FPM_NATIVE=$(grep '^FPM_NATIVE=' "$GITHUB_ENV" | tail -n 1 | cut -d= -f2-)
    export FPM_NATIVE tools
    green=$(last_green "$cells")
    finding=''
    if [ -z "$green" ]; then
        finding='No earlier green run covers these cells, so there is no range to bisect.'
    else
        good=${green%% *}
        git checkout -q "$good"
        if ! run_cells "$cells"; then
            finding="The cells also fail at the last green commit $good now, so the cause is outside the repository (toolchain, runner or network); no bisect."
        else
            git checkout -q "$head"
            git bisect start "$head" "$good"
            git bisect run "$tools/nightly-triage.sh" step "$cells" | tee "$RUNNER_TEMP/bisect.log"
            bad=$(sed -n 's/^\([0-9a-f]\{40\}\) is the first bad commit$/\1/p' "$RUNNER_TEMP/bisect.log" | head -n 1)
            git bisect reset > /dev/null
            if [ -n "$bad" ]; then
                pr=$(gh api "repos/$repo/commits/$bad/pulls" --jq '.[0].number // empty' || true)
                finding="First bad commit: $bad${pr:+ (#$pr)}, found by bisecting $good..$head."
            else
                finding="Bisecting $good..$head did not isolate a first bad commit; see the run log."
            fi
        fi
        git checkout -q "$head"
    fi
    {
        echo "The nightly Flang run failed at $head on \`$branch\`."
        echo
        echo "Failing cells:"
        printf '%s\n' "${failed[@]}" | while read -r cell url; do echo "- Flang $cell: $url"; done
        echo
        echo "$finding"
        echo
        echo "Run: $run_url"
        echo
        echo "<!-- nightly-failing-cells: $cells -->"
    } > "$RUNNER_TEMP/nightly-report.md"
    cat "$RUNNER_TEMP/nightly-report.md"
    issue=$(open_issue)
    if [ -n "$issue" ]; then
        gh issue comment "$issue" -R "$repo" --body-file "$RUNNER_TEMP/nightly-report.md"
    else
        gh issue create -R "$repo" --title "$title" --label "$label" \
            --body-file "$RUNNER_TEMP/nightly-report.md"
    fi
    ;;
success)
    passed=",${2:?},"
    issue=$(open_issue)
    [ -n "$issue" ] || { echo 'no open nightly-failure issue'; exit 0; }
    for cell in $(issue_cells "$issue"); do
        [[ "$passed" == *",$cell,"* ]] || { echo "cell $cell not rerun; issue stays open"; exit 0; }
    done
    gh issue close "$issue" -R "$repo" \
        --comment "All recorded cells pass again at $(git rev-parse HEAD): $run_url"
    ;;
*)
    echo 'usage: ci/nightly-triage.sh failure | success CELLS | step CELLS' >&2
    exit 2
    ;;
esac
