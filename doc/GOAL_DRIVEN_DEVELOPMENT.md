# Goals and architectural freedom

## Decisions

Issues and plans specify desired behavior, independent evidence and essential
compatibility constraints. The implementing agent chooses the smallest adequate
design. Make architectural decisions as soon as required and as late as possible.
Change an earlier decision when an observed problem warrants it.

Module names, directory layouts, internal representations, service lists,
extraction sequences and proposed algorithms are suggestions unless they are
part of an existing public contract. They are not completion requirements.
Do not delay working behavior to complete speculative architecture. Use Sol for
a demonstrated design bottleneck under the current controller/escalation rules;
this is not mandatory architecture review before every increment.

## Less maintained code

[Fo #205](https://github.com/lazy-fortran/fo/issues/205) is a major delivery goal:
substantially reduce maintained production, test/support and documentation code
while keeping features, correctness, reliability and fast feedback.

Prefer deletion and consolidation when they remove a demonstrated maintenance
burden. Count the whole affected stack: relocating code or replacing readable
code with dense/minified code does not achieve this goal. Record a fixed
baseline, net physical/non-comment line change and responsibilities removed.
Preserve useful independent failure detection when reducing tests. Do not add
tests that enforce document wording, repository layout or line counts.

## Fast delivery

Use the working resident Fo Gremlin lane while developing the tools themselves.
Run the affected/reproducer gate through the exact candidate driver. The
controller publishes small locally verified increments promptly; background
coverage and GitHub CI continue independently. Benchmarks and third-party
audits are optional evidence, never prerequisites for ordinary development.

Only actual consumer blockers delay the next consumer task. A cleanup goal does
not justify holding useful work behind the entire architecture backlog.

## Correctness and scope

Existing accepted language, public API/ABI and scientific contracts remain
authoritative. Invalid input, unsupported behavior, partial coverage and
infrastructure failure stay explicit. Preserve exact generation evidence,
cache freshness, useful completed receipts and unrelated processes/state.

Ordinary one-shot and continuous commands should share useful implementation
and agree on equivalent behavior. CLI and MCP remain first-class interfaces.
No agent/CI scheduler, generic OS sandbox or host environment manager is implied.

## Issue and plan format

Keep each active issue to its goal, observable success, indispensable boundary,
minimal reproducer/reference and concise current evidence where needed. Use an
existing focused oracle when it establishes the claim; add an independent one
for a genuine gap. Split work only when the goals can be delivered independently.

Plans order current goals and link their owners. They do not repeat issue
specifications or freeze future implementation choices. Keep historical evidence
at immutable references. Preserve issue states, labels and assignments when
rewriting plans; documentation changes do not claim implementation completion.
