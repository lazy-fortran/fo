# Fo roadmap

[PLAN.md](PLAN.md) is the sole delivery order.
[Goals and architectural freedom](doc/GOAL_DRIVEN_DEVELOPMENT.md) governs how
agents choose and revise implementations. Issues specify outcomes and evidence,
not required internal layouts.

## Destination

Fast, reliable local Fortran development through ordinary commands and resident
Gremlin, with equivalent CLI/MCP interfaces, correct incremental reuse, truthful
current-generation verification and substantially less maintained code.

The immediate consumer is Fo itself, followed by FFC. Standalone FPM and the
maintained ITpPlasma CMake/CTest subset follow those usable development loops.

## Major goals

- [#200](https://github.com/lazy-fortran/fo/issues/200): sustain actual resident
  self-development and current focused gate evidence.
- [#205](https://github.com/lazy-fortran/fo/issues/205): substantially reduce
  production/test/documentation volume while preserving features/correctness.
- #175/#189/#138: trustworthy relevant inputs, affected-first testing and finite
  remaining coverage independent of cache warmth.
- #139/#142/#151/#155/#183: reliable ownership, exact drivers, reconnectable
  interfaces, quiet completion and recovered evidence.
- #165–#170: reusable, inexpensive and independent build/test/storage behavior.
- #149/#150/#161/#163/#184–#186: remove duplicated and obsolete implementation
  and test machinery as concrete maintenance burdens are established.
- #201–#204: initial standalone FPM compatibility goals; remaining parity is
  still required and unsupported forms must be explicit.
- #192–#198: native required-profile compatibility; #199 is optional optimization.
- #145/#190: optional independent audits, outside the development gate.

Adjacent public-command defects retain their own issue scope and can proceed
independently. They do not become prerequisites merely by appearing here.
Historical specifications/evidence remain at
[the earlier roadmap](https://github.com/lazy-fortran/fo/blob/f74daf7/ROADMAP.md).
