# fs_probe

Standalone C probe (libc only) that characterises shared-filesystem semantics
relevant to a lock-free build cache that publishes by temp file + `rename(2)`
in one directory. It is a measurement helper, not part of the fo build.

```bash
cc -O2 -o fs_probe fs_probe.c
fs_probe local DIR      # one host: forks roles A and B
fs_probe A DIR          # host 1 } start both within 60 s,
fs_probe B DIR          # host 2 } same fresh DIR on the shared filesystem
```

`DIR` must exist and should be a fresh, empty directory. The roles coordinate
only through files `DIR/fsp.*` (barrier files polled with `open(2)`), so the
coordination itself is subject to the filesystem's visibility semantics; a
barrier timeout (exit 4) therefore means "peer not running or its files are not
visible within 20 s" and is itself a result.

Output is one `PROPERTY: value (evidence)` line per property: rename-over
atomicity under concurrent readers, create/rename/unlink visibility round trip,
`link(2)` same/cross directory, `O_CREAT|O_EXCL`, `link(2)` and
`renameat2(RENAME_NOREPLACE)` race exclusivity (claims are cross-checked with
the winner's file content), `flock` and `fcntl(F_SETLK)` conflicts across the
roles, mtime granularity, atime-on-read behaviour, peer `utimensat`
visibility, `readdir` consistency during concurrent create/unlink, and reads
through an open fd while the peer unlinks the file. Durability (fsync) is out
of scope.

Exit codes: 0 ok, 2 usage, 3 `DIR` not accessible or writable (e.g. a missing
AFS token), 4 barrier timeout, 124 per-role 100 s alarm. Every `fsp.*` file is
removed on exit, on SIGTERM/SIGINT and on the alarm (role A removes them after
the final barrier; directory listings are retried on transient errors).

Two hosts example (Slurm, compile per node in node-local scratch):

```bash
srun -N1 -n1 -w node1 sh -c 'cc -O2 -o /tmp/p fs_probe.c && /tmp/p A ~/run' &
srun -N1 -n1 -w node2 sh -c 'cc -O2 -o /tmp/p fs_probe.c && /tmp/p B ~/run'
```
