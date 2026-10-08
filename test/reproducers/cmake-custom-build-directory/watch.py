#!/usr/bin/env python3
"""Independent Linux native watch oracle: output, authored sibling, nested provider."""
import ctypes as c
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import time

source = Path(sys.argv[1]).resolve()
root = Path(sys.argv[2]).resolve()
root.mkdir()
project = root / 'project'
output = project / 'out/native'
provider = output / 'provider'
authored = project / 'out/authored'
for p in (provider, authored):
    p.mkdir(parents=True)
for p in (provider / 'input.dat', authored / 'input.dat'):
    p.write_text('known17\n')
libfile = root / 'watch.so'
subprocess.run(['gcc', '-shared', '-fPIC', str(source), '-o', str(libfile)], check=True)
lib = c.CDLL(str(libfile))
lib.fo_change_native_open.argtypes = [c.POINTER(c.c_int)]
lib.fo_change_native_open.restype = c.c_void_p
for name in ('fo_change_native_root', 'fo_change_native_exclude_root'):
    getattr(lib, name).argtypes = [c.c_void_p, c.c_char_p]
for name in ('fo_change_native_reconcile', 'fo_change_native_pending'):
    getattr(lib, name).argtypes = [c.c_void_p]
lib.fo_change_native_poll.argtypes = [c.c_void_p, c.c_int, c.c_char_p, c.c_int,
                                    c.POINTER(c.c_int)]
lib.fo_change_native_close.argtypes = [c.c_void_p]
err = c.c_int()
handle = lib.fo_change_native_open(c.byref(err))
assert handle and err.value == 0
rows = []

def poll(seconds):
    deadline = time.monotonic()+seconds
    events = []
    while time.monotonic() < deadline:
        text = c.create_string_buffer(4096)
        kind = c.c_int()
        assert lib.fo_change_native_poll(handle, 20, text, 4096, c.byref(kind)) == 0
        if kind.value:
            events.append({'path': text.value.decode(), 'kind': kind.value})
    return events

try:
    assert lib.fo_change_native_exclude_root(handle, str(output).encode()) == 0
    assert lib.fo_change_native_root(handle, str(project).encode()) == 0
    assert lib.fo_change_native_root(handle, str(provider).encode()) == 0
    assert lib.fo_change_native_reconcile(handle) == 0
    poll(0.3)
    (output / 'generated.f90').write_text('compiler output is not authored input\n')
    events = poll(0.4)
    rows.append({'phase': 'output', 'events': events})
    assert not events, events
    (authored / 'input.dat').write_text('known23\n')
    events = poll(0.4)
    rows.append({'phase': 'authored-sibling', 'events': events})
    assert any(e['path'] == str(authored / 'input.dat') for e in events), events
    (provider / 'input.dat').write_text('known29\n')
    events = poll(0.4)
    rows.append({'phase': 'explicit-provider', 'events': events})
    assert any(e['path'] == str(provider / 'input.dat') for e in events), events
finally:
    lib.fo_change_native_close(handle)
    (root / 'receipt.json').write_text(json.dumps({'source': str(source),
        'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
        'rows': rows}, indent=2)+'\n')
print(json.dumps(rows))
