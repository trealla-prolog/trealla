#!/bin/sh

# Acceptance for library(janus). Needs a binary built with `make janus`;
# a default build has no janus module at all, which is the point, so
# this is not part of tests/run.sh.
#
# Phase 0: build wiring, libpython discovery, shutdown hook.
# Phase 1: the bi-translation table, both directions.
# Phase 2: calling, keyword arguments, options, the GIL.
# Phase 3: iteration, dict access, sys.path.

TPL=${TPL:-./tpl}

# stdin closed throughout: a goal that fails or throws never reaches
# halt, and tpl would otherwise drop to the toplevel and sit waiting for
# input - which reads as a hang rather than a failure.

echo "=== phase 1: marshalling ==="
$TPL -q -f tests/janus/phase1.pl -g "main,halt" </dev/null

echo
echo "=== phase 2: calling ==="
$TPL -q -f tests/janus/phase2.pl -g "main,halt" </dev/null

echo
echo "=== phase 3: iteration, dicts, library paths ==="
$TPL -q -f tests/janus/phase3.pl -g "main,halt" </dev/null

echo
echo "=== phase 0: startup and shutdown ==="

# Last, because it halts with a non-zero status on purpose and takes the
# Python interpreter down with it.
# phase0's main halts with status 3 itself - that is what it tests.
$TPL -q -f tests/janus/phase0.pl -g main </dev/null
rc=$?
echo "exit status $rc"
