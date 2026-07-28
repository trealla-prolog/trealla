#!/bin/sh

# Compiles and runs tests/misc/skiplist.c, the unit tests for
# src/skiplist.c. run_misc.sh only knows how to run *.pl and *.sh, so
# this driver is what puts the C tests in the suite; skiplist.c itself
# is skipped by the loop.

set -e
CC=${CC:-cc}
DIR=$(dirname "$0")
OUT=$(mktemp -d)
trap 'rm -rf "$OUT"' EXIT

# -lm for log() in random_level(). No other objects: skiplist.c is
# #included by the test, which stubs the lock calls itself.
$CC -O1 -o "$OUT/skiplist" "$DIR/skiplist.c" -lm
"$OUT/skiplist"
