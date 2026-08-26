#!/bin/sh

# Smoke test for the library build: samples/embed links libtrealla.a the
# way an embedder would and exercises the C API in src/trealla.h.
#
# Only the results table is compared. The engine prints its own answers
# while the checks run, and those are not worth pinning down here.
#
# Two defects in the embedding API used to be recorded here as "gap:"
# lines rather than checked: pl_query destroyed the parser holding the
# goal's strings, and get_status stayed false after a non-deterministic
# success. Both are fixed, so both are ordinary checks now.

if [ ! -x ./samples/embed ]
then
	echo "samples/embed not built"
	exit 1
fi

# The sample leaves a presentation-only blank line after the results. Keep the
# golden output focused on the table itself.
./samples/embed 2>&1 | sed -n '/=== results ===/,$p' | sed '${/^$/d;}'
