#!/bin/sh

# Smoke test for the library build: samples/embed links libtrealla.a the
# way an embedder would and exercises the C API in src/trealla.h.
#
# Only the results table is compared. The engine prints its own answers
# while the checks run, and those are not worth pinning down here.
#
# The two "gap:" lines record defects in the embedding API. If one is
# fixed, its line changes to "looks fixed" and this test fails - that is
# deliberate, and the fix should come with an update to embed.expected.

if [ ! -x ./samples/embed ]
then
	echo "samples/embed not built"
	exit 1
fi

./samples/embed 2>&1 | sed -n '/=== results ===/,$p'
