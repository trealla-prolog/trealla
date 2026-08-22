#!/bin/sh

# Phase 0 acceptance for library(janus). Needs a binary built with
# `make janus`; a default build has no janus module at all, which is
# the point, so this is not part of tests/run.sh.

TPL=${TPL:-./tpl}

$TPL -q -f tests/janus/phase0.pl -g main
rc=$?
echo "exit status $rc"
