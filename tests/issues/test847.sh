#!/bin/sh

# set -e

TPL=./tpl

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
EOF

$TPL $TMPPL <<EOF >$TMP
length(L,3).
halt.
EOF

rm $TMPPL
