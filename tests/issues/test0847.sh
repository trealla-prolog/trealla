#!/bin/sh

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
EOF

$TPL -f $TMPPL >$TMP <<EOF
length(L,3).
halt.
EOF

