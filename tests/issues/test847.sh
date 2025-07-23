#!/bin/sh

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
EOF

$TPL $TMPPL >$TMP <<EOF
length(L,3).
halt.
EOF

rm $TMPPL
