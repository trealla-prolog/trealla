#!/bin/sh

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
vx(X) :- -X=X.
EOF

$TPL $TMPPL >$TMP <<EOF
vx(Y), vx(Z), X = 1.
halt.
EOF
