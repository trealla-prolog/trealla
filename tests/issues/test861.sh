#!/bin/sh

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
EOF

$TPL -f $TMPPL >$TMP <<EOF
member(X,X), !.
halt.
EOF

