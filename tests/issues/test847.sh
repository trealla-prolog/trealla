#!/bin/sh
TMP=tmp.out
TMPPL=tmp.pl

cat >$TMPPL <<EOF
EOF

$TPL $TMPPL <<EOF >$TMP
length(L,3).
halt.
EOF

rm $TMPPL
