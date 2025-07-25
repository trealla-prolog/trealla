#!/bin/sh

TMP=tmp.out
TMPPL=tmp.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<EOF
p(X,Y) :- X = f(f(f(X))), Y = f(f(Y)).
p(X,Y) :- X = a(f(X,Y)), Y = b(g(X,Y)).
p(X,Y) :- X = s(s(X,Y),_), Y = s(Y,X).
EOF

$TPL --autofail $TMPPL >$TMP <<EOF
p(X,Y).
halt.
EOF

