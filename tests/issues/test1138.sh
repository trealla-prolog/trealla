#!/bin/sh

# Issue #1138: a cyclic answer whose cycle starts *below* the reported
# variable was printed as though it closed on that variable, which says
# something else: S = [z|T], T = "abcdef"||T is not S = "zabcdef"||S,
# whose z repeats. dump_variable() named the loop after the variable
# being dumped whenever it could not name the entry itself, so the
# acyclic prefix was swallowed into the cycle.
#
# The bug is in what is *printed* - the terms themselves were always
# right - so this has to check the toplevel's text, as test0861.sh does.

TMPPL=tmp1138.pl

trap "rm -f $TMPPL" EXIT

cat >$TMPPL <<'EOF'
p1(S) :- T = [1,2|T], S = [9|T].
p2(S) :- T = "abcdef"||T, S = [z|T].
p3(S,U) :- T = [1,2|T], S = [9|T], U = [5|T].
p4(S) :- T = a(b(c(d(e(f(T)))))), S = z(T).
p5(S) :- T = f(T), S = z(T).
EOF

$TPL -q $TMPPL <<'EOF'
p1(S).
p2(S).
p3(S,U).
p4(S).
p5(S).
S = [9|S].
S = f(S).
T = f(T), S = z(T).
S = "ab"||S.
T = [1,2|T], S = [9|T].
member(X,X), !.
halt.
EOF
