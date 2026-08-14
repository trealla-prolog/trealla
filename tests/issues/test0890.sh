#!/bin/sh

# Regression for issue #890: cyclic chars-lists must rightsplice at
# named (and anon) spine variables when dumped at the toplevel, e.g.
#   L = "ab"||I, I = "cd"||L.
# rather than self-closing as L = "ab"||L, I = "cd"||I.
#
# Covers the queries from https://github.com/trealla-prolog/trealla/issues/890

TPL=${TPL:-./tpl}
TMPPL=tmp.pl

trap 'rm -f "$TMPPL"' EXIT
touch "$TMPPL"

$TPL -q -f --autofail "$TMPPL" <<'EOF'
L=[a|[b|I]], I=[c|[d|L]].
L=[a|[b|I]], I=[c|[d|L]], I=[_,_,Ia,Ib,_,_|_], L=[_,_,Lc,Ld,_,_|_].
I=[_,_,Ia,Ib,_,_|_], L=[_,_,Lc,Ld,_,_|_], L=[a|[b|I]], I=[c|[d|L]].
I=[_,_,Ia,Ib,_,_|X], L=[_,_,Lc,Ld,_,_|Y], L=[a|[b|I]], I=[c|[d|L]].
I=[_,_,Ia,Ib|X], L=[_,_,Lc,Ld,_,_,_,_|Y], L=[a|[b|I]], I=[c|[d|L]].
I=[_,_,Ia,Ib,_,_,_,_|X], L=[_,_,Lc,Ld|Y], L=[a|[b|I]], I=[c|[d|L]].
I=[_|X], L=[_|Y], L=[a|[b|I]], I=[c|[d|L]].
I=[_|_], L=[_|_], L=[a|[b|I]], I=[c|[d|L]].
I=[_|F], L=[_|F], L=[a|[b|I]], I=[c|[d|L]].
I=[_,_,_|F], L=[_|F], L=[a|[b|I]], I=[c|[d|L]].
L=[a|[b|L]].
L=[a,b|X].
X=[a|Y], Y=[b|X].
between(0,5,N), length(M,N), append(M,[_|X],I), L=[_|Y], L=[a|[b|I]], I=[c|[d|L]].
halt.
EOF
