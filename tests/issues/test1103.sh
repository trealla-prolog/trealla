#!/bin/sh

# Regression for issue #1103: the toplevel must choose "..." notation from
# the *content* of a list plus the double_quotes flag in force, never from
# how the term was built. Two properties:
#
#   P1  A == B  implies  A and B are written identically.
#       (a packed string from atom_codes/2 vs the same list as cons cells)
#   P2  a written answer, read back under the same flag, is the same term.
#
# Both failed before: under double_quotes=chars a codes list was written
# "\x0\", which reads back as ['\x0\'], and under double_quotes=atom every
# packed string was written "..." , which reads back as an atom.
#
# See https://github.com/trealla-prolog/trealla/issues/1103

TPL=${TPL:-./tpl}
TMPPL=tmp.pl

trap 'rm -f "$TMPPL"' EXIT
touch "$TMPPL"

$TPL -q -f --autofail "$TMPPL" <<'EOF'
set_prolog_flag(double_quotes,chars).
atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.
atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Cs = "\x0\".
atom_codes(A,[97,98]), atom_codes(A,Cs).
atom_chars(A,[a,b]), atom_chars(A,Cs).
X = [0'a,0'b].
X = [a,b].
X = "ab".
X = [a,b|T].
set_prolog_flag(double_quotes,codes).
atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.
atom_codes(A,[97,98]), atom_codes(A,Cs).
atom_chars(A,[a,b]), atom_chars(A,Cs).
X = [0'a,0'b].
X = [a,b].
set_prolog_flag(double_quotes,atom).
atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.
atom_codes(A,[97,98]), atom_codes(A,Cs).
atom_chars(A,[a,b]), atom_chars(A,Cs).
X = [a,b].
X = "ab".
halt.
EOF
