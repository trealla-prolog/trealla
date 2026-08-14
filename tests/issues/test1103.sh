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

run() {
	printf '%s\n' "$1" | "$TPL" -q -f --autofail
}

run_with_flag() {
	printf '%s\n' "$2" | "$TPL" -q -f --autofail \
		-g "set_prolog_flag(double_quotes,$1)"
}

run 'set_prolog_flag(double_quotes,chars).'
run_with_flag chars 'atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.'
run_with_flag chars 'atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Cs = "\x0\".'
run_with_flag chars 'atom_codes(A,[97,98]), atom_codes(A,Cs).'
run_with_flag chars 'atom_chars(A,[a,b]), atom_chars(A,Cs).'
run_with_flag chars "X = [0'a,0'b]."
run_with_flag chars 'X = [a,b].'
run_with_flag chars 'X = "ab".'
run_with_flag chars 'X = [a,b|T].'

run 'set_prolog_flag(double_quotes,codes).'
run_with_flag codes 'atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.'
run_with_flag codes 'atom_codes(A,[97,98]), atom_codes(A,Cs).'
run_with_flag codes 'atom_chars(A,[a,b]), atom_chars(A,Cs).'
run_with_flag codes "X = [0'a,0'b]."
run_with_flag codes 'X = [a,b].'

run 'set_prolog_flag(double_quotes,atom).'
run_with_flag atom 'atom_codes(A,[0]), atom_codes(A,Cs), Cs = [Null], Ds = [Null], Cs = Ds.'
run_with_flag atom 'atom_codes(A,[97,98]), atom_codes(A,Cs).'
run_with_flag atom 'atom_chars(A,[a,b]), atom_chars(A,Cs).'
run_with_flag atom 'X = [a,b].'
run_with_flag atom 'X = "ab".'
