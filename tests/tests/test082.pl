main :-
	L = [A,B,C| L], copy_term_nat(L,V), V=[D,E,F|T], T == V,
	write_term(L, [quoted(true),variable_names(['A'=A, 'B'=B, 'C'=C])]), nl,
	write_term(V, [quoted(true),variable_names(['D'=D, 'E'=E, 'F'=F])]), nl,
	L = V.

:- initialization(main).
