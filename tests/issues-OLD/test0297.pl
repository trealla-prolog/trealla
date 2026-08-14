main :-
	A=A-A,
	term_variables([A,_X,_Y,_Z], L),
	write_term(L, [variable_names(['X'=_X, 'Y'=_Y, 'Z'=_Z])]), nl.

:- initialization(main).
