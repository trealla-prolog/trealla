main :-
	A=A-A,
	term_variables([A,_X,_Y,_Z], L),
	write(L), nl.

:- initialization(main).
