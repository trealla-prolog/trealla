:- initialization(main).

main :-
	X = (A - =<(1,2) ),
	write_term(X, [variable_names(['A'=A])]), nl.
