:- initialization(main).

main :-
	a(X) =.. [Y|Z],
	write_term(Y, [quoted(true),variable_names(['X'=X])]), nl,
	write_term(Z, [quoted(true),variable_names(['X'=X])]), nl.
