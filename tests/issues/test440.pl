:- initialization(main).

main :-
	Y =.. [x,[Head|Y]],
	write_term(Y, [variable_names(['Head'=Head, 'Y'=Y])]), nl.
