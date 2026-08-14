:- initialization(main).

main :-
	L = ""||K,
	write_term(L=K, [variable_names(['K'=K])]), nl.
