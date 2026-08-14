:- initialization(main).

main :-
	T=f(X),write_term(T,[quoted(true),variable_names(['X'=X])]), nl,
	true.
