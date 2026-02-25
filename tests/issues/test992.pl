:- initialization(main).

main :-
	T=f(X),N='Bad',write_term(T,[quoted(true),variable_names([N=T])]), nl,
	true.
