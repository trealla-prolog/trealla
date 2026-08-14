:-initialization(main).

main :-
	(between(1, 1000, _),
		assertz(hello(there)), false) ;
		setof(X, hello(X), Ls),
		writeq(Ls), nl.
