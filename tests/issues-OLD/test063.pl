:- initialization(main).

main :-
	a(X) =.. [Y|Z],
	writeq(Y), nl, writeq(Z), nl.
