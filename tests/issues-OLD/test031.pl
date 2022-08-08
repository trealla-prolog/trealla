:-initialization(main).

main :-
	X = f(X), X == X,
	writeq(X), nl.
