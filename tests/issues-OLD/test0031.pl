:-initialization(main).

main :-
	X = f(X), X == X,
	write_term(X,[max_depth(5)]), nl.
