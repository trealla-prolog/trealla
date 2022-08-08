:-initialization(main).

main :-
	srandom(1000),
	X is rand mod 32,
	write(X), nl,
	srandom(1000),
	Y is rand mod 2^5,
	write(Y), nl.
