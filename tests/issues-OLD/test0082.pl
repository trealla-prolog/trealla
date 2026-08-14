:-initialization(main).

main :-
	srandom(1000),
	X is rand mod 32,
	srandom(1000),
	Y is rand mod 2^5,
	X =:= Y,
	write(ok), nl.
