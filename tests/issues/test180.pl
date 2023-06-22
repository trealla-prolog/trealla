:-initialization(main).

main :-
	[X|Y] = [a,b,X|X],
	write(X), nl,
	write(Y), nl.
