:-initialization(main).

main :-
	[X|Y] = [a,b,X|X],
	write([X,Y]), nl.
