:- initialization(main).

main :-
	[X,Y] = [x,[X|Y]],
	write([X,Y]), nl.

