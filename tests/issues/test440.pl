:- initialization(main).

main :-
	Y =.. [x,[_|Y]],
	write(Y), nl.

