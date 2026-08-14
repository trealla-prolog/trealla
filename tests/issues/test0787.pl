:- initialization(main).

main :-
	number_chars(N,"\'\\\n-\' 1"),
	write(N), nl.
