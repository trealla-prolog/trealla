:- initialization(main).

main :-
	number_chars(N0,"'\\\n\\\n-\\\n'1"),
	write(N0), nl,
	number_chars(N1,"'-\\\n'1"),
	write(N1), nl.
