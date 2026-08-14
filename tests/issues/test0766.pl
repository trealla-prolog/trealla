:- initialization(main).

main :-
	number_chars(N0,"0'''"),
	write(N0), nl,
	number_chars(N1,"0''").
