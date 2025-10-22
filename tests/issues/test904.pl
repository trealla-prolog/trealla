:- initialization(main).

main :-
	number_chars(N1,"1_%E\n1"), write(N1), nl,
	number_chars(N2,"2_/*E*/\n2"), write(N2), nl.
