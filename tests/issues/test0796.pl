:- initialization(main).

main :-
	number_chars(N1,"%$$$\n1"), write(N1), nl,
	number_chars(N2,"/*$$$*/-2"), write(N2), nl.
