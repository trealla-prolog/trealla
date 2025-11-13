:- initialization(main).

main :-
	term_singletons(a(_A,_B,_A), L),
	write(L), nl.

