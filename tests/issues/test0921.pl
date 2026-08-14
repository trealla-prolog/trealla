:- initialization(main).

main :-
	term_singletons(a(_A,_B,_A), L),
	length(L, 1),
	write(ok), nl.

