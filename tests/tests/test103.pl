:- initialization(main).

main :-
	F1=f(1,F2,3), F2=f(1,F1,3),
	ground(F1),
	cyclic_term(F1),
	term_variables(F1,[]),
	write(ok), nl.
