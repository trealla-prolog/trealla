:- initialization(main).

main :-
	\+ unify_with_occurs_check(L,[_|L]),
	\+ unify_with_occurs_check([_|L],L),
	write(ok), nl.
