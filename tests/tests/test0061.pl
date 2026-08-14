:- initialization(main).

main :-
	read_term_from_chars("[1,2,3]", Term, []),
	write(Term), nl.
