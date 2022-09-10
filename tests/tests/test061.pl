:- initialization(main).

main :-
	read_term_from_chars(Term, [], "[1,2,3]"),
	write(Term), nl.
