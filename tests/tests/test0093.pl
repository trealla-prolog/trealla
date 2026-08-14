:-initialization(main).

main :-
	write_term([[h|t]|r],[max_depth(1)]), nl,
	write_term(+A - +B,[max_depth(2),variable_names(['A'=A, 'B'=B])]), nl,
	write_term(+A - +B - +C,[max_depth(2),variable_names(['A'=A, 'B'=B, 'C'=C])]), nl,
	write_term(-D* -E,[max_depth(2),variable_names(['D'=D, 'E'=E])]), nl,
	write_term([]*[]*[],[max_depth(3)]), nl,
	write_term_to_chars([1|(A*[[]*B])],[quoted(true),max_depth(5)],K),
	X = (A =:= -B-1), write_term(X, [variable_names(['A'=A, 'B'=B])]), nl,
	true.
