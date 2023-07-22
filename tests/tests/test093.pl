:-initialization(main).

main :-
	write_term([[_|_]|_],[max_depth(1)]), nl,
	write_term(+A - +B,[max_depth(2)]), nl,
	write_term(+A - +B - +C,[max_depth(2)]), nl,
	write_term(-_* -_,[max_depth(2)]), nl,
	write_term([]*[]*[],[max_depth(3)]), nl,
	write_term_to_chars([1|(A*[[]*B])],[quoted(true),max_depth(5)],K),
	X = (A =:= -B-1), write(X), nl,
	true.
