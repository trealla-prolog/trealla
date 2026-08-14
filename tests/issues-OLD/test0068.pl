:- initialization(main).

main :-
	prepare(List, X, Y, Z),
	write_term(List, [quoted(true),variable_names(['X'=X, 'Y'=Y, 'Z'=Z])]), nl,
    sort(List, ListSorted),
	write_term(ListSorted, [variable_names(['X'=X, 'Y'=Y, 'Z'=Z])]), nl.

prepare([B,A], X, Y, Z) :-
    A =.. [pair,2,X],
    B =.. [trio,3,Y,Z].
