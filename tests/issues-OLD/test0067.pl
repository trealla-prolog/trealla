:- initialization(main).
:- use_module(library(lists)).

main :-
	prepare(List, A, B),
	write_term(List, [quoted(true),variable_names(['A'=A, 'B'=B])]), nl,
    sort(List, ListSorted),
	write_term(ListSorted, [quoted(true),variable_names(['A'=A, 'B'=B])]), nl.

prepare(List, A, B) :-
    append([A,B], [B,A], List).
