:- initialization(main).
:- use_module(library(lists)).

main :-
	prepare(List),
    writeq(List), nl,
    sort(List, ListSorted),
    writeq(ListSorted), nl.

prepare(List) :-
    append([A,B], [B,A], List).
