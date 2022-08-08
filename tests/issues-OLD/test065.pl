:- initialization(main).

:- use_module(library(lists)).

main :-
	setof(I, member(I, [A,B,B,A]), Set), writeq(Set), nl,
	bagof(I, member(I, [A,B,B,A]), Bag), writeq(Bag), nl.
