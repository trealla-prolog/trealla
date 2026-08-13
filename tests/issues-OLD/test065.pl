:- initialization(main).

:- use_module(library(lists)).

main :-
	setof(I, member(I, [A,B,B,A]), Set), Set = [S1,S2], write_term(Set, [quoted(true),variable_names(['S1'=S1, 'S2'=S2])]), nl,
	bagof(I, member(I, [A,B,B,A]), Bag), Bag = [B1,B2,B3,B4], write_term(Bag, [quoted(true),variable_names(['B1'=B1, 'B2'=B2, 'B3'=B3, 'B4'=B4])]), nl.
