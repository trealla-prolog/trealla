:- initialization(main).
:- use_module(library(lists)).

main :-
	findall(I, member(I, [A,B,B,A]), L),
	writeq(L), nl, fail.
main.

