:- initialization(main).
:- use_module(library(dif)).

main :-
	dif([],A), A=_*A,
	write_term(A,[max_depth(5)]), nl.

