:- initialization(main).
:- use_module(library(dif)).

main :-
	dif([],A), A=_*A,
	write(A), nl.

