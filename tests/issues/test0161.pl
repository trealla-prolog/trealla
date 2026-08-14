:- initialization(main).
:- use_module(library(freeze)).

main :-
	freeze(X,integer(X)), X=1, write(X), nl.
