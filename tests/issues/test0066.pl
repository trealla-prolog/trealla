:- initialization(main).
:- use_module(library(freeze)).

main :-
	freeze(X, (X > 3, X < 5)),
	findall(X, between(1, 20, X), Xs),
	write(Xs), nl.

