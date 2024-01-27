:- initialization(main).
:- use_module(library(freeze)).

main :-
	freeze(X,integer(X)),freeze(Z,((freeze(_,I))=Z)),frozen(X,Z), X=1,
	write(X), nl, write(Z), nl, write(I), nl.
