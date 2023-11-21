:- use_module(library(freeze)).

main :-
	freeze(X,(write(here),nl)), X \= true.

:- initialization(main).
