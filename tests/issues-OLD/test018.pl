:- initialization(main).
:- use_module(library(dcgs)).

main :-
	phrase([], Ls), write(Ls), nl,
	phrase([a], Ls), write(Ls), nl,
	!.
main.
