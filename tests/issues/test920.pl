:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(X,Z=2),
	freeze(Z,writeln(here)),
	duplicate_term(X,Y),
	Y=1.
