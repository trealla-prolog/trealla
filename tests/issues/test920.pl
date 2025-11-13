:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(X,Z=2),
	freeze(Z,writeln(here)),
	copy_term(X,Y),
	Y=1.
