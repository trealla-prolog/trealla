:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(X,writeln(hello)),
	freeze(Y,writeln(world)),
	duplicate_term(X,Y),
	X=x,
	Y=y.
