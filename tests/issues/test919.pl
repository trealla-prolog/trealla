:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(X,writeln(hello)),
	freeze(Y,writeln(world)),
	copy_term(X,Y),
	X=x,
	Y=y.
