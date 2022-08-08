:- use_module(library(freeze)).


main :-
	freeze(V1, W1=2),
	freeze(V2, W2=3),
	V1 = V2,
	(V1 = 1; V1 = -1),
	writeq([V1,V2,W1,W2]), nl,
	fail.
main.

:- initialization(main).
