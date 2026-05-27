:- use_module(library(clpb)).
:- initialization(main).

main :-
	( sat(A+B), A=B -> format("A=~w B=~w~n", [A,B]) ; write('unexpected failure'), nl ).
