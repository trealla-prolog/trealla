:- initialization(main).
:- use_module(library(clpz)).

main :-
	#X #= #X * #Y, #X #= -1 * #Y.
