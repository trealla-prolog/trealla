:- use_module(library(clpz)).
:- initialization(main).

main :-
	#X #= #X * #Y, #X #= -1 * #Y.
