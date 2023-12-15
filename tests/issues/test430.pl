:- use_module(library(clpz)).
:- initialization(main).

main :-
	2 #> 1,
	1 #> 1 -> false; true,
	\+ (1 #> 1).

