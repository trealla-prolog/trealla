:- initialization(main).
:- use_module(library(clpz)).

main :-
	4 #= 4 #<==> #B,
	write(B), nl.
