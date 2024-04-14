:- initialization(main).
:- use_module(library(clpz)).

main :-
	4 #= 4 #<==> #B,
	write(B), nl,
	maplist(#=(3),[1,2,3],Bs).

