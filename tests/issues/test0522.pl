:- use_module(library(clpz)).
:- initialization(main).

main :-
	4 #= 4 #<==> #B,
	write(B), nl,
	maplist(#=(3),[1,2,3],Bs).

