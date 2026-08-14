:- initialization(main).
:- use_module(library(lists)).

main :-
	append([a,b],[c,d],L), write(L), nl,
	append(L1,L2,[a,b,c,d]), write(L1), write(' <==> '), write(L2), nl, fail.
main.
