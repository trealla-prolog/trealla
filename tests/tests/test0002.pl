:-initialization(main).

h([H|T],L) :- L=[H|T].

main :- h([a,b,c,d],L), write(L), nl, L=[a,b,c,d].
