:-initialization(main).

xrevzap([], L, L) :- !.
xrevzap([H|L], L2, L3) :- xrevzap(L, [H|L2], L3).
xreverse(L1, L2) :- xrevzap(L1, [], L2).

main :- xreverse([a,b,c,d],L), write(L), nl, L=[d,c,b,a].
