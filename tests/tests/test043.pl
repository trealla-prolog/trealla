:- initialization(main).

foo(L, _X) :- [a|L1] = L, [_Y|_L2] = L1.
bar(L, _X) :- foo(L, _Y).
baz(L) :- bar(L, _X).

main :- baz([a]).
main.
