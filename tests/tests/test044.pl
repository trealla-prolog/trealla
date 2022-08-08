:- initialization(main).

foo(L) :- [a|_] = L, write('foo'), nl.
bar([X|L]) :- foo([X|L]).

main :- bar([a]).
