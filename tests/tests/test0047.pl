:- initialization(main).

foo(A, B) :- A = 1, write(B), nl.

main :- foo(X, X).

