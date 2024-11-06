:- initialization(main).

:- use_module(library(lists)).

main :-
    maplist(col([[1,2],[3,4]]), [1,2], X1),
    writeq(X1), nl,
    maplist(col([[A,2],[3,A]]), [1,2], X2),
    write(X2), nl,
    maplist(length, M, [2,2]),
    writeq(M), nl,
    maplist(col(M), [1,2], X),
    writeq(X), nl.

col(Matrix, N, Column) :-
    maplist(nth1(N), Matrix, Column).
