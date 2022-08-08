% Fast Fourier Transform
% Code from the book "Clause and Effect" Chapter 10

:- use_module(library(lists)).

test :-
    fft([0,1,2,3,4,5,6,7], X),
    write(X), nl.

testq :-
    between(1, 1000,_),
		fft([0,1,2,3,4,5,6,7], _),
		fail.
testq.

fft(A, L) :-
    eval(p(A, w^0), X0, 8),
    eval(p(A, w^1), X1, 8),
    eval(p(A, w^2), X2, 8),
    eval(p(A, w^3), X3, 8),
    eval(p(A, w^4), X4, 8),
    eval(p(A, w^5), X5, 8),
    eval(p(A, w^6), X6, 8),
    eval(p(A, w^7), X7, 8),
    gen((X0;X1;X2;X3;X4;X5;X6;X7), []-L, _).

eval(p([I], _), a(I), _).
eval(p(L, V^P), A1+V^P*A2, N) :-
    alternate(L, L1, L2),
    P1 is (P*2) mod N,
    eval(p(L1, V^P1), A1, N),
    eval(p(L2, V^P1), A2, N).

alternate([], [], []).
alternate([A, B|T], [A|T1], [B|T2]) :-
    alternate(T, T1, T2).

% gen(InTree, ListOutFront-ListOutBack, NodeIndex)
gen(X+Y, L0-L3, A) :-
    !,
    gen(X, L0-L1, A1),
    gen(Y, L1-L2, A2),
    node(n(A, op(+, A1, A2)), L2-L3).
gen(X*Y, L0-L3, A) :-
    !,
    gen(X, L0-L1, A1),
    gen(Y, L1-L2, A2),
    node(n(A, op(*, A1, A2)), L2-L3).
gen((X;Y), L0-L2, _) :-
    !,
    gen(X, L0-L1, _),
    gen(Y, L1-L2, _).
gen(X, L0-L1, A) :-
    node(n(A, X), L0-L1).

% node(TryNode, OutDiffList)
node(n(1, N), []-[n(1, N)]) :-
    !.
node(N, L-L) :-
    memberchk(N, L),
    !.
node(n(A1, N1), [n(A, N)|T]-[n(A1, N1), n(A, N)|T]) :-
    A1 is A+1.
