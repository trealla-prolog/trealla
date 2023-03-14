:- use_module('samples/concurrent.pl').

test1(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    future_all([F1,F2], F),
    await(F, [A,B]),
    C is 2 * A + B.

test(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    await(F1, A),
    await(F2, B),
    C is 2 * A + B.

test3(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    future_any([F1,F2], F),
    await(F, V),
    C = V.


slow_predicate(X) :- delay(900), X = 41.
other_slow_thing(X) :- delay(500), X = 1.

