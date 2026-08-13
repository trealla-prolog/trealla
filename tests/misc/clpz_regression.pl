:- use_module(library(clpz)).

check(Name, Goal) :-
        ( call(Goal) -> format('~w: ok~n', [Name])
        ; format('~w: FAILED~n', [Name])
        ).

:- initialization(main).

main :-
        check(interval, interval),
        check(linear, linear),
        check(disequality, disequality),
        check(reification, reification),
        check(reflection, reflection),
        check(labeling, labeling),
        check(backtracking, backtracking).

interval :- X in 1..9, X #=< 4, fd_dom(X, 1..4).
linear :- X #= Y + 2, X in 3..5, Y #= 1.
disequality :- X in 1..3, X #\= 2, fd_dom(X, 1\/3).
reification :- X in 1..2, '#<==>'(X #= 1, B), B #= 1, X = 1.
reflection :- X in -2..7, fd_inf(X, -2), fd_sup(X, 7), fd_size(X, 10).
labeling :- findall(X, (X in 1..3, X #\= 2, label([X])), [1,3]).
backtracking :- X in 1..2, (X = 1 ; X = 2).
