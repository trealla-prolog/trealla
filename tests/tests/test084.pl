:-initialization(main).

:- op(700, xfx, rplus).

:- dynamic(dummy/4).

goal_expansion(L rplus R, R + L).

dummy(A,B,C,D) :- B rplus A, D rplus C.

dummy(A,B) --> { B rplus A }.

dummy((Head --> Goals), Clause, _, _) :-
        list_goal(Goals, Body),
        expand_term((Head --> Body), Clause).

dummy(g(Goal), []) --> [{Goal}, !].

main :- listing(dummy/4).
