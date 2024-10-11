:- module(aggregate, [
	aggregate_all/3,
	aggregate/3
	]).

/**
 * aggregate_all(A, G, S):
 * The predicate aggregates A for the solutions of G and unifies the
 * result with S. Works like findall/3, without witness grouping. See
 * documentation for the supported aggregates functions.
 */
% aggregate_all(+Aggregate, +Goal, -Term)
aggregate_all(A, G, S) :-
   sys_aggr_init(A, T, I),
   findall(T, G, L),
   sys_aggr_compute(L, A, I, S).

/**
 * aggregate(A, G, S):
 * The predicate aggregates A for the solutions of G and unifies the
 * result with S. Works like bagof/3, with witness grouping. See
 * documentation for the supported aggregates functions.
 */
% aggregate(+Aggregate, +Goal, -Term)
aggregate(A, G, S) :-
   sys_aggr_init(A, T, I),
   bagof(T, G, L),
   sys_aggr_compute(L, A, I, S).

% sys_aggr_compute(+List, +Aggregate, +Term, -Term)
sys_aggr_compute([], A, I, S)  :-
   sys_aggr_fini(A, I, S).
sys_aggr_compute([T|L], A, I, S) :-
   sys_aggr_next(A, I, T, J),
   sys_aggr_compute(L, A, J, S).

% sys_aggr_init(+Aggregate, -Term, -Term)
sys_aggr_init(A, _, _) :- var(A),
   throw(error(instantiation_error,_)).
sys_aggr_init(count, '', 0) :- !.
sys_aggr_init(sum(X), X, 0) :- !.
sys_aggr_init(mul(X), X, 1) :- !.
sys_aggr_init(min(X), X, sup) :- !.
sys_aggr_init(max(X), X, inf) :- !.
sys_aggr_init(bag(X), X, []) :- !.
sys_aggr_init(set(X), X, []) :- !.
sys_aggr_init((F,G), (X,Y), (C,D)) :- !,
   sys_aggr_init(F, X, C),
   sys_aggr_init(G, Y, D).
sys_aggr_init(A, _, _) :-
   throw(error(type_error(aggregate,A),_)).

% sys_aggr_next(+Aggregate, +Term, +Term, -Term)
sys_aggr_next(count, A, '', C) :- C is A+1.
sys_aggr_next(sum(_), A, B, C) :- C is A+B.
sys_aggr_next(mul(_), A, B, C) :- C is A*B.
sys_aggr_next(min(_), A, B, C) :- sys_min(A, B, C).
sys_aggr_next(max(_), A, B, C) :- sys_max(A, B, C).
sys_aggr_next(bag(_), A, B, [B|A]).
sys_aggr_next(set(_), A, B, [B|A]).
sys_aggr_next((F,G), (A,B), (X,Y), (C,D)) :-
   sys_aggr_next(F, A, X, C),
   sys_aggr_next(G, B, Y, D).

% sys_aggr_fini(+Aggregate, +Term, -Term)
sys_aggr_fini(count, A, A).
sys_aggr_fini(sum(_), A, A).
sys_aggr_fini(mul(_), A, A).
sys_aggr_fini(min(_), A, A).
sys_aggr_fini(max(_), A, A).
sys_aggr_fini(bag(_), A, B) :- reverse(A, B).
sys_aggr_fini(set(_), A, B) :- reverse(A, H), sort(H, B).
sys_aggr_fini((F,G), (X,Y), (C,D)) :-
   sys_aggr_fini(F, X, C),
   sys_aggr_fini(G, Y, D).

% sys_min(+ExtNumber, +ExtNumber, -ExtNumber)
sys_min(sup, X, R) :- !, R = X.
sys_min(X, sup, R) :- !, R = X.
sys_min(inf, _, R) :- !, R = inf.
sys_min(_, inf, R) :- !, R = inf.
sys_min(X, Y, R) :- R is min(X, Y).

% sys_max(+ExtNumber, +ExtNumber, -ExtNumber)
sys_max(inf, X, R) :- !, R = X.
sys_max(X, inf, R) :- !, R = X.
sys_max(sup, _, R) :- !, R = sup.
sys_max(_, sup, R) :- !, R = sup.
sys_max(X, Y, R) :- R is max(X, Y).

