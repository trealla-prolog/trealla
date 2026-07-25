:- initialization(main).
:- use_module(library(tabling)).

% Direct connections in our graph
edge(a, b).
edge(b, c).
edge(c, a). % This introduces a cycle (a -> b -> c -> a)
edge(c, d).

% Enable tabling for the path/2 predicate
:- table path/2.

% Left-recursive path definition
path(X, Y) :- path(X, Z), edge(Z, Y).
path(X, Y) :- edge(X, Y).

main :- path(a, d).
