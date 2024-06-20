:- use_module(library(freeze)).

foo(1).
foo(20).
foo(1337).
foo(5).

highest(X) :-
    foo(X),
    freeze(Higher, Higher > X),
    \+ foo(Higher).

main :- highest(X), !, write(X), nl.
