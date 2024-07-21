:- use_module(library(iso_ext)). % for scryer
:- use_module(library(clpz)).

% if this is uncommented, it works:
% :- use_module(library(dcgs)).

:-initialization(main).

main :-
    X in 1 .. 2,
    copy_term(X, X, Attrs),
    write(Attrs), nl.

