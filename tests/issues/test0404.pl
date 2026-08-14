:- initialization(main).
:- use_module(library(dif)).

main :- dif([],A),A=A*_*A, write(ok), nl.
