:- initialization(main).
:- use_module(library(dif)).

main :- A=A*C,B=A*A,dif(A,B),C=1, write(ok), nl, !.
main :- write(nok), nl.
