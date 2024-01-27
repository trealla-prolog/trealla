:- initialization(main).
:- use_module(library(dif)).

main :- A=A*B,B=C*C,B=C,dif(A,B), write(nok1), nl.
main :- dif(A,B),A=A*B,B=C*C,B=C, write(nok2), nl.
main :- write(done), nl.
