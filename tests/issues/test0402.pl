:- initialization(main).
:- use_module(library(dif)).

main :- dif(A,B),A=B*[],B=B*[]*[], write(nok1), nl.
main :- write(done), nl.
