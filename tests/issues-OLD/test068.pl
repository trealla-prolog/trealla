:- initialization(main).

main :-
    prepare(List),
    writeq(List), nl,
    sort(List, ListSorted),
    write(ListSorted), nl.

prepare([B,A]) :-
    A =.. [pair,2,X],
    B =.. [trio,3,Y,Z].
