:-initialization(main).

explode :-
    findall(A-B, something(A, B), Xs),
    write(Xs), nl.

something(a, b).


main :-
	explode.
