:-initialization(main).

populate :-
	assertz(x24(0)),
	assertz(x24(1)),
	assertz(x24(2)),
	assertz(x24(3)).


main :- populate, retract(x24(X)), write(X), nl, fail.
main.
