:-initialization(main).

populate :-
	assertz(x24(0)),
	assertz(x24(1)),
	assertz(x24(2)),
	assertz(x24(3)).

main :- populate, clause(x24(X),B), write(' > '), write(X), write(' <==> '), write(B), nl, fail.
main :- nl.
