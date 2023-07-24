:-initialization(main).

main :-
	findall(X, (between(1,3,X) *-> true ; true), L),
	write(L), nl.
