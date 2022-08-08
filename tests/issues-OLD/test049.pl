:-initialization(main).

main :-
	setof(t, true, Ls),
	writeq(Ls), nl,
	setof(tt, true, Ls2),
	writeq(Ls2), nl.
