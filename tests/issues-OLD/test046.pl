:-initialization(main).

main :-
	1 =.. L1, write(L1), nl,
	aa =.. L2, write(L2), nl,
	[aa] =.. L3, write(L3), nl,
	[aa,bb] =.. L4, write(L4), nl,
	[aa,bb,cc] =.. L5, write(L5), nl.
