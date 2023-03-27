:-initialization(main).

main :-
	append([a],[b],L1) =.. L1,
	([_] = [_|L2]) =.. L2.
