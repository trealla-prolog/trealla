:-initialization(main).

main :-
	sort([],L1),
	sort("",L2),
	write(L1), nl,
	write(L2), nl,
	L1 = L2,
	true.

