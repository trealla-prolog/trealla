:-initialization(main).

main :-
	sort([],L1),
	write(L1), nl,
	sort("",L2),
	write(L2), nl,
	L1 = L2,
	sort([c,b,a,' ',a,b,c],L3),
	write(L3), nl,
	sort("cba abc",L4),
	write(L4), nl,
	true.

