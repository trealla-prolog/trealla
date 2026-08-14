:-initialization(main).

main :-
	L1=[a|(b|c)],
	L2=[a|(b,c)],
	L3=[a|(b;c)],
	write([L1,L2,L3]), nl,
	true.

