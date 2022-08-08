:-initialization(main).

main :-
	atom_length('一二三',L1),
	length("一二三",L2),
	L1=L2, L1=3,
	write(L1), nl.
