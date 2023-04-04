:-initialization(main).

main :-
	[1,1,2,1,1,2|L1]=L1, [1,1,2|R1]=R1, L1=R1,
	[1,1,2,1,1,2|L2]=L2, [1,1,2|R2]=R2, L2==R2,
	true.
