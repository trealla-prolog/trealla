:- initialization(main).

main :-
	L1=("a"||.),
	writeq(L1), nl,
	L2=("a"||...),
	writeq(L2), nl,
	L3= ""||'.' ,
	writeq(L3), nl.

