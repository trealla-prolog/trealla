:- initialization(main).

main :-
	findall([Before,Len,After], sub_atom(banana, Before, Len, After, ana), L),
	write(L), nl.
