:- initialization(main).

main :-
	atom_concat(G,_,abcdefgh), atom_concat(A,A,G),
	writeq([A,G]), nl,
	fail; true.
