:- initialization(main).

main :-
	(atom_concat(A,A,aaa); true),
	atom_concat(A,A,aaaa), A=aa, write(A), nl.

