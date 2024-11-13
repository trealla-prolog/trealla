:- initialization(main).

main :-
	sub_atom('не смог бы', P, _, Q, ' '),
	write([P,Q]), nl.
