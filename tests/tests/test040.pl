:-initialization(main).

:-op(500, xfy, '').

main :-
	write_canonical(1 '' 2), nl, write(1 '' 2), nl,
	write_canonical((-)-(-)), nl, write((-)-(-)), nl,
	write_canonical((1+2)*3), nl, write((1+2)*3), nl,
	write_canonical(1*(2+3)), nl, write(1*(2+3)), nl,
	writeq([.,.(.,.,.)]), nl.
