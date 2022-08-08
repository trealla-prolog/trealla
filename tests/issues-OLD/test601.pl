main :-
	L=['3'|L],
	Es=['0'|L],
	writeq(L), nl,
	writeq(Es), nl.

:- initialization(main).
