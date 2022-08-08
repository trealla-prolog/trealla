:-initialization(main).

as --> [].
as --> [a], as.

main :-
	phrase(as, Ls),
	Ls = [a|_],
	writeq(Ls), nl.
