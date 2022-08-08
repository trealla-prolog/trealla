:-initialization(main).

as --> [].
as --> [a], as.

main :-
	length(_,E), writeq(E), nl,
	N is 2^E, length(Ls, N),
	phrase(as, Ls),
	E == 14,
	!.
