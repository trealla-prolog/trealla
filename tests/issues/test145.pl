:-initialization(main).

main :-
	Z = ([[a|Z],X]=[X,[Y|Z]]),
	Z,
	write(Z), nl.

