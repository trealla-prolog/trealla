:-initialization(main).

main :-
	[a,b] \== [a,c],
	f(X) \== f(Y),
	write(ok), nl,
	!.
main :-
	write(nok), nl.

