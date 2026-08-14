:-initialization(main).

main :-
	a(b,c) =.. [_],
	halt.
main :-
	writeq(false), nl,
	div([-10,0],fois([-4,0],[1,0])) =.. [F,X,Y],
	writeq(F), nl,
	writeq(X), nl,
	writeq(Y), nl,
	writeq(ok), nl.
