:-initialization(main).

main :-
	freeze(X,integer(X)),freeze(Z,((freeze(_,I))=Z)),frozen(X,Z), X=1,
	writeln(X), writeln(Z), writeln(I).
