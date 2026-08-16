:- use_module(library(dif)).
:- initialization(main).

xmaplist_dif(_, _).
xmaplist_dif(X, [Y|Ys]) :-
	dif(X, Y),
	xmaplist_dif(X, Ys).

main :-
	findall(ok,
		(dif(V, X), dif(V, Xs), length(Xs, 7), xmaplist_dif(X, Xs)),
		Solutions),
	length(Solutions, 8),
	write(ok), nl,
	halt.
