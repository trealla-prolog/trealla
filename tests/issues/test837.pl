:- initialization(main).
:- op(699,xf,>.).
:- op(9,yf,.>).

main :-
	writeq(a>. >b), nl,
	writeq((a>.) >.), nl,
	writeq((a.>) .>), nl.
