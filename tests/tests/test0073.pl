:- initialization(main).

main :-
	X = {a:b},
	write(X), nl,
	Y = {}(a:b),
	write(Y), nl,
	Z = :(a,b),
	write(Z), nl,
	A = a:b,
	write(A), nl,
	B = (a:b),
	write(B), nl.


