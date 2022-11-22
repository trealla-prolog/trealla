:-initialization(main).

main :-
	X0 is 1 mod 3,
	write(X0), nl,
	X1 is -1 mod 3,
	write(X1), nl,
	X2 is -1 mod -3,
	write(X2), nl,
	X3 is 1 mod -3,
	write(X3), nl,

	X4 is -2 mod 3,
	write(X4), nl,
	X5 is -2 mod -3,
	write(X5), nl,
	X6 is 2 mod -3,
	write(X6), nl,

	X7 is (1 << 150) mod (3 << 150),
	write(X7), nl,
	X8 is -(1 << 150) mod (3 << 150),
	write(X8), nl,
	X9 is -(1 << 150) mod -(3 << 150),
	write(X9), nl,
	X10 is (1 << 150) mod -(3 << 150),
	write(X10), nl,

	X11 is -5 mod -3,
	write(X11), nl,

	true.

