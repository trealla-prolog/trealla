:-initialization(main).

main :-
	C = + +1, B = +B,
	C @< B,
	\+ (B @< C),
	true.
