:- initialization(main).

main :-
	number_chars(N,"0'\\0\\"), write(N), nl,
	X = 0'\12\, write(X), nl.

