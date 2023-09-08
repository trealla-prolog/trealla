n_factorial(0, 1).
n_factorial(N, F) :-
	N #> 0,
	N1 #= N - 1,
	n_factorial(N1, F1),
	F #= N * F1.
