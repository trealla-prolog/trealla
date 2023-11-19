run :-
	dif(A,B), A=C*B, C = c, B = b,
	!,
	write(ok), nl.
run :-
	write(nok), nl.

:- initialization(run).

