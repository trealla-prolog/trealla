run :-
	dif([],A), A=_*A,
	write(A), nl.

:- initialization(run).

