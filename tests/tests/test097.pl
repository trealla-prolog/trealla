run :-
	dif([],A),L=A*L,L=L*A,A=a,
	!,
	write(nok), nl.
run :-
	write(ok), nl.

:- initialization(run).

