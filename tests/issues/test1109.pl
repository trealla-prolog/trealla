:- initialization(main).

burn(0) :- !.
burn(N) :-
	N1 is N - 1,
	burn(N1).

main :-
	statistics(cputime, T0),
	burn(1000000),
	statistics(cputime, T1),
	(   T1 > T0
	->  writeln(cpu_time_advances)
	;   writeln(cpu_time_stalled)
	).
