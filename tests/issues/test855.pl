% Issue #855: unification was exponential in a shared DAG (blam/1).
% Pair-memoization keeps L = K near-linear in N.
% Before the fix, N=28 was ~3s; after it is microseconds.

:- initialization(main).

blam([]).
blam([L|L]) :- blam(L).

main :-
	length(L, 28),
	length(K, 28),
	blam(L),
	blam(K),
	statistics(cputime, T0),
	(	L = K
	->	statistics(cputime, T1),
		D is T1 - T0,
		(	D < 0.5
		->	write(ok), nl
		;	write(too_slow), write(' '), write(D), nl
		)
	;	write(fail_unify), nl
	).
