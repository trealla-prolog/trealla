% Issue #1129: attributed-variable wakeup goals were run through once/1,
% making freeze/2 deterministic. Separately frozen goals were also merged
% into a bare conjunction, allowing a cut in one to prune another.

:- use_module(library(freeze)).
:- initialization(main).

main :-
	findall(Y, (freeze(X, (Y=1;Y=2)), X=c), Ys),
	(   Ys == [1,2]
	->  write(freeze_nondet_ok)
	;   write('FAIL: nondeterministic frozen goal lost answers')
	), nl,

	findall([X2,Y2],
		((freeze(X2, (Y2=1;Y2=2)), freeze(X2, !), X2=c); X2=end),
		Solutions),
	(   Solutions = [[c,1],[c,2],[end,_]]
	->  write(freeze_cut_ok)
	;   write('FAIL: separately frozen cut pruned answers')
	), nl,
	halt.
