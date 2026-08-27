% Issue #1129: attributed-variable wakeup goals were run through once/1,
% making freeze/2 deterministic. Separately frozen goals were also merged
% into a bare conjunction, allowing a cut in one to prune another.

:- use_module(library(freeze)).
:- initialization(main).

freeze_nondet ?- freeze(X, (Y=1;Y=2)), X=c.
   X = c, Y = 1
;  X = c, Y = 2.

freeze_cut ?- freeze(X, (Y=1;Y=2)), freeze(X, !), X=c ; X=end.
   X = c, Y = 1
;  X = c, Y = 2
;  X = end.

main :-
	use_module(library(quads)),
	run_quads.
