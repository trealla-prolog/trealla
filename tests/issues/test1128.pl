:- initialization(main).

% Issue #1128: 'maybe' marks an answer that leaves some variable of
% the query attributed - a pending constraint of any attribute module,
% not resolved into an ordinary binding. It names no particular
% variable or module, only that one exists.

:- use_module(library(dif)).

1 ?- dif(X, Y), X = a.
   X = a, maybe.

2 ?- dif(X, Y).
   maybe.

main :-
	use_module(library(quads)),
	run_quads.
