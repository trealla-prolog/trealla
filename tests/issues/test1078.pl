:- initialization(main).

% A malformed answer description does not stop the load: it is recorded
% like any other quad and reported as failed when the quads are run, so
% several such cases can share one file, and quads following them are
% still loaded (issue #1078). The same applies to a non-ground quad
% identifier.

?- true.
   true.

?- X = 1.
   1 = X.

?- Y = 1.
   Y = 1, Y = 2.

?- Z = f(W), W = 1.
   Z = f(W), W = 1.

X ?- true.
   true.

?- member(V, [1,2]).
   V = 1
;  V = 2.

main :-
	use_module(library(quads)),
	run_quads.
