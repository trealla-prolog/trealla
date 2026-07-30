:- initialization(main).

% A term whose principal functor is (',')/2 (or (;)/2, (|)/2) after a
% query is an answer description even when a conjunct is unknown. It is
% consumed as malformed rather than warned as "no description" and
% loaded as a clause (permission error on (',')/2) — issue #1087.

18 ?- Y = 2.
   Y = 2, some_unknown_stuff.

main :-
	use_module(library(quads)),
	run_quads.
