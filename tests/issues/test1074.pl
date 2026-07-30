:- initialization(main).

% A toplevel answer reports an answer substitution, so every equation
% in an answer description binds a variable. '1 = X' does not, and is
% consumed as a malformed description rather than loaded as a clause.
% The load carries on and run_quads reports it as failed (issue #1078).

?- X = 1.
   1 = X.

main :-
	use_module(library(quads)),
	run_quads.
