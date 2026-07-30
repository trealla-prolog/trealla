:- initialization(main).

% An answer substitution is idempotent, so no variable it binds occurs
% in what it binds another to. 'X = f(Y), Y = 1' is not an answer, the
% answer being 'X = f(1), Y = 1', and is reported as malformed rather
% than run as a quad that passes.

?- X = f(Y), Y = 1.
   X = f(Y), Y = 1.

main :-
	use_module(library(quads)),
	run_quads.
