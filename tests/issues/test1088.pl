:- initialization(main).

% Issue #1088: '...' in answer substitutions matches any subterm.

19 ?- X = 1.
   X = ... .

20 ?- length(L, 999).
   L = [_A,_B,_C|...].

main :-
	use_module(library(quads)),
	run_quads.
