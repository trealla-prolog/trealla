:- initialization(main).

% Issue #1084: outputs/1 per disjunctive answer. call_nth(N) re-runs
% earlier branches, so only the suffix beyond the previous answer's
% captured output is matched.

17 ?- put_char(a) ; put_char(b).
   outputs("a")
;  outputs("b").

% Prior answers without outputs/1 still contribute to the prefix.
?- put_char(x) ; put_char(y).
   true
;  outputs("y").

main :-
	use_module(library(quads)),
	run_quads.
