:- initialization(main).

% Issue #1096: other_answer_sequence — leaf answers may match in any order.
% ISO setof/3 (8.10.3) leaves answer order undefined.

% Ulrich's example: Trealla finds Y=1 before Y=2; the description lists
% the other order. With the annotation both must pass.

setof_7 ?- setof(1, (Y=2 ; Y=1), L).
   Y = 2, L = [1]
;  Y = 1, L = [1]
|  other_answer_sequence.

% Same answers, description already in Trealla's order.

setof_7_native ?- setof(1, (Y=2 ; Y=1), L).
   Y = 1, L = [1]
;  Y = 2, L = [1]
|  other_answer_sequence.

% Without the annotation, the foreign order must fail.

setof_7_ordered ?- setof(1, (Y=2 ; Y=1), L).
   Y = 2, L = [1]
;  Y = 1, L = [1].

% Well-formedness: a single leaf cannot be exchanged.

oas_one_leaf ?- true.
   true
|  other_answer_sequence.

% Well-formedness: the annotation alone is not an outcome.

oas_alone ?- true.
   other_answer_sequence.

% Not a (|)-alternative of its own.

oas_conjunct ?- true.
   true, other_answer_sequence.

main :-
	use_module(library(quads)),
	run_quads,
	halt.
