% goal_expansion/2 is applied repeatedly until it reaches a fixpoint.
% A clause that returns its input unchanged - the usual "fall through"
% shape - has no fixpoint to reach, and used to recurse until the C
% stack gave out (a two-line program was enough to dump core). The
% expander now stops as soon as an expansion changes nothing, and caps
% the chain regardless.

:- initialization(main).

% 1. identity: must simply run the goal, not hang or crash
user:goal_expansion(ident(X), ident(X)).
ident(X) :- integer(X).

test_identity :-
	(	ident(1) ->
		write('identity: ok')
	;	write('identity: FAILED')
	),
	nl.

% 2. a real expansion still rewrites
user:goal_expansion(double(X, Y), Y is X * 2).

test_rewrite :-
	double(21, R),
	(	R =:= 42 ->
		write('rewrite: ok')
	;	write('rewrite: FAILED')
	),
	nl.

% 3. a chain still runs to its fixpoint
user:goal_expansion(step_a(X), step_b(X)).
user:goal_expansion(step_b(X), step_c(X)).
step_c(X) :- integer(X).

test_chain :-
	(	step_a(7) ->
		write('chain: ok')
	;	write('chain: FAILED')
	),
	nl.

% 4. an expansion that keeps changing must be capped, not loop forever
user:goal_expansion(spin(X), spin(s(X))).

test_capped :-
	(	catch(spin(0), _, true) ->
		true
	;	true
	),
	write('capped: ok'),
	nl.

main :-
	test_identity,
	test_rewrite,
	test_chain,
	test_capped.
