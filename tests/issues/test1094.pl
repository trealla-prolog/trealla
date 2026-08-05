:- initialization(main).

% Issue #1094: resource_error(memory) from length/2 must be catchable.
% Overflow of MAX_LOCAL_VARS used to set q->oom and hard-terminate the
% query ("resource_error(memory). %query terminated"), which broke
% run_quads even when the attempt was wrapped in catch/3.

main :-
	catch(length(_, 1_000_000_000), E1, true),
	E1 = error(resource_error(memory), _),
	write(catch_length-ok), nl,
	% Nested under the same wrappers quads uses.
	catch(
		call_with_time_limit(1.0, \+ \+ length(_, 1_000_000_000)),
		E2,
		true
	),
	E2 = error(resource_error(memory), _),
	write(catch_under_time_limit-ok), nl,
	% After catch/3 clears oom, $fail_on_retry/1 (once/call/->) must not
	% see a stale bound CP index (uninstantiation_error). Trail growth
	% failure used to bind without trailing.
	once(true),
	( true -> true ; fail ),
	call(true),
	write(catch_fail_on_retry-ok), nl,
	% And as a quad answer description.
	use_module(library(quads)),
	run_quads,
	halt.

?- length(L, 1_000_000_000).
   resource_error(memory).
