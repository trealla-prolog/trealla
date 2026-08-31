% Tabling restraints (DESIGN-tabling-phase2.md item 1).
%
% A tabled predicate with an infinite answer set stores answers until
% OOM-killed - no message, no partial output, exit code 137 like any
% other OOM. Restraints turn that into a diagnostic resource_error.
%
% tests/run.sh fails a test on a non-zero exit status even when stdout
% happens to match - the doc calls this out explicitly: an output-only
% check passes on a killed process, so the exit code is the thing that
% actually proves the fix.

:- use_module(library(tabling)).
:- use_module(library(lists)).

:- initialization(main).

% ---------------------------------------------------------------------
% Defaults are infinite; nothing changes for existing programs.

test_defaults :-
	(	current_prolog_flag(max_table_answer_size, infinite),
		current_prolog_flag(max_table_subgoal_size, infinite),
		current_prolog_flag(max_answers_for_subgoal, infinite) ->
		write('defaults: ok')
	;	write('defaults: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% An infinite answer SET raises instead of running away. This is the
% case that used to be OOM-killed (exit 137); a caught resource_error
% here, followed by a clean halt, is the whole point of the fix.

:- table as//0.

as --> [].
as --> [a], as.

test_answer_count :-
	set_prolog_flag(max_answers_for_subgoal, 100),
	(	catch(
		  ( phrase(as, Ls), var(Ls) -> true ; true ),
		  error(resource_error(max_answers_for_subgoal), _),
		  true
		) ->
		write('answer count restraint: ok')
	;	write('answer count restraint: FAILED')
	),
	nl,
	set_prolog_flag(max_answers_for_subgoal, infinite).

% A BOUNDED table must be unaffected by the same restraint - the limit
% only stops runaway growth, it does not shrink legitimate answers.

test_bounded_unaffected :-
	set_prolog_flag(max_answers_for_subgoal, 100),
	abolish_all_tables,
	(	phrase(as, [a,a,a]) ->
		write('bounded table unaffected: ok')
	;	write('bounded table unaffected: FAILED')
	),
	nl,
	set_prolog_flag(max_answers_for_subgoal, infinite).

% ---------------------------------------------------------------------
% A single answer too big to store.

:- table big_answer/1.

big_answer(X) :- length(X, 5000).

test_answer_size :-
	set_prolog_flag(max_table_answer_size, 100),
	(	catch(
		  big_answer(_),
		  error(resource_error(max_table_answer_size), _),
		  true
		) ->
		write('answer size restraint: ok')
	;	write('answer size restraint: FAILED')
	),
	nl,
	set_prolog_flag(max_table_answer_size, infinite).

% ---------------------------------------------------------------------
% A single call term too big to table.

:- table echo/1.

echo(_).

test_subgoal_size :-
	length(Big, 5000),
	set_prolog_flag(max_table_subgoal_size, 100),
	(	catch(
		  echo(Big),
		  error(resource_error(max_table_subgoal_size), _),
		  true
		) ->
		write('subgoal size restraint: ok')
	;	write('subgoal size restraint: FAILED')
	),
	nl,
	set_prolog_flag(max_table_subgoal_size, infinite).

% ---------------------------------------------------------------------

main :-
	test_defaults,
	test_answer_count,
	test_bounded_unaffected,
	test_answer_size,
	test_subgoal_size.
