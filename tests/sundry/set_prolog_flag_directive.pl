% Regression: a bare `:- set_prolog_flag(Name, Value).` DIRECTIVE (as
% opposed to a goal inside another directive's body) was silently a
% no-op for any flag other than the five that affect parsing itself
% (double_quotes, character_escapes, occurs_check, strict_iso,
% empty_args). directives() in src/parser.c intercepts set_prolog_flag
% directives at parse time for those five and used to `return true`
% unconditionally afterwards, discarding the directive instead of
% falling through to run it as an ordinary goal - so `tabling`,
% `global_bb`, and the tabling restraint flags never reached the
% runtime set_prolog_flag/2 that actually implements them when set
% this way. Calling set_prolog_flag/2 from inside another goal (eg.
% initialization/1's argument, or a predicate body) always worked; only
% the bare directive form was affected.

:- use_module(library(tabling)).

:- initialization(main).

% A flag with no parse-time meaning, boolean-valued.

:- set_prolog_flag(tabling, false).

test_tabling_directive :-
	(	current_prolog_flag(tabling, false) ->
		write('tabling directive: ok')
	;	write('tabling directive: FAILED')
	),
	nl,
	set_prolog_flag(tabling, true).

% A flag with no parse-time meaning, INTEGER-valued - this is the case
% that broke even after a first fix that only handled unrecognized
% flag NAMES: an earlier `if (!is_interned(p2)) return true` guard
% discarded any non-atom value before the flag name was ever checked.

:- set_prolog_flag(max_answers_for_subgoal, 5).

test_integer_flag_directive :-
	(	current_prolog_flag(max_answers_for_subgoal, 5) ->
		write('integer flag directive: ok')
	;	write('integer flag directive: FAILED')
	),
	nl,
	set_prolog_flag(max_answers_for_subgoal, infinite).

% The five parse-time flags must still take effect immediately (this
% is what the special-casing in directives() exists for in the first
% place) - a regression here would only show up as a syntax error on
% whatever follows, so check it does not by parsing a double-quoted
% string as codes right after switching the flag, within the same
% load.

:- set_prolog_flag(double_quotes, codes).

test_parse_time_flag :-
	X = "ab",
	(	X == [0'a, 0'b] ->
		write('parse-time flag: ok')
	;	write('parse-time flag: FAILED')
	),
	nl.

:- set_prolog_flag(double_quotes, atom).

main :-
	test_tabling_directive,
	test_integer_flag_directive,
	test_parse_time_flag.
