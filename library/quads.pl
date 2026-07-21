/** Quads: queries using answer descriptions.

  Quads are embedded tests: a '?- Query.' term in a source file
  followed by a term describing the expected toplevel answers.
  See https://github.com/trealla-prolog/trealla/issues/1063

  When a file is consulted the compiler records each quad as a fact

      '$quad'(Query, VarNames, AnswerDescription, File, Line)

  in the module being consulted. Nothing is executed at load time.
  This library interprets the recorded quads as tests:

      ?- use_module(library(quads)).
      ?- run_quads.             % run quads recorded in module user
      ?- run_quads(mymod).      % run quads recorded in some module

  run_quads/0 and run_quads/1 always succeed and print a report.
  run_quads_halt/0 halts the system with a non-zero exit code if
  any quad failed, for use in scripts:

      tpl file.pl -g 'use_module(library(quads)), run_quads_halt'

  Answer description syntax (see the issue for the full grammar):

      ?- member(X, [1,2,3]).
         X = 1
      ;  X = 2
      ;  X = 3.

      ?- fail.
         false.

      ?- atom_length(A, L).
         error(instantiation_error, _).

      ?- repeat.
         true
      ;  true
      ;  ... .

  Alternative acceptable outcomes are separated by (|)/2.

  An answer description annotated with 'unexpected' describes an
  answer that must *not* occur:

      ?- X = 1.
         X = 2, unexpected.

  The annotation sto is recognised but such (parts of) descriptions
  are currently skipped, not interpreted.
*/

:- module(quads, [run_quads/0, run_quads/1, run_quads_halt/0]).

:- use_module(library(lists)).
:- use_module(library(iso_ext)).

run_quads :-
	run_quads(user).

run_quads(M) :-
	quad_list(M, Qs),
	( Qs == [] ->
		write('quads: nothing to run.'), nl
	;	run_list(Qs, M, 0, Passed, 0, Failed),
		Total is Passed + Failed,
		write('quads: '), write(Total), write(' run, '),
		write(Passed), write(' passed, '),
		write(Failed), write(' failed.'), nl,
		bb_put(quads_failed, Failed)
	).

run_quads_halt :-
	run_quads,
	( bb_get(quads_failed, N), N > 0 -> halt(1) ; halt ).

quad_list(M, Qs) :-
	catch(
		findall(q(Q, VNs, AD, F, L), M:'$quad'(Q, VNs, AD, F, L), Qs),
		error(existence_error(_, _), _),
		Qs = []
	).

run_list([], _, P, P, F, F).
run_list([q(Q, VNs, AD, File, Line)|T], M, P0, P, F0, F) :-
	( \+ \+ check_quad(M, Q, VNs, AD, File, Line) ->
		P1 is P0 + 1, F1 = F0
	;	P1 = P0, F1 is F0 + 1
	),
	run_list(T, M, P1, P, F1, F).

% A quad passes if any (|)-alternative of its answer description
% matches. Same-named variables of the query term and the answer
% description term are first unified via the VarNames list.

check_quad(M, Q, VNs, AD, File, Line) :-
	link_names(VNs),
	strip_unexpected(AD, AD1, Unexpected),
	alternatives(AD1, Alts),
	(	(	Unexpected == true
		->	\+ ( member(Alt, Alts), \+ \+ check_alternative(M, Q, Alt) )
		;	member(Alt, Alts),
			\+ \+ check_alternative(M, Q, Alt)
		)
	->	true
	;	report_failure(M, Q, VNs, AD, File, Line, Unexpected),
		fail
	).

% An answer description may carry the annotation 'unexpected', meaning
% the answer it describes must *not* occur (issue #1065). It is written
% as a trailing conjunct, as in
%
%     ?- X = 1.
%        X = 2, unexpected.
%
% so it is stripped wherever it appears and the sense of the whole
% check is then inverted. A quad documenting a known bug therefore
% fails while the bug is present and passes once it is fixed, which is
% what lets a quad be filed verbatim as a bug report.

strip_unexpected('|'(A, B), '|'(A1, B1), F) :- !,
	strip_unexpected(A, A1, FA),
	strip_unexpected(B, B1, FB),
	( FA == true -> F = true ; F = FB ).
strip_unexpected((A ; B), (A1 ; B1), F) :- !,
	strip_unexpected(A, A1, FA),
	strip_unexpected(B, B1, FB),
	( FA == true -> F = true ; F = FB ).
strip_unexpected(Sol, Sol1, F) :-
	conj(Sol, Items),
	drop_unexpected(Items, Kept, F),
	rebuild_conj(Kept, Sol1).

drop_unexpected([], [], false).
drop_unexpected([I|T], Kept, F) :-
	drop_unexpected(T, Kept0, F0),
	(	I == unexpected
	->	Kept = Kept0, F = true
	;	Kept = [I|Kept0], F = F0
	).

rebuild_conj([], true).
rebuild_conj([I], I) :- !.
rebuild_conj([I|T], (I , R)) :-
	rebuild_conj(T, R).

link_names([]).
link_names([N=V|T]) :-
	( member(N2=V2, T), N2 == N -> V = V2 ; true ),
	link_names(T).

alternatives('|'(A, B), [A|T]) :- !,
	alternatives(B, T).
alternatives(A, [A]).

solutions((A ; B), [A|T]) :- !,
	solutions(B, T).
solutions(A, [A]).

conj((A , B), [A|T]) :- !,
	conj(B, T).
conj(A, [A]).

check_alternative(M, Q, Alt) :-
	solutions(Alt, Sols),
	check_solutions(Sols, M, Q, 1).

% Walk the expected solutions, requesting the Nth answer of the
% query for the Nth description. After the last description the
% query must yield no further answer, unless '...' said otherwise.

check_solutions([], M, Q, N) :-
	attempt(M, Q, N, none).
check_solutions([Sol|T], M, Q, N) :-
	conj(Sol, Items),
	( Items = ['...'|_] ->
		true							% any further answers accepted
	; Items = [sto|_] ->
		true							% occurs-check dependent: skipped
	; Items = [loops] ->
		attempt(M, Q, N, loops)
	; Items = [false] ->
		T == [],
		attempt(M, Q, N, none)
	; expected_ball(Sol, Ball) ->
		T == [],
		attempt(M, Q, N, ball(Ball))
	;	attempt(M, Q, N, solution(Items)),
		N1 is N + 1,
		check_solutions(T, M, Q, N1)
	).

% Request the Nth answer of Q and check the outcome. Every call is
% bindings-transparent (\+ \+) and time-limited, so nonterminating
% queries are caught and nothing leaks between attempts.

attempt(M, Q, N, Expect) :-
	catch(
		( call_with_time_limit(1.0, \+ \+ (call_nth(M:Q, N), check_items(Expect))) ->
			Outcome = matched
		;	( catch(call_with_time_limit(1.0, \+ \+ call_nth(M:Q, N)), _, fail) ->
				Outcome = mismatched
			;	Outcome = none
			)
		),
		Ball0,
		( timeout_ball(Ball0) -> Outcome = loops ; Outcome = ball(Ball0) )
	),
	match_outcome(Expect, Outcome).

check_items(solution(Items)) :- !,
	check_bindings(Items).
check_items(_).

match_outcome(solution(_), matched).
match_outcome(none, none).
match_outcome(loops, loops).
match_outcome(ball(B), ball(B0)) :- subsumes_term(B, B0).

check_bindings([]).
check_bindings([Item|T]) :-
	check_binding(Item),
	check_bindings(T).

check_binding(true) :- !.
check_binding(V = T) :- !,
	subsumes_term(T, V).
check_binding(_).							% unknown annotation: accept

% Errors may be written in full or in the customary shorthand

expected_ball(error(E, Impl), error(E, Impl)).
expected_ball(throw(B), B).
expected_ball(instantiation_error, error(instantiation_error, _)).
expected_ball(type_error(T, C), error(type_error(T, C), _)).
expected_ball(domain_error(D, C), error(domain_error(D, C), _)).
expected_ball(existence_error(T, C), error(existence_error(T, C), _)).
expected_ball(permission_error(O, T, C), error(permission_error(O, T, C), _)).
expected_ball(evaluation_error(E), error(evaluation_error(E), _)).
expected_ball(system_error, error(system_error, _)).
expected_ball(syntax_error(E), error(syntax_error(E), _)).
expected_ball(representation_error(R), error(representation_error(R), _)).
expected_ball(resource_error(R), error(resource_error(R), _)).
expected_ball(uninstantiation_error(C), error(uninstantiation_error(C), _)).

timeout_ball(B) :-
	nonvar(B),
	B = error(E, _),
	nonvar(E),
	functor(E, time_limit_exceeded, _).

report_failure(M, Q, VNs, AD, File, Line, Unexpected) :-
	write('quads: FAILED '), write(File), write(':'), write(Line), nl,
	write('   ?- '), write_term(M:Q, [variable_names(VNs), quoted(true)]), write('.'), nl,
	(	Unexpected == true
	->	write('   unexpected: ')
	;	write('   expected: ')
	),
	write_term(AD, [variable_names(VNs), quoted(true)]), nl.
