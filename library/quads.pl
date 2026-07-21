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

  A quad may carry more than one answer description; all of them
  must hold.

  An answer description annotated with 'unexpected' (or its synonym
  'inattendue') describes an answer that must *not* occur:

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
	alternatives(AD, Alts),
	(	member(Alt, Alts),
		\+ \+ check_alternative(M, Q, VNs, Alt)
	->	true
	;	report_failure(M, Q, VNs, AD, File, Line),
		fail
	).

% An answer description may carry the annotation 'unexpected',
% meaning the answer it describes must *not* occur. It attaches to a
% single leaf answer, not to a disjunction, so
%
%     ?- foo(X).
%        X = 1
%     ;  X = 2, unexpected.
%
% asserts that the first answer is X = 1 and that, if there is a second
% answer, it is not X = 2. A quad documenting a known bug therefore
% fails while the bug is present and passes once it is fixed, which is
% what lets a quad be filed verbatim as a bug report.

% Annotations may appear anywhere in the conjunction, most naturally
% as a trailing conjunct, so they are removed wherever they occur.

drop_annotation([], _, [], false).
drop_annotation([I|T], Ann, Kept, F) :-
	drop_annotation(T, Ann, Kept0, F0),
	(	annotation(I, Ann)
	->	Kept = Kept0, F = true
	;	Kept = [I|Kept0], F = F0
	).

annotation(I, unexpected) :- ( I == unexpected ; I == inattendue ), !.
annotation(I, Ann) :- I == Ann.

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

check_alternative(M, Q, VNs, Alt) :-
	solutions(Alt, Sols),
	check_solutions(Sols, M, Q, VNs, 1).

% Walk the expected solutions, requesting the Nth answer of the
% query for the Nth description. After the last description the
% query must yield no further answer, unless '...' said otherwise.

check_solutions([], M, Q, VNs, N) :-
	attempt(M, Q, VNs, N, none).
check_solutions([Sol0|T], M, Q, VNs, N) :-
	conj(Sol0, Items0),
	drop_annotation(Items0, unexpected, Items, Unexpected),
	drop_annotation(Items, sto, Items1, Sto),
	rebuild_conj(Items1, Sol),
	( Items1 = ['...'|_] ->
		true							% any further answers accepted
	; Sto == true ->
		true							% occurs-check dependent: skipped
	; Items1 = [loops] ->
		expect(Unexpected, M, Q, VNs, N, loops)
	; Items1 = [false] ->
		T == [],
		expect(Unexpected, M, Q, VNs, N, none)
	; expected_ball(Sol, Ball) ->
		T == [],
		expect(Unexpected, M, Q, VNs, N, ball(Ball))
	;	expect(Unexpected, M, Q, VNs, N, solution(Items1)),
		N1 is N + 1,
		check_solutions(T, M, Q, VNs, N1)
	).

% An 'unexpected' answer must not be the one the query produces there.

expect(true, M, Q, VNs, N, Expect) :- !,
	\+ attempt(M, Q, VNs, N, Expect).
expect(_, M, Q, VNs, N, Expect) :-
	attempt(M, Q, VNs, N, Expect).

% Request the Nth answer of Q and check the outcome. Every call is
% bindings-transparent (\+ \+) and time-limited, so nonterminating
% queries are caught and nothing leaks between attempts.

attempt(M, Q, VNs, N, Expect) :-
	catch(
		( call_with_time_limit(1.0, \+ \+ attempt_match(M, Q, VNs, N, Expect)) ->
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

% An answer description must describe the answer *completely*: the
% bindings of the query's named variables have to be a variant of the
% ones the description gives (issue #1067). Checking only the variables
% a description happens to mention would accept
%
%     ?- X = f(Y,Z), Y = Z.
%        X = f(Y,Y).
%
% which says nothing about Z. The query is solved in one copy and the
% description applied to a second, so the two witnesses stay
% independent and can be compared. Note that a variable in a binding
% therefore denotes a variable in the answer, whereas one inside an
% error term stays a wildcard, since errors are matched by
% subsumes_term/2 in match_outcome/2 below.

attempt_match(M, Q, VNs, N, solution(Items)) :- !,
	witness(Q, VNs, W),
	copy_term(Q-W, Q1-W1),
	call_nth(M:Q1, N),
	copy_term(qd(Q,W,VNs,Items), qd(_,W2,VNs2,Items2)),
	link_names(VNs2),
	apply_equations(Items2),
	variant(W1, W2).
attempt_match(M, Q, _, N, _) :-
	call_nth(M:Q, N).

% The named variables of the query: exactly the bindings a toplevel
% would report. Anonymous variables are not recorded in VarNames.

witness(Q, VNs, W) :-
	term_variables(Q, QVs),
	query_vars(VNs, QVs, W).

query_vars([], _, []).
query_vars([_=V|T], QVs, W) :-
	(	var_member(V, QVs)
	->	W = [V|W0]
	;	W = W0
	),
	query_vars(T, QVs, W0).

var_member(V, [X|Xs]) :-
	( V == X -> true ; var_member(V, Xs) ).

apply_equations([]).
apply_equations([Item|T]) :-
	( Item = (V = Val) -> V = Val ; true ),
	apply_equations(T).

match_outcome(solution(_), matched).
match_outcome(none, none).
match_outcome(loops, loops).
match_outcome(ball(B), ball(B0)) :- ball_matches(B, B0).

% The described ball must be a variant of the one actually thrown,
% except that '...' stands for an unspecified subterm. A variable in a
% description denotes an actual variable, here as anywhere else, so
%
%     error(instantiation_error, _)
%
% requires the implementation-defined second argument to be unbound.
% To leave it unspecified write error(instantiation_error, ...), or the
% concise shorthand instantiation_error (#1068).

ball_matches(P, _) :- P == '...', !.
ball_matches(P, A) :- var(P), !, var(A).
ball_matches(P, A) :- \+ compound(P), !, P == A.
ball_matches(P, A) :-
	compound(A),
	P =.. [F|Ps],
	A =.. [F|As],
	ball_args(Ps, As).

ball_args([], []).
ball_args([P|Ps], [A|As]) :-
	ball_matches(P, A),
	ball_args(Ps, As).

% Errors may be written in full or in the customary shorthand

expected_ball(error(E, Impl), error(E, Impl)).
expected_ball(throw(B), B).
expected_ball(instantiation_error, error(instantiation_error, '...')).
expected_ball(type_error(T, C), error(type_error(T, C), '...')).
expected_ball(domain_error(D, C), error(domain_error(D, C), '...')).
expected_ball(existence_error(T, C), error(existence_error(T, C), '...')).
expected_ball(permission_error(O, T, C), error(permission_error(O, T, C), '...')).
expected_ball(evaluation_error(E), error(evaluation_error(E), '...')).
expected_ball(system_error, error(system_error, '...')).
expected_ball(syntax_error(E), error(syntax_error(E), '...')).
expected_ball(representation_error(R), error(representation_error(R), '...')).
expected_ball(resource_error(R), error(resource_error(R), '...')).
expected_ball(uninstantiation_error(C), error(uninstantiation_error(C), '...')).

timeout_ball(B) :-
	nonvar(B),
	B = error(E, _),
	nonvar(E),
	functor(E, time_limit_exceeded, _).

report_failure(M, Q, VNs, AD, File, Line) :-
	link_names(VNs),
	write('quads: FAILED '), write(File), write(':'), write(Line), nl,
	write('   ?- '), write_term(M:Q, [variable_names(VNs), quoted(true)]), write('.'), nl,
	write('   expected: '),
	write_term(AD, [variable_names(VNs), quoted(true)]), nl.
