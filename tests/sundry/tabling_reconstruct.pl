% Trie-path answer reconstruction (DESIGN-tabling-phase2.md item 5).
%
% An answer used to be stored TWICE: as its path in the answer trie,
% and again as a full `cell *image` copy. The image is now dropped and
% the answer rebuilt from the path, which costs a parent pointer per
% trie node (8 bytes) instead of a whole term copy (24 bytes per cell,
% plus a malloc each) - and trie nodes are shared between answers with
% common prefixes while images never were.
%
% Reconstruction has to reproduce the term EXACTLY, so these check the
% shapes where a canonicalising round-trip could plausibly lose
% something: nesting, the several numeric types, both string
% representations, and - the sharp one - variable identity.
%
% The one carve-out: a SUBSUMPTIVE table (item 2) omits the aggregated
% argument from its trie by construction, since that is how answers
% collide and combine. Its path cannot reproduce that value, so those
% tables keep their image; test 5 is the guard on that.

:- use_module(library(tabling)).
:- use_module(library(lists)).

:- initialization(main).

% ---------------------------------------------------------------------
% 1. Assorted ground shapes must survive the round trip unchanged.

:- table shapes/2.

shapes(1, f(a, g(b, h(c)), [1,2,3])).
shapes(2, [[], [x], [y,z]]).
shapes(3, point(-1, 0, 42)).
shapes(4, "a string").
shapes(5, [0'a, 0'b, 0'c]).
shapes(6, mixed(1.5, -2.25, 1000000000000000000000)).
shapes(7, deep(deep(deep(deep(bottom))))).

test_ground_shapes :-
	findall(N-T, shapes(N,T), Got),
	msort(Got, Sorted),
	(	Sorted == [1-f(a, g(b, h(c)), [1,2,3]),
			   2-[[], [x], [y,z]],
			   3-point(-1, 0, 42),
			   4-"a string",
			   5-[0'a, 0'b, 0'c],
			   6-mixed(1.5, -2.25, 1000000000000000000000),
			   7-deep(deep(deep(deep(bottom))))] ->
		write('ground shapes: ok')
	;	write('ground shapes: FAILED'), nl, write(Sorted)
	),
	nl.

% ---------------------------------------------------------------------
% 2. Variable IDENTITY, not just variable-ness. s([a,V],[V]) shares V
% between its arguments; the trie numbers variables canonically by
% first appearance, so reconstruction must map each number back to ONE
% fresh variable or the two occurrences silently stop being the same.
% Binding one must bind the other.

:- table share/2.

share([a|X], X).

test_variable_sharing :-
	share([_P,Q], R),
	R = [c],
	(	Q == c ->
		write('variable sharing: ok')
	;	write('variable sharing: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 3. Distinct variables must stay distinct - the mirror of test 2. A
% reconstruction that collapsed every variable to one would still pass
% test 2, so this pins the other direction.

:- table two_vars/1.

two_vars(pair(_A,_B)).

test_distinct_vars :-
	two_vars(pair(X,Y)),
	X = 1,
	(	var(Y) ->
		write('distinct vars: ok')
	;	write('distinct vars: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 4. Repeated variables within one answer keep their identity too:
% f(V,V) must reconstruct as one variable used twice, not two.

:- table repeated/1.

repeated(f(V,V)).

test_repeated_var :-
	repeated(f(A,B)),
	A = bound,
	(	B == bound ->
		write('repeated var: ok')
	;	write('repeated var: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 5. A subsumptive table keeps its image, because its trie omits the
% aggregated argument. If that carve-out were dropped, the aggregated
% value would come back as an unbound variable rather than a number.

:- table agg(_,min).

acost(a,7).
acost(a,3).
acost(b,5).

agg(X,C) :- acost(X,C).

test_subsumptive_still_exact :-
	findall(X-C, agg(X,C), Got),
	msort(Got, Sorted),
	(	Sorted == [a-3, b-5] ->
		write('subsumptive still exact: ok')
	;	write('subsumptive still exact: FAILED'), nl, write(Sorted)
	),
	nl.

% ---------------------------------------------------------------------
% 6. Reconstruction happens on every read, so a table read repeatedly
% must give the same answers every time - a reconstruction that
% consumed or mutated the path would pass once and fail after.

test_stable_across_reads :-
	findall(T, shapes(_,T), A),
	findall(T, shapes(_,T), B),
	findall(T, shapes(_,T), C),
	(	A == B, B == C ->
		write('stable across reads: ok')
	;	write('stable across reads: FAILED')
	),
	nl.

% ---------------------------------------------------------------------

main :-
	test_ground_shapes,
	test_variable_sharing,
	test_distinct_vars,
	test_repeated_var,
	test_subsumptive_still_exact,
	test_stable_across_reads.
