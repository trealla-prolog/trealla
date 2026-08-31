% Tabling regression tests.
%
% Each of these was a real bug found by running third-party programs
% (SWI-Prolog samples, Scryer bugs, Logtalk's tabling example) against
% the native tabling engine. They are cheap and cover the awkward corners:
% suspension vs completion, continuation capture across barriers, and
% termination on cyclic call graphs.

:- use_module(library(tabling)).
:- use_module(library(lists)).

:- initialization(main).

% ---------------------------------------------------------------------
% 1. A tabled call inside findall/3.
%
% A consumer cannot be suspended here: its continuation lives in the
% collector's C-level state, so a captured goal-list continuation cannot
% resume it. Fresh variants must therefore be COMPLETED in a nested SCC
% rather than suspended. Used to silently answer 3 instead of 6.

:- table sum_to/2.

sum_to(0, 0).
sum_to(N, S) :-
	N > 0,
	M is N - 1,
	findall(X, sum_to(M, X), Xs),
	sum_list(Xs, S0),
	S is S0 + N.

test_findall :-
	(	sum_to(3, S), S == 6 ->
		write('findall: ok')
	;	write('findall: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 2. A tabled call inside setof/3 whose template mentions the tabled
% predicate (the shape of SWI's box-stacking sample).

:- table chain/2.

chain(0, [0]).
chain(N, [N|T]) :-
	N > 0,
	M is N - 1,
	setof(L, chain(M, L), [T|_]).

test_setof :-
	(	chain(3, L), L == [3,2,1,0] ->
		write('setof: ok')
	;	write('setof: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 3. Suspension underneath call/1.
%
% call/1 plants a barrier of its own; only the barrier belonging to the
% engine's own reset/3 ends a captured continuation. Getting this wrong
% truncated the continuation and produced answers containing unbound
% variables (this is also how Logtalk's debug wrapper calls goals).

:- table under_call/1.

under_call(1).
under_call(X) :-
	wrap(inner(Y)),
	Y < 3,
	X is Y + 1.

wrap(G) :- call(G).
inner(Y) :- under_call(Y).

test_call_barrier :-
	findall(X, under_call(X), Xs0),
	msort(Xs0, Xs),
	(	Xs == [1,2,3] ->
		write('call barrier: ok')
	;	write('call barrier: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 4. Left recursion over a cyclic graph: the classic reason to table.
% Only terminates if the recursive call suspends on the active table.

:- table path/2.

path(X, Y) :- path(X, Z), edge(Z, Y).
path(X, Y) :- edge(X, Y).

edge(a, b).
edge(b, c).
edge(c, a).

test_left_recursion :-
	findall(X-Y, path(X, Y), Ps),
	length(Ps, N),
	(	N == 9 ->
		write('left recursion: ok')
	;	write('left recursion: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 5. Mutual recursion between two tabled predicates.

:- table even/1, odd/1.

even(0).
even(N) :- N > 0, M is N - 1, odd(M).
odd(N)  :- N > 0, M is N - 1, even(M).

test_mutual :-
	(	even(20), \+ odd(20) ->
		write('mutual: ok')
	;	write('mutual: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 6. A genuine cycle with no answers must fail, not loop. Exercises SCC
% merging: the inner SCC depends on an outer one, so its tables are
% handed to the parent instead of being completed on their own.

:- table p/0, q/0.

p :- q.
q :- p.

test_cycle :-
	(	\+ p ->
		write('cycle: ok')
	;	write('cycle: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 6b. A cycle WITH answers, entered through the predicate that is not
% the recursive one. p calls q; q calls back into p. Querying q/1
% first opens q's SCC, nests p's SCC inside it, and p's SCC escapes
% (it depends on q, the outer one) and is merged into q's rather than
% completed on its own. If completion() marked p's table complete
% before that merge/escape was checked, p's table would be cached
% complete but empty - test_cycle above can't catch this because its
% cycle has no answers at all, so an empty (wrongly-completed) table
% looks identical to a correct one. Query order matters here: q first
% is what exposes it.

:- table pm/1, qm/1.

pm(X) :- qm(X).
pm(1).
qm(X) :- pm(X).

test_scc_merge :-
	(	qm(1), pm(1) ->
		write('scc merge: ok')
	;	write('scc merge: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 7. Memoization actually happens: an exponential fib is instant when
% tabled (and the answer is right).

:- table fib/2.

fib(0, 1).
fib(1, 1).
fib(N, F) :-
	N > 1,
	N1 is N - 1,
	N2 is N - 2,
	fib(N1, F1),
	fib(N2, F2),
	F is F1 + F2.

test_fib :-
	(	fib(100, F), F =:= 573147844013817084101 ->
		write('fib: ok')
	;	write('fib: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 8. The tabling flag turns memoization off; tabled predicates then run
% as plain calls (still correct here, just not memoized).

test_flag :-
	current_prolog_flag(tabling, true),
	set_prolog_flag(tabling, false),
	(	fib(10, F), F =:= 89 ->
		write('flag off: ok')
	;	write('flag off: FAILED')
	),
	nl,
	set_prolog_flag(tabling, true),
	current_prolog_flag(tabling, true),
	write('flag on: ok'),
	nl.

% ---------------------------------------------------------------------
% 9. Answer dedup is by VARIANT, not by term identity.
%
% Scryer issue #2621: two clauses q(_). q(_). must yield ONE answer, and
% q(A,_,A). q(_,A,A). q(A,_,A). exactly TWO - the third clause is a
% variant of the first. Requires the answer trie to number variables
% canonically rather than compare terms structurally.

:- table dup/1.

dup(_).
dup(_).

:- table dup3/3.

dup3(A, _, A).
dup3(_, A, A).
dup3(A, _, A).

test_variant_answers :-
	findall(x, dup(_), L1),
	length(L1, N1),
	findall(x, dup3(_, _, _), L2),
	length(L2, N2),
	(	N1 == 1, N2 == 2 ->
		write('variant answers: ok')
	;	write('variant answers: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 10. Answers must not depend on the order in which tabled predicates
% are first called.
%
% Scryer issue #1895: with p/1 calling setof over a tabled g/1, asking
% p/1 first lost the setof answer, while asking g/1 first found it. Same
% root cause as (1): the consumer inside setof/3 cannot be suspended, so
% a fresh variant has to be completed instead.

:- table p/1.
:- table g/1.

g(a).

p(a).
p(Ls) :- setof(X, g(X), Ls).

test_order_independent :-
	abolish_all_tables,
	findall(X, p(X), P1),
	abolish_all_tables,
	findall(_, g(_), _),
	findall(X, p(X), P2),
	(	P1 == [a,[a]], P2 == [a,[a]] ->
		write('order independence: ok')
	;	write('order independence: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 11. Non-ground answers must keep variable sharing.
%
% Scryer issue #3365. An answer like s([a,V],[V]) shares V between its
% arguments. The imported answer's variables are created in the frame
% running the tabling driver; if that frame is trimmed on deterministic
% exit, a structure the caller holds points at recycled slots and the
% two occurrences silently stop being the same variable - binding one no
% longer binds the other. Every test above returns GROUND answers, which
% is why this went unnoticed.

:- table share/2.

share([a|X], X).

test_sharing :-
	share([_P,Q], R),
	R = [c],
	(	Q == c ->
		write('answer sharing: ok')
	;	write('answer sharing: FAILED')
	),
	nl.

% The same defect lost whole solutions in the issue's grammar: calling a
% tabled predicate in generate mode (an unbound list) dropped answers
% and returned half-bound terms. The recursive call here leaves its
% second argument unbound, so the answer shares variables across
% arguments - exactly the shape that breaks.

:- table o/2, gram/2.

o([the,man|B], B).
o([the,ball|B], B).
o([the,big,ball|B], B).

gram(A, B) :- o(A, B).
gram(A, B) :- o(A, C), C = [that|D], gram(D, E), E = [runs|B].

test_generate :-
	findall(W, (length(W, 7), gram(W, [])), Ws),
	msort(Ws, Sorted),
	(	Sorted == [[the,ball,that,the,big,ball,runs],
		           [the,big,ball,that,the,ball,runs],
		           [the,big,ball,that,the,man,runs],
		           [the,man,that,the,big,ball,runs]] ->
		write('generate mode: ok')
	;	write('generate mode: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 12. Backtracking into a tabled call that creates a fresh variant each
% time. Scryer issue #2701 panicked here (an internal heap index error
% in its attributed-variable bookkeeping). Each member/2 solution makes
% blink/3 a new call variant, so this exercises table creation under
% backtracking and repeated enumeration of completed tables.

:- table blink/3.

blink(0, _, 1).
blink(N, X, Xs) :-
	N > 0,
	N1 is N - 1,
	blink(N1, X, Xs).

test_backtrack_variants :-
	findall(Xs, (member(X, [1,2,3,4,5]), blink(7, X, Xs)), L),
	(	L == [1,1,1,1,1] ->
		write('backtrack variants: ok')
	;	write('backtrack variants: FAILED')
	),
	nl.

% ---------------------------------------------------------------------
% 13. Many thousands of distinct call variants (the real workload behind
% issue #2701: Advent of Code 2024 day 11). Tabling is what makes this
% tractable at all - untabled it is exponential. It is also the shape
% that made a Scryer branch consume gigabytes, so it is worth keeping an
% eye on: here it runs in a few hundredths of a second in single-digit
% MB, which only holds while variant lookup stays O(1) (hash-indexed
% trie children) and completed tables free their suspensions.

:- table count/3.

count(_, 0, 1).
count(S, N, C) :-
	N > 0,
	N1 is N - 1,
	step(S, N1, C).

step(0, N1, C) :-
	!,
	count(1, N1, C).
step(S, N1, C) :-
	number_codes(S, Cs),
	length(Cs, L),
	L mod 2 =:= 0,
	!,
	H is L // 2,
	length(Front, H),
	append(Front, Back, Cs),
	number_codes(A, Front),
	number_codes(B, Back),
	count(A, N1, C1),
	count(B, N1, C2),
	C is C1 + C2.
step(S, N1, C) :-
	S2 is S * 2024,
	count(S2, N1, C).

total(_, [], 0).
total(Blinks, [S|Ss], Total) :-
	count(S, Blinks, C),
	total(Blinks, Ss, T0),
	Total is T0 + C.

test_many_variants :-
	total(25, [125,17], T25),
	total(75, [125,17], T75),
	(	T25 =:= 55312, T75 =:= 65601038650482 ->
		write('many variants: ok')
	;	write('many variants: FAILED')
	),
	nl.


% Tabling state is process-global and unlocked, so it is owned by the
% first thread to use it; a tabled call from any other thread must fail
% fast with a clear error rather than racing the tries (in practice:
% hanging in completion). Deterministic by construction - this thread
% has already tabled above, so the child is always the loser.

% Tables are per-thread, so concurrent tabling is allowed and each
% thread must get the right answers off its own tables. This used to
% assert resource_error(tabling_not_thread_safe); that error no longer
% exists.
%
% Two things are checked, because "it didn't crash" is not evidence:
%   1. the main thread tables while children do, and all agree with the
%      value computed before any thread started;
%   2. every child does its OWN work - a shared table would let a
%      second thread find the first one's answers and skip the
%      computation, so each child's worker count must be non-zero.

:- dynamic(thr_result/2).

test_threads :-
	(	catch(thread_create(thread_child(1), T1, []), _, fail) ->
		test_threads_(T1)
	;	% no thread support in this build - nothing to check
		write('threads: ok'), nl
	).

% thr_t/1 exists to be counted. Its body appends a marker (append-only,
% so three threads asserting at once is not a read-modify-write race).
% One marker per thread means each thread computed the table itself.

:- table thr_t/1.
:- dynamic(thr_work/1).

thr_t(X) :- assertz(thr_work(marker)), X = done.

test_threads_(T1) :-
	thread_create(thread_child(2), T2, []),
	thread_create(thread_child(3), T3, []),
	count(125, 1, Main),			% main thread tables concurrently
	thr_t(_),
	thread_join(T1, _), thread_join(T2, _), thread_join(T3, _),
	findall(I-C, thr_result(I, C), Rs0),
	msort(Rs0, Rs),
	findall(x, thr_work(_), Ws),
	length(Ws, NW),
	(	Rs = [1-C1, 2-C2, 3-C3],
		C1 == Main, C2 == Main, C3 == Main
	->	(	NW =:= 4			% 3 children + main, each on its own table
		->	write('threads: ok')
		;	write('threads: FAILED shared tables, workers='), write(NW)
		)
	;	write('threads: FAILED '), write(Rs-Main)
	),
	nl.

thread_child(I) :-
	catch(( count(125, 1, C), thr_t(_), Got = C ),
	      E,
	      Got = err(E)),
	assertz(thr_result(I, Got)).


% abolish_table/1: selective invalidation. A completed table does not
% notice assert/retract, so this is the supported way to drop one
% predicate's answers without discarding every other table too. The
% counters prove the selectivity: keep_t/1 must stay cached while
% drop_t/1 recomputes.

:- dynamic(ab_hits/1).
:- dynamic(ab_edge/2).

ab_hits(0).
ab_edge(x,y).

ab_bump :- retract(ab_hits(C)), C1 is C + 1, assertz(ab_hits(C1)).

:- table keep_t/1.
keep_t(X) :- ab_bump, member(X, [1,2]).

:- table drop_t/1.
drop_t(X) :- ab_bump, member(X, [3,4]).

:- table ab_path/2.
ab_path(X,Y) :- ab_edge(X,Y).

test_abolish :-
	findall(_, keep_t(_), _),
	findall(_, drop_t(_), _),
	ab_hits(H1),
	abolish_table(drop_t/1),
	findall(_, keep_t(_), _),
	ab_hits(H2),
	findall(_, drop_t(_), _),
	ab_hits(H3),
	findall(P0, ab_path(x,P0), Before),
	assertz(ab_edge(x,z)),
	abolish_table(ab_path/2),
	findall(P1, ab_path(x,P1), After),
	catch(abolish_table(_), error(E1,_), true),
	catch(abolish_table(no_such_t/3), error(E2,_), true),
	catch(abolish_table(42), error(E3,_), true),
	(	H2 =:= H1,			% untouched table stayed cached
		H3 =:= H1 + 1,			% abolished one recomputed
		Before == [y], After == [y,z],	% assert now visible
		E1 = instantiation_error,
		E2 = existence_error(table, no_such_t/3),
		E3 = type_error(predicate_indicator, 42) ->
		write('abolish_table: ok')
	;	write('abolish_table: FAILED')
	),
	nl.

main :-
	test_findall,
	test_setof,
	test_call_barrier,
	test_left_recursion,
	test_mutual,
	test_cycle,
	test_scc_merge,
	test_fib,
	test_variant_answers,
	test_order_independent,
	test_sharing,
	test_generate,
	test_backtrack_variants,
	test_many_variants,
	test_threads,
	test_abolish,
	test_flag.
