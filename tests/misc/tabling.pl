:- initialization(main).

% Tabling regression tests.
%
% Each of these was a real bug found by running third-party programs
% (SWI-Prolog samples, Logtalk's tabling example) against the native
% tabling engine. They are cheap and cover the awkward corners:
% suspension vs completion, continuation capture across barriers, and
% termination on cyclic call graphs.

:- use_module(library(tabling)).
:- use_module(library(lists)).

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

main :-
	test_findall,
	test_setof,
	test_call_barrier,
	test_left_recursion,
	test_mutual,
	test_cycle,
	test_fib,
	test_variant_answers,
	test_order_independent,
	test_sharing,
	test_generate,
	test_flag.
