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

main :-
	test_findall,
	test_setof,
	test_call_barrier,
	test_left_recursion,
	test_mutual,
	test_cycle,
	test_fib,
	test_flag.
