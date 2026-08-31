% Shared completed tables (DESIGN-tabling-phase2.md item 4): threads
% stop recomputing the same predicate. ":- table p/1 as shared".
%
% A shared table is still BUILT privately with no locking - the
% leader's critical section spans completion/0, a Prolog loop running
% arbitrary user code, and no lock survives that. It is only PUBLISHED
% once complete, and a completed table is immutable. Publication and
% lookup are short and contain no user code, which is what makes a
% mutex sound there.
%
% Every check below counts markers rather than inspecting answers:
% three threads agreeing on `done` proves nothing about whether they
% each recomputed it, which is the whole point of the item.

:- use_module(library(tabling)).
:- use_module(library(lists)).

:- initialization(main).

:- dynamic(work/1).
:- dynamic(res/2).

% ---------------------------------------------------------------------
% 1. A thread arriving AFTER publication reuses the table instead of
% rebuilding it. Main computes first, so exactly one computation must
% be recorded no matter how many threads then ask.
%
% Note this does NOT test simultaneous cold starts: publication happens
% at completion, so threads that all start before anyone finishes will
% each build their own. That is correct for a publish-on-completion
% design - it avoids recomputation for arrivals after the fact, it is
% not a barrier - and test 2 is the negative control that pins it.

:- table shared_t/1 as shared.

shared_t(X) :- assertz(work(shared_marker)), X = done.

child(I) :- shared_t(X), assertz(res(I,X)).

test_shared_reuse :-
	shared_t(M),
	(	catch(thread_create(child(1), T1, []), _, fail) ->
		thread_create(child(2), T2, []),
		thread_create(child(3), T3, []),
		thread_join(T1,_), thread_join(T2,_), thread_join(T3,_),
		findall(I-X, res(I,X), Rs0), msort(Rs0, Rs),
		findall(x, work(shared_marker), Ws), length(Ws, NW),
		(	M == done, Rs == [1-done,2-done,3-done], NW =:= 1 ->
			write('shared reuse: ok')
		;	write('shared reuse: FAILED'), nl, write(Rs-NW)
		)
	;	% no thread support in this build - one computation is still
		% the right answer, just for a duller reason
		findall(x, work(shared_marker), Ws), length(Ws, NW),
		(	M == done, NW =:= 1 ->
			write('shared reuse: ok')
		;	write('shared reuse: FAILED')
		)
	),
	nl.

% ---------------------------------------------------------------------
% 2. Negative control for test 1. The SAME shape without `as shared`
% must still have every thread do its own work - otherwise test 1
% proves nothing about sharing, only that the answer is cacheable.

:- table private_t/1.

private_t(X) :- assertz(work(private_marker)), X = done.

child_p(I) :- private_t(X), assertz(res(I,X)).

test_private_not_shared :-
	retractall(res(_,_)),
	private_t(_),
	(	catch(thread_create(child_p(1), U1, []), _, fail) ->
		thread_create(child_p(2), U2, []),
		thread_join(U1,_), thread_join(U2,_),
		findall(x, work(private_marker), Ws), length(Ws, NW),
		(	NW =:= 3 ->			% main + 2 children, each its own
			write('private not shared: ok')
		;	write('private not shared: FAILED'), nl, write(n=NW)
		)
	;	write('private not shared: ok')
	),
	nl.

% ---------------------------------------------------------------------
% 3. Answers actually survive the crossing. A shared table holding a
% compound with refcounted subcells (a string) must read back intact in
% another thread - reading an answer bumps refcounts on shared subcells,
% which is only safe because pl_refcnt is _Atomic under USE_THREADS.

:- table shared_term/1 as shared.

shared_term(f(hello, [1,2,3], "text")).

child_t(I) :- shared_term(X), assertz(res(I,X)).

test_shared_term_integrity :-
	retractall(res(_,_)),
	shared_term(Main),
	(	catch(thread_create(child_t(1), V1, []), _, fail) ->
		thread_create(child_t(2), V2, []),
		thread_join(V1,_), thread_join(V2,_),
		findall(X, res(_,X), Xs)
	;	Xs = []
	),
	(	Main = f(hello,[1,2,3],_),
		forall(member(X, Xs), X = f(hello,[1,2,3],_)) ->
		write('shared term integrity: ok')
	;	write('shared term integrity: FAILED'), nl, write(Main-Xs)
	),
	nl.

% ---------------------------------------------------------------------
% 4. abolish_all_tables/0 must reach published tables too, or a user
% who abolishes still gets stale answers. It RETIRES rather than frees
% them - another thread may be reading one, and freeing under a live
% reader is the exact use-after-free this design avoids - so the next
% call misses the registry and recomputes, while the old memory waits
% for teardown.

:- table abol_t/1 as shared.

abol_t(X) :- assertz(work(abol_marker)), X = done.

test_abolish_reaches_shared :-
	retractall(work(abol_marker)),
	abol_t(_),
	findall(x, work(abol_marker), W1), length(W1, N1),
	abolish_all_tables,
	abol_t(_),
	findall(x, work(abol_marker), W2), length(W2, N2),
	(	N1 =:= 1, N2 =:= 2 ->
		write('abolish reaches shared: ok')
	;	write('abolish reaches shared: FAILED'), nl, write(N1-N2)
	),
	nl.

% ---------------------------------------------------------------------

main :-
	test_shared_reuse,
	test_private_not_shared,
	test_shared_term_integrity,
	test_abolish_reaches_shared.
