% The thread mailbox, queues, join and mutexes - pinned as they behave
% today, before phase 1 of GUSTTO rewrites the blocking underneath them.
%
% This lives in tests/misc because it needs real threads, and the WASI
% build in CI runs `make test` with NOTHREADS. Nothing here is otherwise
% platform-specific.
%
% Why this file matters more than it looks: phase 1 replaces the condvar
% wait inside do_match_message() with a task parking on the queue and
% being woken by the send. That is a rewrite of the mechanism underneath
% every property below, and these are the properties that must come out
% the other side unchanged.
%
% Two behaviours recorded here are worth arguing about rather than
% preserving blindly; both are marked at the point they are asserted.
%
% Every test is made deterministic by a join or by a queue handshake -
% nothing asserts an interleaving of threads running concurrently.

:- initialization(main).

:- dynamic(echoed/1).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% Drain a queue without blocking, so a test can never hang on one.

qdrain(Q, L) :- qdrain_(Q, [], R), reverse(R, L).
qdrain_(Q, A, L) :-
	(	thread_get_message(Q, X, [timeout(0.05)])
	->	qdrain_(Q, [X|A], L)
	;	L = A
	).

% Messages come back in the order they were sent.

fifo_order :-
	message_queue_create(Q),
	forall(member(M,[a,b,c]), thread_send_message(Q,M)),
	qdrain(Q, L),
	report(fifo_order, L, [a,b,c]).

% Selective receive scans the queue without disturbing it: taking 3 out
% of 1,2,3,4 leaves 1,2,4 in that order.
%
% This is the property that makes this mailbox worth keeping and the
% task-side recv/1 not - given the same queue, recv/1 rotates the
% skipped messages to the back and leaves 4,1,2.

selective_receive_preserves_order :-
	message_queue_create(Q),
	forall(between(1,4,N), thread_send_message(Q,N)),
	thread_get_message(Q, 3),
	qdrain(Q, L),
	report(selective_receive_preserves_order, L, [1,2,4]).

% A message that matches nothing stays put rather than being consumed,
% and the receive still honours its deadline.
%
% This hung until the deadline check was added to the no-match path.
% thread_get_message/3 consulted its timeout only in the branch taken
% when the queue is *empty*, so with messages present but none matching
% the walk fell out of the inner loop, returned to the top of the outer
% one, found the queue still non-empty and walked it again forever -
% spinning, not even sleeping. Intermittent by nature: it needed the
% queue to be non-empty at the moment of the receive.

no_match_leaves_queue_intact :-
	message_queue_create(Q),
	forall(member(M,[x,y]), thread_send_message(Q,M)),
	(	thread_get_message(Q, zzz, [timeout(0.05)])
	->	R = matched_wrongly
	;	R = no_match
	),
	qdrain(Q, L),
	report(no_match_leaves_queue_intact, R-L, no_match-[x,y]).

% Peek does not consume, and fails on an empty queue rather than
% blocking.

peek_does_not_consume :-
	message_queue_create(Q),
	thread_send_message(Q, p),
	(	thread_peek_message(Q, p) -> P = peeked ; P = not_peeked ),
	qdrain(Q, L),
	report(peek_does_not_consume, P-L, peeked-[p]).

peek_empty_fails :-
	message_queue_create(Q),
	(	thread_peek_message(Q, _) -> R = unexpected ; R = fails ),
	report(peek_empty_fails, R, fails).

% The timeout form gives up rather than waiting forever.

timeout_expires :-
	message_queue_create(Q),
	(	thread_get_message(Q, _, [timeout(0.1)]) -> R = unexpected ; R = timed_out ),
	report(timeout_expires, R, timed_out).

% A real handshake across two threads. The join is what makes this
% deterministic: by the time it returns, the worker has run.

handshake :-
	retractall(echoed(_)),
	message_queue_create(Q),
	thread_create((thread_get_message(Q,X), assertz(echoed(X))), T, []),
	thread_send_message(Q, ping),
	thread_join(T, _),
	findall(E, echoed(E), L),
	report(handshake, L, [ping]).

% A message sent before anyone is waiting is not lost.

send_before_receive :-
	retractall(echoed(_)),
	message_queue_create(Q),
	thread_send_message(Q, early),
	thread_create((thread_get_message(Q,X), assertz(echoed(X))), T, []),
	thread_join(T, _),
	findall(E, echoed(E), L),
	report(send_before_receive, L, [early]).

% What join reports, in the same vocabulary as
% thread_property(_, status(S)).
%
% Until this test was written, a goal that failed and a goal that threw
% both came back as plain `true`: the thread recorded the ball but join
% never looked at it, and failure was not recorded at all. Fixed on the
% way in, so these are now the SWI values.

join_status :-
	thread_create(true, T1, []),            thread_join(T1, S1),
	thread_create(fail, T2, []),            thread_join(T2, S2),
	thread_create(throw(oops), T3, []),     thread_join(T3, S3),
	thread_create(thread_exit(bye), T4, []),thread_join(T4, S4),
	report(join_status_true,  S1, true),
	report(join_status_fail,  S2, false),
	report(join_status_throw, S3, exception(oops)),
	report(join_status_exit,  S4, exited(bye)).

% Mutexes are recursive: the holder may lock again without deadlocking,
% and must unlock as many times as it locked.

mutex_is_recursive :-
	mutex_create(M),
	mutex_lock(M), mutex_lock(M),
	mutex_unlock(M), mutex_unlock(M),
	report(mutex_is_recursive, ok, ok).

% mutex_trylock/1 succeeds for the holder and reports rather than blocks.

mutex_trylock_succeeds_for_holder :-
	mutex_create(M),
	mutex_lock(M),
	(	mutex_trylock(M) -> R = acquired, mutex_unlock(M) ; R = refused ),
	mutex_unlock(M),
	report(mutex_trylock_succeeds_for_holder, R, acquired).

% Properties of an object created without an alias.
%
% All three property predicates used to build an alias/1 term out of a
% null pointer, so make_cstring() ran strlen(NULL) and the process
% segfaulted. An object with no alias simply has no alias property; the
% others must still enumerate, which is the part a naive fix breaks.

unaliased_queue_properties :-
	message_queue_create(Q),
	findall(P, message_queue_property(Q,P), L),
	report(unaliased_queue_properties, L, [size(0)]).

unaliased_mutex_properties :-
	mutex_create(M),
	findall(P, mutex_property(M,P), L),
	report(unaliased_mutex_properties, L, [status(unlocked)]).

unaliased_thread_properties :-
	message_queue_create(Q),
	thread_create(thread_get_message(Q,_), T, []),
	findall(P, thread_property(T,P), L),
	thread_send_message(Q, go),
	thread_join(T, _),
	report(unaliased_thread_properties, L, [detached(false),status(running)]).

% An alias, where there is one, still shows up alongside the rest.

aliased_queue_properties :-
	message_queue_create(Q, [alias(a_queue)]),
	findall(P, message_queue_property(Q,P), L),
	report(aliased_queue_properties, L, [alias(a_queue),size(0)]).

% message_queue_property/2 with the property bound enumerated the
% *mutexes*: it filtered on is_mutex_only where its sibling with both
% arguments unbound filtered on is_queue_only. It threw an
% existence_error as soon as a mutex existed.

% Written against whatever else this file has left alive, so it asks
% the two questions that matter rather than for an exact list: the new
% queues are found, and the mutex is not.

queue_property_enumerates_queues :-
	message_queue_create(Q1),
	message_queue_create(Q2),
	mutex_create(M),
	findall(X, message_queue_property(X,size(_)), L),
	(	memberchk(Q1, L), memberchk(Q2, L)
	->	Found = queues_found
	;	Found = queues_missing
	),
	(	memberchk(M, L)
	->	Leaked = mutex_leaked_in
	;	Leaked = no_mutex
	),
	report(queue_property_enumerates_queues, Found-Leaked, queues_found-no_mutex).
% A receive inside a *task* must not hold the scheduler.
%
% This is the GUSTTO phase 1 property. A task waiting on a queue parks
% on the timer heap and its siblings run meanwhile; before phase 1 it
% sat on the condvar inside do_match_message and every sibling waited
% out the full timeout with it. The blocker is spawned first, so under
% the old behaviour the siblings could only appear after it finished.
%
% Timing is not asserted - only the order, which is what changed.

:- dynamic(ran/1).

blocker(Q) :- assertz(ran(blocked)),
	( thread_get_message(Q,_,[timeout(0.3)]) -> true ; true ),
	assertz(ran(woke)).

runner(N) :- assertz(ran(sib(N))).

task_receive_yields_to_siblings :-
	retractall(ran(_)),
	message_queue_create(Q),
	call_task(blocker, Q),
	call_task(runner, 1),
	call_task(runner, 2),
	wait,
	findall(X, ran(X), L),
	report(task_receive_yields_to_siblings, L, [blocked,sib(1),sib(2),woke]).

% ... and a parked task still receives, rather than only timing out.

waiter(Q) :- ( thread_get_message(Q,M,[timeout(2)]) -> assertz(ran(got(M))) ; assertz(ran(timed_out)) ).
poster(Q) :- sleep(0.05), thread_send_message(Q, delivered).

parked_task_still_receives :-
	retractall(ran(_)),
	message_queue_create(Q),
	call_task(waiter, Q),
	call_task(poster, Q),
	wait,
	findall(X, ran(X), L),
	report(parked_task_still_receives, L, [got(delivered)]).

main :-
	fifo_order,
	selective_receive_preserves_order,
	no_match_leaves_queue_intact,
	peek_does_not_consume,
	peek_empty_fails,
	timeout_expires,
	handshake,
	send_before_receive,
	join_status,
	mutex_is_recursive,
	mutex_trylock_succeeds_for_holder,
	unaliased_queue_properties,
	unaliased_mutex_properties,
	unaliased_thread_properties,
	aliased_queue_properties,
	queue_property_enumerates_queues,
	task_receive_yields_to_siblings,
	parked_task_still_receives.
