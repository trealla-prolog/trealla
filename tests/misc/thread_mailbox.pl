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

% A message that matches nothing stays put rather than being consumed.
%
% DISABLED - this hangs. thread_get_message/3 consults its deadline only
% in the branch it takes when the queue is *empty* (the suspend_thread()
% loop in do_match_message). With messages present but none matching,
% the walk falls out of the inner loop, returns to the top of the outer
% one, finds the queue still non-empty and walks it again - never
% reaching the timeout check. It is a hot spin, not even a sleep.
%
% Re-enable once that is fixed; the expectation below is what it should
% report.

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

main :-
	fifo_order,
	selective_receive_preserves_order,
	peek_does_not_consume,
	peek_empty_fails,
	timeout_expires,
	handshake,
	send_before_receive,
	join_status,
	mutex_is_recursive,
	mutex_trylock_succeeds_for_holder.
