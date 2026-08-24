% task_cancel/1 - cross-thread-safe cooperative task cancellation.
% Everything here is single-threaded (same-thread cancel only);
% cancelling a task from a real, different OS thread is what motivated
% the design (see the comment on cancel_requested in src/internal.h -
% writing `error = true` directly from a foreign thread would race
% with the packed bool:1 flags start() mutates constantly) but needs a
% real thread to actually exercise cross-thread, so belongs in
% tests/misc.
%
% Cancellation is cooperative, not preemptive: it lands at the next
% scheduling checkpoint (sched_run()'s dispatch, right before a task
% would otherwise run again), not mid-instruction. The looper below
% yields every iteration, so that checkpoint comes up every single
% tick - deliberately the easiest case, not a worst case.

:- initialization(main).

:- dynamic(tick/1).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

looper(N) :-
	retractall(tick(_)),
	assertz(tick(N)),
	N1 is N + 1,
	yield,
	looper(N1).

% Cancel a running task; it must actually stop (tick/1 stops changing)
% rather than merely being asked to.

cancel_stops_task :-
	retractall(tick(_)),
	assertz(tick(0)),
	task_create(looper(0), Qid),
	forall(between(1,20,_), yield),
	tick(Before),
	task_cancel(Qid),
	wait,
	tick(After),
	forall(between(1,20,_), yield),
	tick(Final),
	( After =< Before + 1, Final == After -> R = ok ; R = failed ),
	report(cancel_stops_task, R, ok).

% Cancelling a qid that does not exist (or already finished) is an
% error, not a silent no-op.

cancel_missing_qid_throws :-
	( catch(task_cancel(999999999), error(existence_error(task,_),_), true)
	-> R = threw_existence_error
	;  R = did_not_throw
	),
	report(cancel_missing_qid_throws, R, threw_existence_error).

% task_cancel/1 only takes tasks, not any addressable qid - the
% top-level query's own qid is registered and sendable-to (see
% task_messaging.pl) but is not a task, and cancelling it makes no
% sense.

cancel_non_task_throws :-
	task_self(Me),
	( catch(task_cancel(Me), error(existence_error(task,_),_), true)
	-> R = threw_existence_error
	;  R = did_not_throw
	),
	report(cancel_non_task_throws, R, threw_existence_error).

% A cancelled task's own mailbox is drained on teardown, not leaked -
% send it something it will never get to read, then cancel it.

doomed_waiter :- recv(never_arrives, [timeout(30.0)]).

cancel_with_pending_mail :-
	task_create(doomed_waiter, Qid),
	send(Qid, unread(1)),
	send(Qid, unread(2)),
	task_cancel(Qid),
	wait,
	report(cancel_with_pending_mail, done, done).

main :-
	cancel_stops_task,
	cancel_missing_qid_throws,
	cancel_non_task_throws,
	cancel_with_pending_mail.
