% recv/2 - the blocking counterpart to recv/1, added to close the gap
% library(task_actors)'s task_actor_recv/1,2 used to paper over with a
% yield/0 busy-loop. Parks the task via do_yield() rather than
% spinning - see the comment on it in src/bif_tasks.c for the mechanism
% (the same do_wait_message() pattern thread_get_message/3's blocking
% form already used) and the bug that shipped once and got fixed
% before landing (do_yield() is a correct no-op for a non-task query,
% which silently made recv/2 "succeed" instantly without checking
% anything - covered here by exercising it from the top-level query
% directly, not just from inside a task).
%
% Everything here is single-threaded, which bounds what it can cover.
% recv/2 called from *within* a task can be woken by another
% cooperative task on the same thread - task_path_blocks_then_wakes
% below exercises exactly that, with wait/0 driving both. recv/2
% called from the top-level query cannot: the non-task path blocks the
% real OS thread directly (see the comment on it in src/bif_tasks.c),
% which does not hand control to the scheduler at all, so a
% cooperative task spawned to wake it would simply never run while it
% waits. Proving that path wakes on an actual send needs a second real
% OS thread and belongs in tests/misc; what timeout_fires and
% zero_timeout_is_nonblocking below cover instead is the bug that
% shipped once - do_yield()'s no-op for a non-task query silently
% making recv/2 "succeed" against a mailbox that was never checked at
% all. Both exercise the non-task path directly, with nothing ever
% sent, and both require it to genuinely wait rather than return
% instantly.

:- initialization(main).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% With nothing ever sent, recv/2 with a timeout must return rather than
% hang, and must not return before roughly the timeout has elapsed.

timeout_fires :-
	get_time(T0),
	( recv(nothing_sent, [timeout(0.15)]) -> R = matched ; R = timed_out ),
	get_time(T1),
	Elapsed is T1 - T0,
	( R == timed_out, Elapsed >= 0.1 -> Ok = true ; Ok = false ),
	report(timeout_fires, Ok, true).

% timeout(0) is a valid, degenerate case: recv/1's exact behaviour
% (check once, do not wait), reachable through recv/2 too.

zero_timeout_is_nonblocking :-
	get_time(T0),
	( recv(nothing_sent, [timeout(0)]) -> R = matched ; R = timed_out ),
	get_time(T1),
	Elapsed is T1 - T0,
	( R == timed_out, Elapsed < 0.5 -> Ok = true ; Ok = false ),
	report(zero_timeout_is_nonblocking, Ok, true).

% An unrecognised option is an error, not something silently ignored -
% matches thread_get_message/3's own strictness on this.

bad_option_throws :-
	( catch(recv(_, [bogus(1)]), error(domain_error(read_option,_),_), true)
	-> R = threw_domain_error
	;  R = did_not_throw
	),
	report(bad_option_throws, R, threw_domain_error).

% The task-parking path (q->is_task true): a cooperative sender yields
% a few times before sending, so the receiver genuinely parks and
% resumes rather than finding the message already sitting there.

waiter(ParentQid) :-
	recv(go, [timeout(5.0)]),
	send(ParentQid, waiter_done).

delayed_cooperative_sender(TargetQid) :-
	yield, yield, yield,
	send(TargetQid, go).

task_path_blocks_then_wakes :-
	task_self(Me),
	task_create(waiter(Me), WaiterQid),
	call_task(delayed_cooperative_sender(WaiterQid)),
	wait,
	( recv(waiter_done) -> R = ok ; R = no_message ),
	report(task_path_blocks_then_wakes, R, ok).

main :-
	timeout_fires,
	zero_timeout_is_nonblocking,
	bad_option_throws,
	task_path_blocks_then_wakes.
