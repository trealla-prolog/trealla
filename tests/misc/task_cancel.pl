% task_cancel/1's cross-thread behaviour - the reason cancel_requested
% exists as a standalone atomic rather than a direct `error = true`
% write (see tests/sundry/task_cancel.pl and the field's comment in
% src/internal.h). Needs real threads, so tests/misc rather than
% tests/sundry.
%
% Progress is observed by accumulating facts rather than retract+assert
% of one mutable value: a concurrent reader could catch the gap between
% a retract and its assert and see nothing at all. Growing a list by
% assertz alone has no such gap.

:- use_module(library(aggregate)).
:- use_module(library(lists)).

:- initialization(main).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% A real thread cancels a task belonging to the main thread.

:- dynamic(main_tick/1).

main_looper(N) :-
	assertz(main_tick(N)),
	N1 is N + 1,
	yield,
	main_looper(N1).

main_tick_count(N) :- aggregate_all(count, main_tick(_), N).

canceller(Qid) :-
	sleep(0.05),
	task_cancel(Qid).

cancel_from_other_thread :-
	retractall(main_tick(_)),
	task_create(main_looper(0), Qid),
	thread_create(canceller(Qid), Id, []),
	wait,
	thread_join(Id, _),
	main_tick_count(Stopped),
	sleep(0.05),
	main_tick_count(StillStopped),
	( Stopped > 0, Stopped == StillStopped -> R = ok ; R = failed ),
	report(cancel_from_other_thread, R, ok).

% The main thread cancels a task that belongs to a different thread's
% own scheduler - find_task_by_qid/2 crossing threads to resolve it,
% then sched_promote() reaching into that thread's ready queue.

:- dynamic(remote_tick/1).

remote_looper(N) :-
	assertz(remote_tick(N)),
	N1 is N + 1,
	yield,
	remote_looper(N1).

remote_tick_count(N) :- aggregate_all(count, remote_tick(_), N).

remote_host(ReadyQid) :-
	task_create(remote_looper(0), ChildQid),
	send(ReadyQid, child_qid(ChildQid)),
	wait.

cancel_task_on_other_thread :-
	retractall(remote_tick(_)),
	task_self(Me),
	thread_create(remote_host(Me), HostId, []),
	recv(child_qid(ChildQid), [timeout(2.0)]),
	sleep(0.05),
	remote_tick_count(Before),
	task_cancel(ChildQid),
	thread_join(HostId, _),
	remote_tick_count(After),
	sleep(0.05),
	remote_tick_count(Final),
	( Before > 0, After == Final -> R = ok ; R = failed ),
	report(cancel_task_on_other_thread, R, ok).

% Two real threads racing to cancel the same set of tasks: a second
% cancel of an already-gone qid throws (existence_error), caught here
% rather than checked - what this proves is that the race does not
% crash or hang, which is what the test harness itself verifies via
% exit status (see tests/run_misc.sh).

:- dynamic(race_alive/1).

race_looper(Id) :-
	assertz(race_alive(Id)),
	yield,
	race_looper(Id).

race_canceller(Qids) :-
	forall(member(Qid, Qids), (sleep(0.005), catch(task_cancel(Qid), _, true))).

concurrent_cancel_race :-
	retractall(race_alive(_)),
	findall(Qid, (
		between(1, 30, N),
		task_create(race_looper(N), Qid)
	), Qids),
	thread_create(race_canceller(Qids), Id1, []),
	thread_create(race_canceller(Qids), Id2, []),
	wait,
	thread_join(Id1, _),
	thread_join(Id2, _),
	report(concurrent_cancel_race, done, done).

main :-
	cancel_from_other_thread,
	cancel_task_on_other_thread,
	concurrent_cancel_race.
