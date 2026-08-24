% library(task_actors): actors on tasks rather than threads. See its
% header for why it exists alongside library(thread_actors) rather
% than replacing it, and docs/DESIGN-GUSTTO.md phase 5.
%
% Two things this file cannot cover single-threaded, both noted in
% library/task_actors.pl itself:
%
%   - a task-based supervisor only makes progress while its owning
%     thread drives the scheduler, unlike a thread-based one, which
%     runs in the background just by existing. What is testable here
%     is a supervisor that terminates on its own (restart budget
%     exhausted) - see restart_budget_exhausted below. A supervisor
%     that keeps running and gets stopped mid-flight needs hosting on
%     a thread of its own (the library's own documented pattern), and
%     belongs in tests/misc.
%   - cross-thread send/recv and cancellation, for the same reason as
%     task_messaging.pl and task_cancel.pl.

:- initialization(main).
:- use_module(library(task_actors)).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% Ping-pong: spawn an actor, tell it who to reply to, wait for the
% reply. Basic spawn + send + blocking recv round trip.

pong(ParentQid) :-
	task_actor_recv(ping),
	task_actor_send(ParentQid, pong).

spawn_send_recv :-
	task_actor_self(Me),
	task_actor_spawn(pong(Me), Pid),
	task_actor_send(Pid, ping),
	wait,
	( task_actor_recv(pong) -> R = ok ; R = failed ),
	report(spawn_send_recv, R, ok).

% Links deliver a death notice, exactly once, carrying why: true for
% plain success, false for plain failure, exception(E) for a thrown E.
% All three run linked and unwaited-for; wait/0 (via task's own
% num_subtasks accounting, unrelated to the link mechanism) still
% blocks until all three have reported, since it tracks the whole
% subtree regardless of linking.

dies_ok :- true.
dies_fail :- fail.
dies_throw :- throw(boom).

link_exit_reasons :-
	task_actor_spawn(dies_ok, P1, [link(true)]),
	task_actor_spawn(dies_fail, P2, [link(true)]),
	task_actor_spawn(dies_throw, P3, [link(true)]),
	wait,
	findall(Pid-Reason, (
		between(1, 3, _),
		task_actor_recv(exit(Pid, Reason))
	), Results),
	msort(Results, Sorted),
	report(link_exit_reasons, Sorted, [P1-true, P2-false, P3-exception(boom)]).

% task_actor_recv/2's timeout(T) option is recv/2's, unmodified - a
% waiting actor with nothing arriving must time out, not hang or
% vacuously succeed.

waits_actor(ParentQid) :-
	( task_actor_recv(never, [timeout(0.15)]) -> R = matched ; R = timed_out ),
	task_actor_send(ParentQid, done(R)).

recv_timeout_option :-
	task_actor_self(Me),
	task_actor_spawn(waits_actor(Me), _),
	wait,
	( task_actor_recv(done(R)) -> true ; R = no_message ),
	report(recv_timeout_option, R, timed_out).

% A supervisor whose only child dies immediately, every time, restarts
% it up to the budget and then stops itself - the one supervisor
% scenario testable without a dedicated host thread, since the
% supervisor's own task terminates on its own once the budget is
% blown (see library/task_actors.pl's supervisor comment).

:- dynamic(run_count/1).

flaky :-
	retract(run_count(N)),
	N1 is N + 1,
	assertz(run_count(N1)),
	throw(boom).

restart_budget_exhausted :-
	retractall(run_count(_)),
	assertz(run_count(0)),
	task_supervisor_start([flaky], _Sup, [max_restarts(3), period(5)]),
	wait,
	run_count(Count),
	% max_restarts(3) means the original run plus 3 retries = 4 runs
	% before the budget is called exhausted and the supervisor stops.
	report(restart_budget_exhausted, Count, 4).

main :-
	spawn_send_recv,
	link_exit_reasons,
	recv_timeout_option,
	restart_budget_exhausted.
