% library(actors): links and death notices on top of threads.
%
% Needs real threads, so tests/misc rather than tests/sundry.
%
% Pids are not printed - they are thread ids and depend on what else the
% run has created.

:- use_module(library(actors)).

:- initialization(main).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% A message sent to an actor comes back to the sender.

echo :-
	actor_recv(M),
	(	M = stop
	->	true
	;	actor_self(Me), actor_send(M, pong(Me)), echo
	).

roundtrip :-
	actor_self(Me),
	actor_spawn(echo, P),
	actor_send(P, Me),
	( actor_recv(pong(_), [timeout(2)]) -> R = pong ; R = timeout ),
	actor_send(P, stop),
	report(roundtrip, R, pong).

% A linked actor's death is delivered as exit(Pid, Reason), and the
% reason distinguishes all three ways a goal can end.

died(Goal, Reason) :-
	actor_spawn(Goal, P, [link(true)]),
	( actor_recv(exit(P, R), [timeout(2)]) -> Reason = R ; Reason = timeout ),
	catch(thread_join(P, _), _, true).

link_reports_success   :- died(true,        R), report(link_reports_success,   R, true).
link_reports_exception :- died(throw(oops), R), report(link_reports_exception, R, exception(oops)).
link_reports_failure   :- died(fail,        R), report(link_reports_failure,   R, false).

% Without a link there is no notice.

unlinked_is_silent :-
	actor_spawn(true, P, []),
	( actor_recv(exit(_,_), [timeout(0.3)]) -> R = notified ; R = silent ),
	catch(thread_join(P, _), _, true),
	report(unlinked_is_silent, R, silent).

% actor_unlink/1 takes it back.

unlink_stops_notice :-
	message_queue_create(Gate),
	actor_spawn(thread_get_message(Gate, go), P, [link(true)]),
	actor_unlink(P),
	thread_send_message(Gate, go),
	( actor_recv(exit(_,_), [timeout(0.3)]) -> R = notified ; R = silent ),
	catch(thread_join(P, _), _, true),
	report(unlink_stops_notice, R, silent).

% Supervision: one_for_one with a restart budget.

:- dynamic(runs/1).

% Dies immediately, so the budget is what stops the restarting.

flaky :- assertz(runs(x)), fail.

% Restarts are bounded by max_restarts: one initial start plus three
% restarts, then the supervisor gives up rather than spinning.

restart_budget_is_respected :-
	retractall(runs(_)),
	supervisor_start([flaky], _, [max_restarts(3), period(5)]),
	sleep(0.6),
	findall(1, runs(_), L),
	length(L, N),
	report(restart_budget_is_respected, N, 4).

% A child that stays alive is started once and left alone.

steady :- assertz(runs(y)), actor_self(Me), thread_get_message(Me, never).

steady_child_not_restarted :-
	retractall(runs(_)),
	supervisor_start([steady], Sup),
	sleep(0.3),
	findall(1, runs(_), L),
	length(L, N),
	supervisor_stop(Sup),
	sleep(0.2),
	report(steady_child_not_restarted, N, 1).

main :-
	roundtrip,
	link_reports_success,
	link_reports_exception,
	link_reports_failure,
	unlinked_is_silent,
	unlink_stops_notice,
	restart_budget_is_respected,
	steady_child_not_restarted.
