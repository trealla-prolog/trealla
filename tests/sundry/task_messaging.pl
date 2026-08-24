% task_self/1, send/2, recv/1, task_create/2 - GUSTTO phase 5's
% qid-addressed task mailbox. Everything here is single-threaded and
% cooperative (call_task/task_create + wait), on purpose: what needs a
% real OS thread to exercise (cross-thread send/recv, task_cancel) is
% in tests/misc instead.
%
% recv/1 is deliberately non-blocking (matches the pre-GUSTTO send/1
% and recv/1 it replaced) and scans its mailbox in place rather than
% rotating a skipped message to the back - the old array-based queue
% did rotate, which was judged wrong for selective receive, so the new
% list-based mailbox exists specifically to not do that. Two of the
% properties below pin that down.

:- initialization(main).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% task_self/1 returns an integer, consistently, across calls.

self_is_stable :-
	task_self(A),
	task_self(B),
	( integer(A), A == B -> R = ok ; R = failed ),
	report(self_is_stable, R, ok).

% Sending to yourself and receiving back is the smallest possible
% round trip - the top-level query is addressable the same way a task
% is, once it has called task_self/1.

self_send_recv :-
	task_self(Me),
	send(Me, hello(1)),
	( recv(hello(X)) -> R = got(X) ; R = failed ),
	report(self_send_recv, R, got(1)).

% recv/1 does not block: with nothing matching, it fails rather than
% waiting.

recv_nonblocking_fails :-
	( recv(nothing_sent_this_atom) -> R = matched ; R = failed_as_expected ),
	report(recv_nonblocking_fails, R, failed_as_expected).

% Selective receive scans in place. Send a(1) then b(1); asking for
% b(_) first must not disturb a(1)'s position - it should still be
% there, and still first, for a later plain recv/1.

selective_receive_in_place :-
	task_self(Me),
	send(Me, a(1)),
	send(Me, b(1)),
	( recv(b(X)) -> Got1 = b(X) ; Got1 = miss ),
	( recv(c(_)) -> Got2 = matched ; Got2 = miss ),	% nothing to match
	( recv(Y) -> Got3 = Y ; Got3 = miss ),				% a(1), still at the front
	report(selective_receive_in_place, Got1-Got2-Got3, b(1)-miss-a(1)).

% task_create/2 hands back the new task's qid synchronously, before it
% has run a single instruction - unlike task_self/1, which only the
% task itself can call, this is how a caller learns a child's address
% without having to be told.

child_reports_own_qid(ParentQid) :-
	task_self(Me),
	send(ParentQid, hello(Me)).

task_create_qid_matches_self :-
	task_self(MyQid),
	task_create(child_reports_own_qid(MyQid), ChildQid),
	( integer(ChildQid) -> R0 = is_int ; R0 = not_int ),
	wait,
	( recv(hello(Reported)), Reported == ChildQid -> R1 = true ; R1 = false ),
	report(task_create_qid_matches_self, R0-R1, is_int-true).

% task_create/2's qid is usable immediately, before the child has run -
% send/2 to it from the spawner must not need the child to have called
% task_self/1 first.

eager_registration :-
	task_create(recv(ping), ChildQid),
	send(ChildQid, ping),
	wait,
	report(eager_registration, sent_before_run, sent_before_run).

% A parent that spawns a task and calls wait/0 sees it run; the child
% can address the parent back by a qid it was simply handed as an
% argument, no separate discovery step.

cross_task_round_trip(N) :-
	Expect is N * 2,
	task_self(Me),
	task_create(child_double(Me, N), _),
	wait,
	( recv(doubled(N, X)) -> R = X ; R = no_message ),
	report(cross_task_round_trip, R, Expect).

child_double(ParentQid, N) :-
	X is N * 2,
	send(ParentQid, doubled(N, X)).

main :-
	self_is_stable,
	self_send_recv,
	recv_nonblocking_fails,
	selective_receive_in_place,
	task_create_qid_matches_self,
	eager_registration,
	cross_task_round_trip(21).
