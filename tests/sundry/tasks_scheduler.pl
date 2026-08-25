% Task scheduler: dispatch fairness and timers. Nothing here touches the
% network, so it is deterministic.
%
% Before the scheduler was rewritten around a ready queue, a timer heap
% and a descriptor list, wait/0 and await/0 were two copies of one scan
% over the task list. Two properties below are regression guards for
% what that scan got wrong:
%
%   - dispatch was LIFO, and the scan stopped at the 64th spawned task,
%     so past 64 tasks the oldest ones starved (fair_order, backlog).
%   - end_wait/0 was honoured by wait/0 but not await/0, which left the
%     flag set for the *next* wait/0 to trip over (end_wait_once).
%
% Tasks reported over send/1 and were read with recv/1 until GUSTTO
% phase 1 removed both, along with await/0. They use the shared database
% now. What went with await/0 is the check that it returned once per
% signalling task - there are no signals left to count.
%
% Anything asserting an interleaving of independent tasks is avoided on
% purpose - see the note in tests/misc/sockets.pl for what that costs.


:- initialization(main).

:- dynamic(result/1).

emit(N) :- write(N), write(' ').
square(N) :- M is N*N, assertz(result(M)).
napper(N) :- S is N/20, sleep(S), write(N), write(' ').

drain(Sum) :- drain_(0, Sum).
drain_(Acc, Sum) :-
	(	retract(result(X))
	->	Acc1 is Acc + X, drain_(Acc1, Sum)
	;	Sum = Acc
	).

% Tasks run in the order they were spawned.

fair_order :-
	forall(between(1,10,N), call_task(emit,N)),
	wait, nl.

% Well past the old 64-task cap: every task must get its turn, not just
% the first 64 to be scanned.

backlog :-
	forall(between(1,200,N), call_task(square,N)),
	wait, drain(Sum),
	Expect is (200*201*401)//6,
	(	Sum =:= Expect
	->	format("backlog: ok~n")
	;	format("backlog: FAILED ~w vs ~w~n", [Sum,Expect])
	).

% A task calling end_wait/0 releases the parent from wait/0. The flag
% must not survive into the next wait/0, which used to make it return
% before running anything.

stopper :- end_wait.

end_wait_once :-
	call_task(stopper),
	call_task(square, 9),
	wait,
	call_task(square, 10),
	wait,
	drain(Sum),
	(	Sum =:= 181
	->	format("end_wait_once: ok~n")
	;	format("end_wait_once: FAILED ~w~n", [Sum])
	).

% Sleeping tasks wake in deadline order, not spawn order. The gaps are
% 50ms so this does not depend on a lightly loaded machine.

timers :-
	forall(member(N,[3,1,2]), call_task(napper,N)),
	wait, nl.

main :-
	fair_order,
	backlog,
	end_wait_once,
	timers,
	yields.

% yield/0 is a requeue, not a sleep: it asks for no deadline and goes
% straight back on the ready queue. It used to be worth a millisecond
% each - do_yield() clamped a zero delay up to 1 - so this loop alone
% took over half a second.
%
% With await/0 gone this can no longer count scheduler wakeups, so what
% is left guards the requeue path itself: 2000 yields must run to
% completion rather than stalling or starving the task.

yielder(0) :- !, assertz(result(0)).
yielder(N) :- yield, N1 is N-1, yielder(N1).

yields :-
	call_task(yielder, 2000),
	wait,
	(	retract(result(0))
	->	format("yields: ok~n")
	;	format("yields: FAILED yielder did not finish~n")
	).
