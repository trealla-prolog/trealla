% Task scheduler: dispatch fairness, timers, and the wait/0 - await/0
% split. Nothing here touches the network, so it is deterministic.
%
% Before the scheduler was rewritten around a ready queue, a timer heap
% and a descriptor list, wait/0 and await/0 were two copies of one scan
% over q->tasks. Three properties below are regression guards for what
% that scan got wrong:
%
%   - dispatch was LIFO, and the scan stopped at the 64th spawned task,
%     so past 64 tasks the oldest ones starved (fair_order, backlog).
%   - end_wait/0 was honoured by wait/0 but not await/0, which left the
%     flag set for the *next* wait/0 to trip over (end_wait_once).
%
% Anything asserting an interleaving of independent tasks is avoided on
% purpose - see the note in tests/misc/sockets.pl for what that costs.

:- initialization(main).

emit(N) :- write(N), write(' ').
square(N) :- M is N*N, send(M).
napper(N) :- S is N/20, sleep(S), write(N), write(' ').

drain(Sum) :- drain_(0, Sum).
drain_(Acc, Sum) :-
	(	recv(X)
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

% await/0 returns once per task that signalled, and fails once no tasks
% are left - which is what makes it usable as a loop condition.

step :-
	forall(between(1,8,N), call_task(square,N)),
	step_(0, Count, 0, Sum),
	(	Count =:= 8, Sum =:= 204
	->	format("step: ok~n")
	;	format("step: FAILED ~w ~w~n", [Count,Sum])
	).

step_(C0, C, S0, S) :-
	(	await
	->	(	recv(X)
		->	C1 is C0 + 1, S1 is S0 + X
		;	C1 = C0, S1 = S0
		),
		step_(C1, C, S1, S)
	;	C = C0, S = S0
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
	step,
	end_wait_once,
	timers,
	yields.

% yield/0 is a requeue, not a sleep: it asks for no deadline and goes
% straight back on the ready queue. It used to be worth a millisecond
% each - do_yield() clamped a zero delay up to 1 - so this loop alone
% took over half a second.
%
% It must still not read as a message. "Yielded with no deadline" is how
% the scheduler recognises the signal send/1 raises, so a plain yield
% carries a mark of its own; without it, await/0 below would return for
% every yield rather than once for the send.

yielder(0) :- !, send(done).
yielder(N) :- yield, N1 is N-1, yielder(N1).

awaits(A, C) :- ( await -> A1 is A+1, awaits(A1, C) ; C = A ).

yields :-
	call_task(yielder, 2000),
	awaits(0, C),
	(	C =:= 1
	->	format("yields: ok~n")
	;	format("yields: FAILED await returned ~w times~n", [C])
	).
