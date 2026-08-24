% Skynet - https://github.com/atemerev/skynet
%
%     tpl samples/skynet_tasks.pl -g "run(1000000,10),halt"
%
% Same benchmark as samples/skynet_threads.pl, ported to
% library(task_actors) instead of library(thread_actors). Every actor
% is a task rather than a real OS thread, which is the whole point:
% skynet_threads.pl calls pthread_create/3 once per actor, and at
% size=1000000 that is ~1.11M real threads - the OS thread ceiling,
% not a bug, and no amount of retrying gets past it. Tasks are
% heap-allocated query structs scheduled cooperatively on however many
% real threads happen to exist (here, just the one), so there is no
% analogous ceiling. See docs/DESIGN-GUSTTO.md phase 5.
%
% Each actor spawns Div children covering a slice of the range, sums what
% they report, and reports the total to its parent; a leaf reports its own
% ordinal. Size must be a power of Div, or the integer division reaches 0
% before the base case and the tree never terminates.
%
% task_actor_recv/1 spins on yield/0 rather than truly blocking (recv/1
% never blocked - see library(task_actors)'s header), so every actor
% waiting on its children is burning some CPU polling rather than
% parked. Measure before assuming that costs more than the OS-thread
% version's context-switch overhead did.
%
% This gets past skynet_threads.pl's OS-thread ceiling - no
% pthread_create involved at all - but a first version (spawn all Div
% children, then collect all Div results, matching skynet_threads.pl
% exactly) traded it for a different one: memory. The task scheduler's
% ready queue is FIFO (sched_ready_push/pop, src/bif_tasks.c), so
% cooperative round-robin expands a spawn-everything-then-collect tree
% roughly breadth-first - by the time the deepest leaves get their
% first turn, most of the tree above them already exists as live query
% structs, none of which can finish and free its memory until its own
% children have. Confirmed by total node count, not depth or branch
% factor, driving the failure: size=1000000,div=10 (~1.11M nodes) and
% size=524288,div=2 (~1.05M nodes, same total despite a very different
% depth/branching shape) both died the same way, RSS climbing past
% 7.3GB first. Real OS threads do not have this failure mode at any
% size they actually reach, because preemptive scheduling lets leaves
% finish and exit throughout, not only once the whole level below them
% exists - they just cannot reach a large size in the first place.
%
% Fixed here by spawning and awaiting one child at a time rather than
% all Div up front, capping how many of a node's children are ever
% alive at once at 1 instead of Div. Peak concurrent tasks becomes
% O(depth) rather than O(tree size) - depth is only log_Div(Size), so
% this is what gets size=1000000 to actually finish. The cost: a
% sibling's subtree cannot make progress while an earlier sibling's is
% still running, where skynet_threads.pl (and the first version of this
% file) had all Div running at once. Untried alternative: a LIFO or
% depth-biased ready queue would get the same memory bound without
% serialising siblings, but that is an engine change affecting every
% task, not a benchmark change - a bigger, riskier decision than this
% file should make unilaterally.
%
% Every actor reports to its parent exactly once, either result/1 or
% failed/1 - same reasoning as skynet_threads.pl: an actor that could die
% without reporting would leave its parent blocked forever. Task
% spawning has no equivalent of a thread-creation ceiling to fail on, so
% this matters less here than there, but the shape is kept identical
% for a fair comparison.

:- use_module(library(lists)).
:- use_module(library(task_actors)).

skynet(Parent, Num, 1, _) :-
	!,
	task_actor_send(Parent, result(Num)).

skynet(Parent, Num, Size, Div) :-
	catch(spawn_and_sum(Num, Size, Div, Tot), E,
		( task_actor_send(Parent, failed(E)), throw(E) )),
	task_actor_send(Parent, result(Tot)).

spawn_and_sum(Num, Size, Div, Tot) :-
	NewSize is Size div Div,
	task_actor_self(Me),
	findall(X, (
		between(1, Div, Idx),
		NewNum is ((Idx - 1) * NewSize) + Num,
		task_actor_spawn(skynet(Me, NewNum, NewSize, Div), _),
		'$skynet_recv_result'(X)
	), Xs),
	sum_list(Xs, Tot).

'$skynet_recv_result'(X) :-
	task_actor_recv(Msg),
	(	Msg = result(X) -> true
	;	Msg = failed(E) -> throw(E)
	;	'$skynet_recv_result'(X)
	).

% Reject a Size that is not a power of Div rather than hanging.

exact_levels(1, _, 0) :- !.
exact_levels(Size, Div, N) :-
	Size > 1,
	0 =:= Size mod Div,
	Next is Size div Div,
	exact_levels(Next, Div, N0),
	N is N0 + 1.

run(Size, Div) :-
	(	exact_levels(Size, Div, _)
	->	true
	;	format("size ~w is not a power of ~w~n", [Size,Div]), fail
	),
	task_actor_self(Me),
	get_time(T0),
	task_actor_spawn(skynet(Me, 0, Size, Div), _),
	wait,
	task_actor_recv(Msg),
	get_time(T1),
	Ms is round((T1-T0)*1000),
	Expect is (Size * (Size - 1)) // 2,
	(	Msg = result(Tot)
	->	( Tot =:= Expect -> R = ok ; R = wrong(Tot,Expect) )
	;	Msg = failed(E) -> R = failed(E)
	;	R = unexpected(Msg)
	),
	format("size=~w div=~w -> ~w in ~wms~n", [Size,Div,R,Ms]).
