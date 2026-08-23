% Who owns a task, and when does it get to run.
%
% Every property here is a *current* behaviour, established by probing
% the engine rather than by reading the docs. Some are arguably wrong -
% a task spawned by a task and then never waited for is dropped without
% a word - and they are pinned down precisely because they are what a
% move to a single global scheduler would change. If one of these
% outputs changes, that has to be a decision someone made, not a side
% effect they did not notice.
%
% The rules, as they stand:
%
%   - a task is a subquery, and it belongs to whoever spawned it. Tasks
%     therefore nest: a task that spawns a task owns it.
%   - the scheduler itself is shared, one per prolog instance. Ownership
%     decides when a wait/0 may return, not what is allowed to run.
%   - wait/0 returns when the caller's whole subtree is done, at any
%     depth - so a task spawned anywhere below will run, whether or not
%     the task that spawned it waited for it.
%
% That last rule changed in GUSTTO phase 0. Before the scheduler was
% hoisted out of the query, each spawner had a scheduler of its own and
% a task that spawned without waiting had its children thrown away when
% it finished. Two tests below are marked with what they used to say.
%
% Observation is through a dynamic predicate rather than send/1, for two
% reasons: tasks share the database with their parent, so it works at
% any depth where send/1 only reaches one level; and it does not tie
% these properties to a messaging layer that is due to be replaced.
%
% Nothing here depends on timing or on the order two independent tasks
% happen to interleave.

:- initialization(main).

:- dynamic(ran/1).

note(X) :- assertz(ran(X)).

ran_list(L) :- findall(X, ran(X), L).

reset :- retractall(ran(_)).

report(Name, Got, Expect) :-
	(	Got == Expect
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q wanted ~q~n", [Name,Got,Expect])
	).

% A task may spawn a task and not wait for it. The child still runs,
% because the wait/0 at the top covers the whole subtree.
%
% Before phase 0 this reported [neglectful] alone: the child went into a
% scheduler belonging to `neglectful` that nothing ever drained, and was
% discarded when that task finished.

orphan :- note(orphan).
neglectful :- call_task(orphan), note(neglectful).

spawner_need_not_wait :-
	reset,
	call_task(neglectful),
	wait,
	ran_list(L),
	report(spawner_need_not_wait, L, [neglectful,orphan]).

% The same task, but waiting: now the child runs, and it runs before
% the owner continues past its wait/0.

attentive :- call_task(orphan), wait, note(attentive).

owner_waits_child_runs :-
	reset,
	call_task(attentive),
	wait,
	ran_list(L),
	report(owner_waits_child_runs, L, [orphan,attentive]).

% Nesting is not limited to one level, and each level needs its own
% wait/0. The top only sees the deepest task because every owner in the
% chain waits for the one below it.

deep3 :- note(deep3).
deep2 :- call_task(deep3), wait, note(deep2).
deep1 :- call_task(deep2), wait, note(deep1).

nesting_is_recursive :-
	reset,
	call_task(deep1),
	wait,
	ran_list(L),
	report(nesting_is_recursive, L, [deep3,deep2,deep1]).

% A gap in the chain of wait/0 calls does not sever what is below it.
% sever2 spawns deep3 without waiting, but sever1's wait/0 covers its
% whole subtree, so deep3 runs - and runs before sever1 resumes.
%
% Before phase 0 this reported [sever2,sever1]: deep3 was discarded
% along with the scheduler sever2 owned.

sever2 :- call_task(deep3), note(sever2).
sever1 :- call_task(sever2), wait, note(sever1).

gap_in_chain_does_not_sever :-
	reset,
	call_task(sever1),
	wait,
	ran_list(L),
	report(gap_in_chain_does_not_sever, L, [sever2,deep3,sever1]).

% Tasks run in the order they were spawned, and to completion: wait/0
% drains rather than interleaving at every opportunity.

seq(N) :- note(a(N)), note(b(N)).

spawn_order_is_fifo :-
	reset,
	call_task(seq, 1),
	call_task(seq, 2),
	wait,
	ran_list(L),
	report(spawn_order_is_fifo, L, [a(1),b(1),a(2),b(2)]).

% A task calling end_wait/0 releases its owner from wait/0 with the
% other tasks still queued; they run on the next wait/0. The flag must
% not survive into that second wait/0.

stopper :- note(stopper), end_wait.
after(N) :- note(after(N)).

end_wait_releases_once :-
	reset,
	call_task(stopper),
	call_task(after, 1),
	wait,
	ran_list(L1),
	wait,
	ran_list(L2),
	report(end_wait_releases_first_wait, L1, [stopper]),
	report(end_wait_releases_second_wait, L2, [stopper,after(1)]).

% end_wait/0 with nothing waiting is not an error.

end_wait_without_wait :-
	(	catch(end_wait, E, true)
	->	( var(E) -> R = ok ; R = threw(E) )
	;	R = failed
	),
	report(end_wait_without_wait, R, ok).

% wait/0 with no tasks at all succeeds and does nothing.

wait_with_no_tasks :-
	reset,
	(	catch(wait, E, true)
	->	( var(E) -> R = ok ; R = threw(E) )
	;	R = failed
	),
	report(wait_with_no_tasks, R, ok).

main :-
	spawner_need_not_wait,
	owner_waits_child_runs,
	nesting_is_recursive,
	gap_in_chain_does_not_sever,
	spawn_order_is_fifo,
	end_wait_releases_once,
	end_wait_without_wait,
	wait_with_no_tasks.
