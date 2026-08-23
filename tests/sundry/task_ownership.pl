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
%   - a task is a subquery, and its scheduler belongs to whoever spawned
%     it. Tasks therefore nest: a task that spawns a task owns it.
%   - a child only runs if its owner calls wait/0. A task that spawns
%     and does not wait has its children discarded when it finishes.
%   - wait/0 drains only what the caller owns, so a nested task is
%     invisible to the top until its owner waits for it.
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

% A task that spawns a task and never waits: the child is discarded.
% The parent's own work still happens, so this is not the whole task
% failing - only the orphan is silently lost.

orphan :- note(orphan).
neglectful :- call_task(orphan), note(neglectful).

orphan_dropped :-
	reset,
	call_task(neglectful),
	wait,
	ran_list(L),
	report(orphan_dropped, L, [neglectful]).

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

% One missing wait/0 anywhere in the chain severs everything below it,
% not just the next level down.

sever2 :- call_task(deep3), note(sever2).
sever1 :- call_task(sever2), wait, note(sever1).

missing_wait_severs_subtree :-
	reset,
	call_task(sever1),
	wait,
	ran_list(L),
	report(missing_wait_severs_subtree, L, [sever2,sever1]).

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
	orphan_dropped,
	owner_waits_child_runs,
	nesting_is_recursive,
	missing_wait_severs_subtree,
	spawn_order_is_fifo,
	end_wait_releases_once,
	end_wait_without_wait,
	wait_with_no_tasks.
