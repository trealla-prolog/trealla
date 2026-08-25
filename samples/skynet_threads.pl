% Skynet - https://github.com/atemerev/skynet
%
%     tpl samples/skynet_threads.pl -g "run(1000000,10),halt"
%
% Each actor spawns Div children covering a slice of the range, sums what
% they report, and reports the total to its parent; a leaf reports its own
% ordinal. Size must be a power of Div, or the integer division reaches 0
% before the base case and the tree never terminates.
%
% At the OS thread ceiling this used to crash intermittently under
% concurrent load - reliably, 50-80% of runs at 16384 leaves. Four
% distinct races in src/bif_threads.c were found and fixed:
%
%   - retire_thread() puts a thread struct back on the free list for
%     immediate reuse by any other thread's new_thread(); several
%     callers (bif_thread_create_3's pthread_create failure path,
%     start_routine_thread_create, bif_thread_join_2, do_cancel,
%     bif_message_queue_destroy_1) kept writing into the struct, or
%     kept t->guard held, AFTER retiring it.
%   - find_thread_by_id() walked the id->thread skiplist with no lock,
%     while new_thread() deletes/inserts nodes in it under prolog_lock -
%     a concurrent sl_del() could free a node this was mid-traversal
%     through.
%   - do_send_message()/queue_to_chan() each resolved the same id
%     independently, and queue_to_chan() never checked the result for
%     NULL; between the two lookups the id's struct could already have
%     been retired and reused by someone else.
%
% Those cut the crash rate to roughly 3-4% at this same boundary (3/80
% in the last measured batch). At LEAST one more race remains,
% signature consistent with the same class (a thread-struct pointer
% going stale between an id lookup and its use) but not yet pinned to
% an exact call site - ASan's own report kept getting cut short by a
% second concurrent fault, and a debugger-based capture was inconclusive
% (one frame pointed at index_check(), which is gated behind
% g_index_check defaulting to 0 and should not be reachable - likely a
% mis-symbolized frame, not the real site). Given the ~30 other
% find_thread_by_id() call sites in bif_threads.c follow the same
% "look up by id, trust the pointer" pattern with only partial
% NULL-checking, the systemic fix is probably a proper audit of all of
% them (or a generation-counter / refcount scheme on thread structs)
% rather than one more one-off patch. Not yet in tests/misc.
%
% Every actor reports to its parent exactly once, either result/1 or
% failed/1. That matters more than it looks: if an actor could die
% without reporting - because its own spawn hit a thread limit, say - its
% parent would block in sum_children forever, and the whole tree would
% deadlock rather than fail. Linking the children would also solve it,
% but costs two assertz per spawn on a shared dynamic predicate, and
% those serialise on the database lock: measured 5.7x slower.

:- use_module(library(actors/threads)).

skynet(Parent, Num, 1, _) :-
	!,
	actor_send(Parent, result(Num)).

skynet(Parent, Num, Size, Div) :-
	catch(spawn_and_sum(Num, Size, Div, Tot), E,
		( actor_send(Parent, failed(E)), throw(E) )),
	actor_send(Parent, result(Tot)).

spawn_and_sum(Num, Size, Div, Tot) :-
	( getenv('RETRIES',RA) -> atom_number(RA,R) ; R = 10 ),
	NewSize is Size div Div,
	actor_self(Me),
	forall(between(1, Div, Idx),
		(	NewNum is ((Idx - 1) * NewSize) + Num,
			actor_spawn(skynet(Me, NewNum, NewSize, Div), _, [retries(R)])
		)),
	sum_children(Div, 0, Tot).

sum_children(0, Tot, Tot) :- !.
sum_children(N, Acc, Tot) :-
	actor_recv(Msg),
	(	Msg = result(X)
	->	Acc1 is Acc + X, N1 is N - 1, sum_children(N1, Acc1, Tot)
	;	Msg = failed(E)
	->	throw(E)
	;	sum_children(N, Acc, Tot)
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
	actor_self(Me),
	get_time(T0),
	actor_spawn(skynet(Me, 0, Size, Div), _),
	actor_recv(Msg),
	get_time(T1),
	Ms is round((T1-T0)*1000),
	Expect is (Size * (Size - 1)) // 2,
	(	Msg = result(Tot)
	->	( Tot =:= Expect -> R = ok ; R = wrong(Tot,Expect) )
	;	Msg = failed(E) -> R = failed(E)
	;	R = unexpected(Msg)
	),
	format("size=~w div=~w -> ~w in ~wms~n", [Size,Div,R,Ms]).
