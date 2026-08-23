% Concurrent database access across real threads.
%
% thread_create/3 shares the database, so two threads asserting and
% retracting the same dynamic predicate while a third walks it is an
% ordinary thing to write - and it segfaulted, reliably. Six runs in ten
% for the simple case, and it predated GUSTTO entirely.
%
% Three separate paths mutated a predicate's index skiplists, and no two
% of them excluded each other:
%
%   - assert/retract, under prolog_lock()
%   - the dirty-list purge in leave_predicate(), under module_lock() -
%     a different lock, so no mutual exclusion at all
%   - query_purge_dirty_list() at query teardown, which for a thread
%     means at join, under no lock
%
% There was also a check-then-act window that no amount of locking one
% side would close: leave_predicate() decremented the reader refcount,
% saw zero, and concluded it was safe to free - but a reader could take
% a fresh handle in that gap and start descending an index the purge was
% already tearing down. The refcount is atomic; the *conclusion drawn
% from it* was not, so enter and leave have to serialise against each
% other rather than merely count.
%
% Deliberately modest counts: enough to have caught the original in
% testing, not enough to dominate the suite. The assertion is only that
% it completes - what is being tested is that it does not crash.

:- initialization(main).

:- dynamic(shared/2).

writer(Tag) :-
	forall(between(1,800,I),
		(	assertz(shared(Tag,I)),
			(	0 is I mod 2
			->	( retract(shared(Tag,_)) -> true ; true )
			;	true
			),
			(	0 is I mod 7
			->	( retract(shared(_,_)) -> true ; true )
			;	true
			)
		)).

reader :-
	forall(between(1,800,_),
		( findall(X-Y, shared(X,Y), L), length(L,_) )).

main :-
	findall(T,
		(	member(G, [writer(a),writer(b),writer(c),reader,reader,writer(d)]),
			thread_create(G, T, [])
		), Ts),
	forall(member(T,Ts), thread_join(T,_)),
	format("db_concurrency: ok~n").
