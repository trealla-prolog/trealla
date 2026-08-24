% The lock in leave_predicate() has to span the refcount decrement, not
% just the purge that follows it.
%
% leave_predicate() drops a reader's handle on a dynamic predicate, and
% when it takes the count to zero it reclaims whatever retract left on
% pr->dirty. The obvious optimisation is to do the decrement and the
% two cheap checks unlocked - the count is atomic and only one thread
% can take it to zero - and to lock only for the reclamation, which is
% rare. leave_predicate() is hot enough that this looked worth a few
% percent.
%
% It is wrong. Holding the lock from before the decrement is what makes
% "took the count to zero" and "reclaimed" one indivisible step against
% everything else contending for that lock; shrinking it to the purge
% lets another thread into the gap, and a thread then executes a clause
% whose memory has been reclaimed under it. It shows up as a BUS or
% SEGV at q->st.instr in the main loop, which is a long way from the
% cause.
%
% What this test needs in order to bite: a churner that builds a large
% dirty list and then drops the last handle, so the reclamation is long,
% and several threads entering and leaving the same predicate throughout
% so one of them lands in the window. Measured against a deliberately
% broken build it fails 20 times in 20; against a correct one it passes
% 40 in 40, in about 0.7s.
%
% Note this is NOT the test for the original database concurrency crash
% - see db_concurrency.pl for that one, which catches its bug 14 times
% in 15 and this one 0 times in 15. They cover different failures and
% neither substitutes for the other.

:- initialization(main).

:- dynamic(p/2).

% Build a big dirty list, then drop the last handle so the purge is long.

churner :-
	forall(between(1,200,_),
		(	forall(between(1,150,I), assertz(p(k,I))),
			forall(between(1,150,_), ( retract(p(k,_)) -> true ; true )),
			( p(k,_) -> true ; true )
		)).

% Enter and leave the same predicate as fast as possible, to land in the
% window while the churner is reclaiming.

hammer :-
	forall(between(1,200000,_), ( p(k,_) -> true ; true )).

main :-
	thread_create(churner, T1, []),
	findall(T, (between(1,4,_), thread_create(hammer, T, [])), Hs),
	forall(member(T, [T1|Hs]), thread_join(T, _)),
	format("db_purge_window: ok~n").
