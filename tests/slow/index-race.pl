:- dynamic(p/2).
:- initialization(main).

% Exercises the wildcard path through a predicate's clause index from
% several threads at once: a partially instantiated compound first
% argument, over a predicate well past the 500-clause index threshold.
% sl_find_key()/sl_next_key() carry per-traversal state, and a lookup
% that loses it drops a clause it should have matched. Run under a
% thread sanitizer to see the races themselves; this only counts short
% reads, and does not reliably provoke one on its own.

seed(N) :-
	between(1, N, I),
	assertz(p(k(I,a), I)),
	assertz(p(k(I,b), I)),
	fail.
seed(_).

scan(I, Hi, Acc, Acc) :- I > Hi, !.
scan(I, Hi, Acc, Short) :-
	findall(V, p(k(I,_), V), Vs),
	length(Vs, N),
	( N =:= 2 -> Acc1 = Acc ; Acc1 is Acc + 1 ),
	I1 is I + 1,
	scan(I1, Hi, Acc1, Short).

passes(0, _, Acc, Acc) :- !.
passes(P, Hi, Acc, Short) :-
	scan(1, Hi, Acc, Acc1),
	P1 is P - 1,
	passes(P1, Hi, Acc1, Short).

worker(Queue, Passes, Hi) :-
	passes(Passes, Hi, 0, Short),
	thread_send_message(Queue, done(Short)).

collect(0, _, Acc, Acc) :- !.
collect(N, Queue, Acc, Short) :-
	thread_get_message(Queue, done(S)),
	Acc1 is Acc + S,
	N1 is N - 1,
	collect(N1, Queue, Acc1, Short).

main :-
	Keys = 600, Threads = 4, Passes = 1800,
	seed(Keys),
	message_queue_create(Queue, []),
	forall(between(1, Threads, _), thread_create(worker(Queue, Passes, Keys), _, [])),
	collect(Threads, Queue, 0, Short),
	format("short reads: ~w~n", [Short]).
