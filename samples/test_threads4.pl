:- initialization(main).

threads(8).

main :-
	threads(THREADS),
	mutex_lock(foo),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		format("Starting ~w~n", [I]),
		assertz(f(I)),
		thread_create(thread_run(I), _, [alias(Alias)]),
		fail.
main :-
	sleep(1),
	writeln('Go...'),
	mutex_unlock(foo),
	sleep(2),
	retract(f(I)),		% should fail
	writeln(oopsf(I)),
	halt.
main :-
	sleep(1),
	retract(g(I)),		% should fail
	writeln(oopsg(I)),
	halt.
main :-
	threads(THREADS),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		format("Joining ~w~n", [I]),
		thread_join(Alias,_),
		fail.
main :-
	writeln(done).

thread_run(I) :-
	mutex_lock(foo),
	mutex_unlock(foo),
	retract(f(I)),
	format("Got f(~w)~n", [I]),
	assertz(g(I)),
	retract(g(I)),
	format("Got g(~w)~n", [I]),
	format("Terminating ~w~n", [I]).
thread_run(I) :-
	format("Thread oops ~w~n", [I]).

