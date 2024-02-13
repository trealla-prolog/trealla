:- initialization(main).

threads(8).

main :-
	threads(THREADS),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		format("Starting ~w~n", [I]),
		thread_create(thread_run(I), _, [alias(Alias)]),
		fail.
main :-
	threads(THREADS),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		Msg = msg(Alias),
		format(" ...Sending ~w ~w~n", [I, Msg]),
		thread_send_message(Alias, Msg),
		fail.
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
	format(" ...Thread ~w~n", [I]),
	thread_get_message(Msg),
	(between(1,1000000,_), fail ; true),
	format(" ...Got ~w got ~w~n", [I, Msg]),
	true.

