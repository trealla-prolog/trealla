:- initialization(main).

main :-
	mutex_create(_, [alias(bar)]),
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		format("Starting ~w~n", [Alias]),
		thread_create(thread_run(I), _, [alias(Alias)]),
		fail.
main :-
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		Msg = msg(Alias),
		format(" ...Sending ~w ~w~n", [Alias, Msg]),
		thread_send_message(Alias, Msg),
		fail.
main :-
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		format("Joining ~w~n", [Alias]),
		thread_join(Alias,_),
		fail.
main :-
	mutex_destroy(bar),
	writeln(done).

thread_run(I) :-
	format(" ...Thread ~w~n", [I]),
	mutex_lock(bar),
	mutex_unlock(bar),
	thread_get_message(Msg),
	format(" ...Got ~w got ~w~n", [I, Msg]),
	true.

