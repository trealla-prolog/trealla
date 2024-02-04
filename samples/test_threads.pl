:- initialization(main).

main :-
	mutex_create(_, [alias(bar)]),
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		mutex_lock(bar),
		format("Starting ~w~n", [Alias]),
		mutex_unlock(bar),
		thread_create(thread_run(I), _, [alias(Alias)]),
		fail.
main :-
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		Msg = ok,
		mutex_lock(bar),
		format(" ...Sending ~w msg ~w~n", [Alias, Msg]),
		mutex_unlock(bar),
		thread_send_message(Alias, Msg),
		fail.
main :-
	between(1, 10, I),
		atomic_concat(foo, I, Alias),
		mutex_lock(bar),
		format("Joining ~w~n", [Alias]),
		mutex_unlock(bar),
		thread_join(Alias,_),
		fail.
main :-
	mutex_destroy(bar),
	writeln(done).

thread_run(I) :-
	atomic_concat(foo, I, Alias),
	mutex_lock(bar),
	format(" ...Thread ~w~n", [Alias]),
	mutex_unlock(bar),
	thread_get_message(Alias, Msg),
	mutex_lock(bar),
	format(" ...Got ~w got ~w~n", [Alias, Msg]),
	mutex_unlock(bar),
	true.

