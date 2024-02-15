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
	thread_self(Self),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		Msg = msg(Alias, from(Self)),
		format(" ...Sending1 ~w ~w~n", [I, Msg]),
		thread_send_message(Alias, Msg),
		fail.
main :-
	threads(THREADS),
	between(1, THREADS, I),
		format("Getting1 ~w~n", [I]),
		thread_get_message(Msg),
		format("... got ~w ~w~n", [I,Msg]),
		fail.
main :-
	threads(THREADS),
	thread_self(Self),
	between(1, THREADS, I),
		atomic_concat(foo, I, Alias),
		Msg = msg(Alias, from(Self)),
		format(" ...Sending2 ~w ~w~n", [I, Msg]),
		thread_send_message(Alias, Msg),
		fail.
main :-
	threads(THREADS),
	between(1, THREADS, I),
		format("Getting2 ~w~n", [I]),
		thread_get_message(Msg),
		format("... got ~w ~w~n", [I,Msg]),
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
	between(1,2,_),
		thread_get_message(Msg),
		format(" ...Got ~w got ~w~n", [I, Msg]),
		Msg = msg(_, from(Chan)),
		thread_send_message(Chan, ok),
		fail.
thread_run(I) :-
	format("Terminating ~w~n", [I]).

