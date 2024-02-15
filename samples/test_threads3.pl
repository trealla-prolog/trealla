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
	I = 1,
	atomic_concat(foo, I, Alias),
	thread_self(Self),
	Msg = msg(Alias, from(Self)),
	format(" ...Sending1 ~w ~w~n", [I, Msg]),
	thread_send_message(Alias, Msg),
	fail.
main :-
	format("... main waiting...~n", []),
	thread_get_message(Msg),
	format("... main got ~w~n", [Msg]),
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
	threads(THREADS),
	format(" ...Thread ~w~n", [I]),
	thread_get_message(Msg),
	format(" ...Got ~w got ~w~n", [I, Msg]),
	( I < THREADS ->
		(
			% pass it on to next thread...
			I2 is I + 1,
			atomic_concat(foo, I2, Alias),
			format(" ...Sending ~w ~w~n", [I, Msg]),
			thread_send_message(Alias, Msg)
		)
	;
		(
			% pass it back to main...
			Msg = msg(Alias, from(Thread)),
			Msg2 = ok,
			format(" ...Sending ~w ~w ~w~n", [I, Thread, Msg2]),
			thread_send_message(Thread, Msg2)
		)
	),
	fail.
thread_run(I) :-
	format("Terminating ~w~n", [I]).

