:- initialization(main).

loops(1000).

main :-
	message_queue_create(_, [alias(foo)]),
	format("Starting~n", []),
	thread_create(thread_run(1), _, [alias(bar)]),
	thread_create(thread_run(2), _, [alias(baz)]),
	delay(1),
	fail.
main :-
	loops(LOOPS),
	between(1, LOOPS, I),
		%format(" ...Sending ~w~n", [I]),
		thread_send_message(foo, foo(1)),
		thread_send_message(foo, foo(2)),
		fail.
main :-
	thread_join(bar, _),
	thread_join(baz, _),
	true.

thread_run(J) :-
	format(" ...Thread ~w~n", [J]),
	loops(LOOPS),
	between(1,LOOPS,I),
		thread_get_message(foo, foo(J)),
		format(" ...Got ~w ~w~n", [I, foo(J)]),
		fail.
thread_run(J) :-
	format("Terminating ~w~n", [J]),
	true.
