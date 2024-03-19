:- initialization(main).

loops(1000).

main :-
	message_queue_create(_, [alias(foo)]),
	format("Starting~n", []),
	thread_create(thread_run(1), _, [alias(bar1)]),
	thread_create(thread_run(2), _, [alias(bar2)]),
	thread_create(thread_run(3), _, [alias(bar3)]),
	thread_create(thread_run(4), _, [alias(bar4)]),
	fail.
main :-
	sleep(0.1),
	loops(LOOPS),
	between(1, LOOPS, I),
		%format(" ...Sending ~w~n", [I]),
		thread_send_message(foo, foo(1)),
		thread_send_message(foo, foo(2)),
		thread_send_message(foo, foo(3)),
		thread_send_message(foo, foo(4)),
		fail.
main :-
	thread_join(bar1, _),
	thread_join(bar2, _),
	thread_join(bar3, _),
	thread_join(bar4, _),
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
