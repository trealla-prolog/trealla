:- initialization(main).

main :-
	message_queue_create(_, [alias(foo)]),
	thread_send_message(foo, f(1)),
	thread_send_message(foo, g(2)),
	thread_peek_message(foo, g(_X1)),
	thread_peek_message(foo, f(_X2)),
	thread_get_message(foo, g(_Y1)),
	thread_get_message(foo, f(_Y2)),
	writeln(done).
