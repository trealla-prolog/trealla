:- initialization(main).
:- dynamic(msg/2).

loops(1000000).

main :-
	message_queue_create(_, [alias(foo)]),
	%format("Starting~n", []),
	thread_create(thread_run, _, [alias(bar)]),
	thread_create(thread_run, _, [alias(baz)]),
	fail.
main :-
	loops(LOOPS),
	between(1, LOOPS, I),
		%format(" ...Sending ~w~n", [I]),
		thread_send_message(bar, ok),
		thread_send_message(baz, ok),
		fail.
main :-
	loops(LOOPS),
	between(1, LOOPS, I),
		%format("Getting1 ~w~n", [I]),
		thread_get_message(foo, Msg),
		%format("... got ~w ~w~n", [I,Msg]),
		fail.
main :-
	loops(LOOPS),
	between(1, LOOPS, I),
		%format("Getting2 ~w~n", [I]),
		thread_get_message(foo, Msg),
		%format("... got ~w ~w~n", [I,Msg]),
		fail.
main :-
	%format("Joining~n", []),
	thread_join(bar,_),
	thread_join(baz,_),
	fail.
main :-
	message_queue_destroy(foo),
	writeln(done).

thread_run :-
	%format(" ...Thread~n", []),
	loops(LOOPS),
	between(1,LOOPS,I),
		thread_get_message(Msg),
		%format(" ...Got ~w ~w~n", [I, Msg]),
		thread_send_message(foo, done),
		fail.
thread_run :-
	%format("Terminating~n", []),
	true.
