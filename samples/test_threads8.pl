:- initialization(main).
:- dynamic f/2.

loops(1000).

main :-
	format("Starting~n", []),
	thread_create(thread_run(1), _, [alias(bar1)]),
	thread_create(thread_run(2), _, [alias(bar2)]),
	thread_create(thread_run(3), _, [alias(bar3)]),
	thread_create(thread_run(4), _, [alias(bar4)]),
	fail.
main :-
	loops(LOOPS),
	between(1, LOOPS, I),
		%format(" ...Sending ~w~n", [I]),
		assertz(foo(1,I)),
		assertz(foo(2,I)),
		assertz(foo(3,I)),
		assertz(foo(4,I)),
		fail.
main :-
	thread_join(bar1, _),
	thread_join(bar2, _),
	thread_join(bar3, _),
	thread_join(bar4, _),
	true.

thread_run(J) :-
	sleep(0.1),
	format(" ...Thread ~w~n", [J]),
	loops(LOOPS),
	between(1,LOOPS,I),
		retract(foo(J, I)),
		format(" ...Got ~w ~w~n", [I, foo(J)]),
		fail.
thread_run(J) :-
	format("Terminating ~w~n", [J]),
	true.
