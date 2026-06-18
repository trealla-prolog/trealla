:- initialization(main).

main :-
	thread_create(catch(call_with_time_limit(1.0, run(here1)), _, true), T1, []),
	thread_create(catch(call_with_time_limit(2.0, run(here2)), _, true), T2, []),
	thread_join(T1),
	writeln('\tdone1'),
	thread_join(T2),
	writeln('\tdone2').

run(Msg) :-
	repeat, writeln(Msg), sleep(0.25), fail.

