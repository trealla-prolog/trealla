:- initialization(main2).

main1 :-
	thread_create(catch(call_with_time_limit(1.0, run1), _, true), T1, []),
	thread_join(T1),
	writeln('done1').

run1 :-
	get_char(_).

main2 :-
	thread_create(catch(call_with_time_limit(1.0, run2(here1)), _, true), T1, []),
	thread_create(catch(call_with_time_limit(2.0, run2(here2)), _, true), T2, []),
	thread_join(T1),
	writeln('\tdone1'),
	thread_join(T2),
	writeln('\tdone2').

run2(Msg) :-
	repeat, writeln(Msg), sleep(0.25), fail.

