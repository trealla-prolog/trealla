:- initialization((main2,main5,main6,main7)).

main1 :-
	writeln('main1...'),
	thread_create(catch(call_with_time_limit(0.1, run1), _, writeln(catch1)), T1, []),
	thread_join(T1),
	writeln('done1').

run1 :-
	get_char(_).

main2 :-
	writeln('main2...'),
	thread_create(catch(call_with_time_limit(1.0, run2(here1)), _, writeln(catch1)), T1, []),
	thread_create(catch(call_with_time_limit(2.0, run2(here2)), _, writeln(catch2)), T2, []),
	thread_join(T1),
	writeln('\tdone1'),
	thread_join(T2),
	writeln('\tdone2').

run2(Msg) :-
	repeat, sleep(0.25), fail.

run5(Secs,Msg) :-
	catch(
		call_with_time_limit(Secs, sleep(2.0)),
		_,
		writeln(Msg)
	).

main5 :-
	writeln('main5...'),
	thread_create(run5(0.5, alarm1),T1,[]),
	thread_create(run5(1.0, alarm2),T2,[]),
	thread_join(T1),
	thread_join(T2).

run6(Secs,Msg) :-
	catch(
		call_with_time_limit(Secs, (repeat,fail)),
		_,
		writeln(Msg)
	).

main6 :-
	writeln('main6...'),
	thread_create(run6(0.5, alarm1),T1,[]),
	thread_create(run6(1.0, alarm2),T2,[]),
	thread_join(T1),
	thread_join(T2).

run7(Secs,Msg) :-
	catch(
		call_with_time_limit(Secs, (repeat,fail)),
		_,
		writeln(Msg)
	).

main7 :-
	writeln('main7...'	),
	thread_create((run7(0.1, alarm1), run7(0.1, alarm2)),T,[]),
	thread_join(T).
