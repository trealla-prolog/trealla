test1 :-
	writeln('Test simple sender/receiver'),
	thread_create(test_receiver,T1,[]),
	thread_create(test_sender(1000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	writeln(done),
	halt.

test_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	writeln(got(Msg)),
	test_receiver.

test_sender(0,To) :-
	sleep(1),
	writeln('Sending halt signal'),
	thread_signal(To,halt).
test_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test_sender(M,To).

