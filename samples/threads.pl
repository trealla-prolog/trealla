test1 :-
	writeln('Test simple sender/receiver with signal to abort'),
	thread_create(test1_receiver,T1,[]),
	thread_create(test1_sender(100_000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	writeln(done(t1=S1,t2=S2)),
	halt.

test1_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)),write('    \r'),
	test1_receiver.

test1_sender(0,To) :-
	sleep(1),
	writeln('Sending halt signal'),
	thread_signal(To,halt).
test1_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test1_sender(M,To).

test2 :-
	writeln('Test simple sender/receiver with shutdown message'),
	thread_create(test2_receiver,T1,[]),
	thread_create(test2_sender(1_000_000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	writeln(done(t1=S1,t2=S2)),
	halt.

test2_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)),write('\r'),
	(Msg == shutdown -> (nl,halt) ; test2_receiver).

test2_sender(0,To) :-
	writeln('Sending halt signal'),
	thread_send_message(To,shutdown),
	halt.
test2_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test2_sender(M,To).

test3 :-
	writeln('Test simple ping-pong with shutdown message'),
	thread_create(test3_receiver,T1,[]),
	thread_create(test3_sender(1_000_000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	writeln(done(t1=S1,t2=S2)),
	halt.

test3_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)), write('\r'),
	(Msg == (nl,shutdown) -> halt ; true),
	Msg = msg(_I,From),
	thread_send_message(From,Msg),
	test3_receiver.

test3_sender(0,To) :-
	writeln('Sending halt signal'),
	thread_send_message(To,shutdown),
	halt.
test3_sender(N,To) :-
	thread_self(Me),
	thread_send_message(To,msg(N,Me)),
	thread_get_message(Me,msg(N,Me)),
	M is N-1,
	test3_sender(M,To).

test4 :-
	writeln('Test as above but with at_exit(Goal)'),
	thread_create(test4_receiver,T1,[at_exit(assertz(t(T1)))]),
	thread_create(test4_sender(100_000,T1),T2,[at_exit(assertz(t(T2)))]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	test4_exit_status,
	(writeln(done(T1=S1,T2=S2)),halt).

test4_exit_status :-
	retract(t(T)),
	writeln(at_exit(T)),
	fail.
test4_exit_status.

test4_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)), write('\r'),
	(Msg == (nl,shutdown) -> halt ; true),
	Msg = msg(_I,From),
	thread_send_message(From,Msg),
	test4_receiver.

test4_sender(0,To) :-
	writeln('Sending halt signal'),
	thread_send_message(To,shutdown),
	halt.
test4_sender(N,To) :-
	thread_self(Me),
	thread_send_message(To,msg(N,Me)),
	thread_get_message(Me,msg(N,Me)),
	M is N-1,
	test4_sender(M,To).

