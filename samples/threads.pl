% All tests work with Trealla & SWI Prolog.

test :-
	test1,fail;
	test2,fail;
	test3,fail;
	test4,fail;
	test5,fail;
	test6,fail;
	test7,fail;
	test8,fail;
	true.

test1 :-
	writeln('\nTest1 simple sender/receiver (x100K) with signal to exit'),
	thread_create(test1_receiver,T1,[]),
	thread_create(test1_sender(100_000,T1),T2,[]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	writeln(done(T1=S1,T2=S2)).

test1_receiver :-
	thread_get_message(Msg),
	write(got(Msg)),write('      \r'),
	test1_receiver.

test1_sender(0,To) :-
	sleep(1),
	writeln('Sending exit signal'),
	thread_signal(To,thread_exit(exit)).
test1_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test1_sender(M,To).

test2 :-
	writeln('\nTest2 simple sender/receiver (x100K) with shutdown message'),
	thread_create(test2_receiver,T1,[]),
	thread_create(test2_sender(100_000,T1),T2,[]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	writeln(done(T1=S1,T2=S2)).

test2_receiver :-
	thread_get_message(Msg),
	write(got(Msg)),write('          \r'),
	(Msg == shutdown -> (nl,thread_exit(exit)) ; test2_receiver).

test2_sender(0,To) :-
	writeln('\rSending shutdown message'),
	thread_send_message(To,shutdown).
test2_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test2_sender(M,To).

test3 :-
	writeln('\nTest3 simple ping-pong (x100K) with shutdown message'),
	thread_create(test3_receiver,T1,[]),
	thread_create(test3_sender(100_000,T1),T2,[]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	writeln(done(T1=S1,T2=S2)).

test3_receiver :-
	thread_get_message(Msg),
	write(got(Msg)), write('      \r'),
	(Msg == shutdown -> thread_exit(exit) ; true),
	Msg = msg(_,From),
	thread_send_message(From,Msg),
	test3_receiver.

test3_sender(0,To) :-
	writeln('\rSending shutdown message'),
	thread_send_message(To,shutdown).
test3_sender(N,To) :-
	thread_self(Me),
	thread_send_message(To,msg(N,Me)),
	thread_get_message(msg(N,Me)),
	M is N-1,
	test3_sender(M,To).

test4 :-
	writeln('\nTest4 as above (x100K) but with at_exit(Goal)'),
	thread_create(test4_receiver,T1,[at_exit(asserta(t(T1)))]),
	thread_create(test4_sender(100_000,T1),T2,[at_exit(assertz(t(T2)))]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	test4_exit_status,
	writeln(done(T1=S1,T2=S2)).

test4_exit_status :-
	retract(t(T)),
	writeln(at_exit(T)),
	fail.
test4_exit_status.

test4_receiver :-
	thread_get_message(Msg),
	write(got(Msg)), write('      \r'),
	(Msg == shutdown -> thread_exit(exit) ; true),
	Msg = msg(_I,From),
	thread_send_message(From,Msg),
	test4_receiver.

test4_sender(0,To) :-
	writeln('\rSending shutdown message'),
	thread_send_message(To,shutdown).
test4_sender(N,To) :-
	thread_self(Me),
	thread_send_message(To,msg(N,Me)),
	thread_get_message(msg(N,Me)),
	M is N-1,
	test4_sender(M,To).

test5 :-
	writeln('\nTest5 aliased ping-pong (x1M) with shutdown message'),
	thread_create(test5_receiver,T1,[alias(consumer)]),
	thread_create(test5_sender(1_000_000),T2,[alias(producer)]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	writeln(done(T1=S1,T2=S2)).

test5_receiver :-
	thread_get_message(consumer,Msg),
	write(got(Msg)), write('      \r'),
	(Msg == shutdown -> thread_exit(exit) ; true),
	thread_send_message(producer,Msg),
	test5_receiver.

test5_sender(0) :-
	writeln('\rSending shutdown message'),
	thread_send_message(consumer,shutdown).
test5_sender(N) :-
	thread_send_message(consumer,msg(N)),
	thread_get_message(producer,msg(N)),
	M is N-1,
	test5_sender(M).

test6 :-
	writeln('\nTest6 aliased ping-pong (x1M) with 1s timeout'),
	thread_create(test6_receiver,T1,[alias(consumer)]),
	thread_create(test6_sender(1_000_000),T2,[alias(producer)]),
	thread_join(T2,S2),
	thread_join(T1,S1),
	writeln(done(T1=S1,T2=S2)).

test6_receiver :-
	thread_get_message(consumer,Msg,[timeout(1)]),
	write(got(Msg)), write('      \r'),
	thread_send_message(producer,Msg),
	test6_receiver.
test6_receiver :-
	writeln('\rTimeout!      ').

test6_sender(0).
test6_sender(N) :-
	thread_send_message(consumer,msg(N)),
	thread_get_message(producer,msg(N)),
	M is N-1,
	test6_sender(M).

test7 :-
	writeln('\nTest7 multiple aliased ping-pong (x100K) with shutdown message'),
	thread_create(test7_receiver,T1a,[alias(consumer1)]),
	thread_create(test7_receiver,T1b,[alias(consumer2)]),
	thread_create(test7_sender(100_000),T2,[alias(producer)]),
	thread_join(T2,S2),
	thread_join(T1a,S1a),
	thread_join(T1b,S1b),
	writeln(done(T1a=S1a,T1b=S1b,T2=S2)).

test7_receiver :-
	thread_get_message(Msg),
	write(got(Msg)), write('      \r'),
	(Msg == shutdown -> thread_exit(exit) ; true),
	thread_send_message(producer,Msg),
	test7_receiver.

test7_sender(0) :-
	writeln('\rSending shutdown message'),
	thread_send_message(consumer1,shutdown),
	thread_send_message(consumer2,shutdown).
test7_sender(N) :-
	thread_send_message(consumer1,msg(N)),
	thread_send_message(consumer2,msg(N)),
	thread_get_message(producer,msg(N)),
	thread_get_message(producer,msg(N)),
	M is N-1,
	test7_sender(M).

test8 :-
	writeln('\nTest8 sender / multiple receiver (x100K) with shutdown message'),
	thread_create(test8_receiver,T1a,[]),
	thread_create(test8_receiver,T1b,[]),
	thread_create(test8_sender(100_000,T1a,T1b),T2,[]),
	thread_join(T2,S2),
	thread_join(T1a,S1a),
	thread_join(T1b,S1b),
	writeln(done(T1a=S1a,T1b=S1b,T2=S2)).

test8_receiver :-
	thread_get_message(Msg),
	write(got(Msg)),write('     \r'),
	(Msg == shutdown -> (nl,thread_exit(exit)) ; test8_receiver).

test8_sender(0,To1,To2) :-
	writeln('\rSending shutdown message'),
	thread_send_message(To1,shutdown),
	thread_send_message(To2,shutdown).
test8_sender(N,To1,To2) :-
	thread_send_message(To1,N),
	thread_send_message(To2,N),
	M is N-1,
	test8_sender(M,To1,To2).

