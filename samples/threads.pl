test1 :-
	write('Test simple sender/receiver with signal to abort'), nl,
	thread_create(test1_receiver,T1,[]),
	thread_create(test1_sender(100000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	write(done(t1=S1,t2=S2)), nl,
	halt.

test1_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)), nl,
	test1_receiver.

test1_sender(0,To) :-
	sleep(1),
	write('Sending halt signal'), nl,
	thread_signal(To,halt).
test1_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test1_sender(M,To).

test2 :-
	write('Test simple sender/receiver with shutdown message'),  nl,
	thread_create(test2_receiver,T1,[]),
	thread_create(test2_sender(100000,T1),T2,[]),
	thread_join(T2,S1),
	thread_join(T1,S2),
	write(done(t1=S1,t2=S2)), nl,
	halt.

test2_receiver :-
	thread_self(Me),
	thread_get_message(Me,Msg),
	write(got(Msg)), nl,
	(Msg == shutdown -> halt ; test2_receiver).

test2_sender(0,To) :-
	write('Sending halt signal'), nl,
	thread_send_message(To,shutdown),
	halt.
test2_sender(N,To) :-
	thread_send_message(To,N),
	M is N-1,
	test2_sender(M,To).

