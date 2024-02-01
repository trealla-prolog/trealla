:- initialization(main).

% At the moment we only do sqrt

main :-
	write('Calculator running...'), nl,
	repeat,
		pl_msg_recv(Tid, Term),
		Term = sqrt(X,Y),
		Y is sqrt(X),
		pl_msg_send(Tid, Term),
		fail.

/*
?- pl_thread(Tid,'samples/thread_calc.pl'),
	Term = sqrt(2,Y),
	pl_msg_send(Tid, Term),
	pl_msg_recv(Tid, Term).
Calculator running...
   Tid = 1, Term = sqrt(2,1.4142135623731), V = 1.4142135623731.
?-
*/
