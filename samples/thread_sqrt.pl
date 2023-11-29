:- initialization(run).

run :-
	repeat,
		pl_recv(Tid, Term),
		Term = sqrt(X,Y),
		Y is sqrt(X),
		pl_send(Tid, Term),
		fail.

/*
?- pl_consult(Tid,'samples/thread_run.pl'),
	pl_send(Tid,sqrt(2,Y)),
	pl_recv(Tid, Response).
   Tid = 1, Response = sqrt(2,1.4142135623731).
?-
*/
