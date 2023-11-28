:- initialization(run).

run :-
	write('Square-root calculator running...'), nl,
	repeat,
		pl_recv(Tid, Term),
		format("Child: ~q~n", [Term]), nl,
		Term = sqrt(X,Y),
		Y is sqrt(X),
		pl_send(Tid, Term),
		fail.

/*
?- pl_consult(Tid,'samples/thread_run.pl'),
	pl_send(Tid,sqrt(2,Y)),
	pl_recv(Tid, Response).
Square-root calculator running...
Child: sqrt(2,_7)

   Tid = 1, Response = sqrt(2,1.4142135623731).
?-
*/
