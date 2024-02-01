:- initialization(main).

% At the moment we only do sqrt

main :-
	write('Calculator running...'), nl,
	repeat,
		thread_get_message(Tid, Term),
		Term = sqrt(X,Y),
		Y is sqrt(X),
		thread_send_message(Tid, Term),
		fail.

/*
?- pl_thread(Tid,'samples/thread_calc.pl'),
	Term = sqrt(2,Y),
	thread_send_message(Tid, Term),
	thread_get_message(Tid, Term).
Calculator running...
   Tid = 1, Term = sqrt(2,1.4142135623731), V = 1.4142135623731.
?-
*/
