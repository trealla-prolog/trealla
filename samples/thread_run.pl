:- initialization(run).

run :-
	write('Running'), nl,
	repeat,
		pl_recv(Tid, Term),
		format("Child: ~q~n", [Term]), nl,
		pl_send(Tid, ok),
		fail.

/*
?- pl_consult(Tid, 'samples/thread_run.pl'),
	pl_send(Tid, msg('hello, world. This is a test.')),
	pl_recv(Tid, Response).
Running
Child: msg('hello, world. This is a test.')

   Tid = 1, Response = ok.
*/
