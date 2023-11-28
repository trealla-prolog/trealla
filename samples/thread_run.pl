:- initialization(run).

run :-
	write('Running'), nl,
	repeat,
		pl_recv(T),
		format("Child: ~q~n", [T]), nl,
		pl_send(ok),
		fail.

/*
?- pl_consult(Tid, 'samples/thread_run.pl'),
	pl_send(Tid, msg('hello, world. This is a test.')),
	pl_recv(Response).
Running
Child: msg('hello, world. This is a test.')

   Tid = 1, Response = ok.
*/
