:- initialization(run).

run :-
	write('Running'), nl,
	repeat,
		pl_recv(T),
		format("Child: ~q~n", [T]), nl,
		pl_send(ok),
		fail.
