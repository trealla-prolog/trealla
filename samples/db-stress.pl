run :-
	run1,
	fail.
run :-
	run2,
	fail.
run :-
	run3,
	fail.
run.

run1 :-
	writeln('retractall...'),
	between(1,10,I),
		write(I), nl,
		test1,
		fail.
run1.

test1 :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test1 :-
	retractall(f(_)).

run2 :-
	writeln('abolish...'),
	between(1,10,I),
		write(I), nl,
		test2,
		fail.
run2.

test2 :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test2 :-
	abolish(f/1).

run3 :-
	writeln('retract...'),
	between(1,10,I),
		write(I), nl,
		test3,
		fail.
run3.

test3 :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test3 :-
	retract(f(_)),
	fail.
test3.
