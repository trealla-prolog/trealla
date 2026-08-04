run :-
	run1, fail;
	run2, fail;
	run3, fail;
	run4, fail;
	run5, fail;
	run6, fail;
	listing(f/1),
	true.

writeln(P) :- write(P), nl.

run1 :-
	writeln('1. retractall...'),
	between(1,50,I),
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
	writeln('2. abolish...'),
	between(1,50,I),
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
	writeln('3. retract...'),
	between(1,50,I),
		write(I), nl,
		test3,
		fail.
run3.

test3 :-
	between(1,1000000,I),
		assertz(f(g(I))),
		fail.
test3 :-
	retract(f(_)),
	fail.
test3.

run4 :-
	writeln('4. retract...'),
	between(1,50,I),
		write(I), nl,
		test4,
		fail.
run4.

test4 :-
	between(1,1000000,I),
		assertz(f(g(I))),
		fail.
test4 :-
	do_retract(1000000),
	fail.
test4.

do_retract(0) :-
	!.
do_retract(I) :-
	retract(f(g(I))),
	I2 is I - 1,
	do_retract(I2).

run5 :-
	writeln('5. clause...'),
	between(1,50,I),
		write(I), nl,
		test5,
		retractall(f(_)),
		fail.
run5.

test5 :-
	between(1,1000000,I),
		assertz(f(g(I))),
		fail.
test5 :-
	clause(f(_),_),
	fail.
test5.

run6 :-
	writeln('6. match...'),
	between(1,50,I),
		write(I), nl,
		test6,
		retractall(f(_)),
		fail.
run6.

test6 :-
	between(1,1000000,I),
		assertz(f(g(I))),
		fail.
test6 :-
	f(_),
	fail.
test6.

