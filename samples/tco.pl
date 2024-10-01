test1(0) :- !, statistics, fail.
test1(N) :- N1 is N-1, test1(N1).

f.

test2(0) :- !, statistics, fail.
test2(N) :- f, N1 is N-1, test2(N1).

f(1).

test3(0) :- !, statistics, fail.
test3(N) :- f(_), N1 is N-1, test3(N1).

f(_, _).

test4(0) :- !, statistics, fail.
test4(I) :- f(I, _), I2 is I-1, test4(I2).

f(_, g(_), g(g(_))).

test5(0) :- !, statistics, fail.
test5(I) :- f(I, g(I), g(g(_))), I2 is I-1, test5(I2).

statistics.

main :-
	test1(1000000);
	test2(1000000);
	test3(1000000);
	test4(1000000);
	test5(1000000);
	true.

