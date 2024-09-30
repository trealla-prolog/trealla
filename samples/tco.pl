test1(0) :- !, statistics, nl, fail.
test1(N) :- N1 is N-1, test1(N1).

f.

test2(0) :- !, statistics, nl, fail.
test2(N) :- f, N1 is N-1, test2(N1).

f(1).

test3(0) :- !, statistics, nl, fail.
test3(N) :- f(_), N1 is N-1, test3(N1).

f(_, _).

test4(0) :- !, statistics, fail.
test4(I) :- f(I, _), I2 is I-1, test4(I2).

main :-
	test1(100000);
	test2(200000);
	test3(300000);
	test4(400000);
	true.

