test1(0) :- !, statistics, nl, fail.
test1(N) :- N1 is N-1, test1(N1).

f.

test2(0) :- !, statistics, nl, fail.
test2(N) :- f, N1 is N-1, test2(N1).

% No local vars & no structures, yet not recovered...

f(_, _).

test3(0) :- !, statistics, fail.
test3(I) :- f(I, _), I2 is I-1, test3(I2).

main :-
	test1(100000);
	test2(200000);
	test3(300000);
	true.

