:- dynamic(g/2).
:- dynamic(f/2).
:- dynamic(f/1).

main :-
	test1a, test1b, test1c, test1d,
	test2a, test2b,
	test3,
	test4,
	test5.

test1a :-
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test1a :-
	write('Match using atomic 1st-arg... '),
	between(1,1000000,I),
		g(I,_),
		fail.
test1a :-
	abolish(g/2),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test1b :-
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test1b :-
	write('Match using atomic 2nd-arg... '),
	between(1,1000000,I),
		g(_,I),
		fail.
test1b :-
	abolish(g/2),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test1c :-
	between(1,1000000,I),
		assertz(g(f(I),I)),
		fail.
test1c :-
	write('Match using compound 1st-arg... '),
	between(1,1000000,I),
		g(f(I),_),
		fail.
test1c :-
	abolish(g/2),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test1d :-
	between(1,1000000,I),
		assertz(g(I,f(I))),
		fail.
test1d :-
	write('Match using compound 2nd-arg... '),
	between(1,1000000,I),
		g(_,f(I)),
		fail.
test1d :-
	abolish(g/2),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test2a :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test2a :-
	write('Iterate over set... '),
	f(_),
		fail.
test2a :-
	abolish(f/1),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test2b :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test2b :-
	write('Use findall... '),
	findall(N,f(N),L),
	length(L,Count),
	write('Done...  '), write(Count), write(' items'), nl,
	true.

test3 :-
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test3 :-
	write('Iterate over 2nd-arg... '),
	g(_,_),
		fail.
test3 :-
	abolish(f/1),
	write('Done...  '), write(1000000), write(' items'), nl,
	true.

test4 :-
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test4 :-
	write('Retract... '),
	retract(f(_)),
		fail.
test4 :-
	abolish(f/1),
	write('Done...  '), write(1000000), write(' items'), nl, true.

test5 :-
	between(1,10,_),
		between(1,100000,J),
			assertz(f(J)),
			fail.
test5 :-
	write('Match using once 1st-arg... '),
	between(1,100000,I),
		once(f(I)),
		%write(I), nl,
		fail.
test5 :-
	abolish(f/1),
	write('Done...  '), write(100000), write(' items'), nl, true.

