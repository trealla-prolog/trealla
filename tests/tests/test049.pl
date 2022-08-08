:-initialization(main).

foo(a, b, c).
foo(a, b, d).
foo(a, b, a).
foo(a, b, a).
foo(b, c, f).
foo(b, c, e).
foo(c, c, g).

main :-
	test1a, test2a, test3a, test4a,
	test1b, test2b, test3b, test4b.

test1a :-
	findall(C,foo(_,_,C),L),
	write(L), nl,
	fail.
test1a.

test1b :-
	findall(bar(C),foo(_,_,C),L),
	write(L), nl,
	fail.
test1b.

test2a :-
	bagof(C,foo(_,_,C),L),
	write(L), nl,
	fail.
test2a.

test2b :-
	bagof(bar(C),foo(_,_,C),L),
	write(L), nl,
	fail.
test2b.

test3a :-
	bagof(C,A^B^foo(A,B,C),L),
	write(L), nl,
	fail.
test3a.

test3b :-
	bagof(bar(C),A^B^foo(A,B,C),L),
	write(L), nl,
	fail.
test3b.

test4a :-
	setof(C,A^B^foo(A,B,C),L),
	write(L), nl,
	fail.
test4a.

test4b :-
	setof(bar(C),A^B^foo(A,B,C),L),
	write(L), nl,
	fail.
test4b.

