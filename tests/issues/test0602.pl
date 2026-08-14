:- initialization(main).

hello(a).
hello(b).
hello(c).
test(ok).

main :-
	hello(X), test(Y), write([X,Y]), nl, fail; true.
