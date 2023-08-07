:-initialization(main).

:- dynamic foo/1.

test :-
    clause(foo(_), Body),
    call(Body).

foo(bar).

main :-
	test.

