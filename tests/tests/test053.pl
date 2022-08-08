:-initialization(main).

fib(0,1) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N - 1,
    N2 is N1 - 1,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.

main :-
	fib(20,F),
	write(F), nl.
