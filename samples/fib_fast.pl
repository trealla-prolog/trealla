test :-
    fib(40,F),
    F =:= 165580141,
	write('fib(40)='), write(F), write(' PASSED'), nl.

fib(A,B) :-
    fib(A,1,1,B).

fib(0,_,A,A).
fib(1,_,A,A).
fib(A,B,C,D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fib(E,C,F,D).
