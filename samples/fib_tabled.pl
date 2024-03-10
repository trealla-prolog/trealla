:- use_module(library(tabling)).

:- table fib/2.

fib(0,1) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N - 1,
    N2 is N1 - 1,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.

test :-
    fib(30,F),
    F =:= 1346269,
    write('fib(30)='), write(F), write(' PASSED'), nl.

test1 :-
    fib(35,F),
    F =:= 14930352,
    write('fib(35)='), write(F), write(' PASSED'), nl.

test2 :-
    fib(40,F),
    F =:= 165580141,
    write('fib(40)='), write(F), write(' PASSED'), nl.
