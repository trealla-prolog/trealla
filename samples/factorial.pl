fac(N,F) :-
    factorial(N,1,F).

factorial(0,F,F).
factorial(N,Tot,F) :-
    NewTot is Tot * N,
    N1 is N - 1,
    factorial(N1,NewTot,F).

test :-
    fac(5,F1),
    F1 = 120,
    write('fac(5)='), write(F1), write(' PASSED'), nl.

test1 :-
    fac(20,F2),
    F2 = 2432902008176640000,
    write('fac(20)='), write(F2), write(' PASSED'), nl.

test2 :-
	between(1,2,_),
		fac(5,F1),
		F1 = 120,
		fail.
test2 :- write('PASSED'), nl.
