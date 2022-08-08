:-initialization(main).

f5(F) :- F=f(X,Y,Z), X=1, Y=2, Z=3.

main :- f5(X), write(X), nl.
