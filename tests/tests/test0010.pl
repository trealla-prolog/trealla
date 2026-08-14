:-initialization(main).

upto(N,X) :- N > 0, N1 is N - 1, upto(N1,X).
upto(N,X) :- true, N > 0, X = N.

main :- \+ ( upto(3,I), upto(I,J), \+ (write([I,J]), nl) ).
