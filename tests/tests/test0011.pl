:-initialization(main).

upto(_,N,X) :- N > 0, N2 is N - 1, upto(c,N2,X).
upto(_,N,X) :- N > 0, X = N.

main :- upto(a,3,I), upto(b,I,J), write([I,J]), nl, fail.
main.
