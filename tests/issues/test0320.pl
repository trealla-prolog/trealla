:-initialization(main).

main :- A=B,C=A*1,A=B*A,B=B*C*A,A=B.
main :- C=A*1,A=B*A,B=B*C*A,A=B, A == B.
main :- C=A*1,A=B*A,B=B*C*A, A = B.
main :- write(ok), nl.

