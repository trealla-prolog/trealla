:-initialization(main).

main :- A=B,A=B*1,A=1*1*1.
main :- A=B,A+B=B*1+B*A*B.
main :- \+ (A+B=B*1+B*A*B).
main :- A+B=B*1+B*A*B,A=B.
main :- A=B,B=B*A*A,A=B*1,A=B.
main :- B=B*A*A,A=B*1,A=B.
main :- A=B*1,B=B*A*A,A=B.
main :- write(ok), nl.

