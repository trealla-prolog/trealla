:-initialization(main).

main :- A=B,B=B*A*A,A=B*1,A=B.
main :- B=B*A*A,A=B*1,A=B.
main :- write(ok), nl.

