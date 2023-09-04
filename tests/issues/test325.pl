:-initialization(main).

main :- A=B,A=B*1,B=B*A*B,A=B.
main :- A=B*1,B=B*A*B,A=B.
main :- write(ok), nl.

