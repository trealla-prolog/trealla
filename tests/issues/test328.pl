:-initialization(main).

main :- A=B,A=A*1,B=B*B.
main :- A=A*1,B=B*B,A=B.
main :- write(ok), nl.

