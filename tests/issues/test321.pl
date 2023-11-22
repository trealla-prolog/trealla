:-initialization(main).

main :- \+ (B=_*(A*B),A=B*B,A=B).
main :- \+ (B=B*(A*B),A=B*B,A=B).
main :- \+ (B=B*(A*B),A=B*B,A=B,A == B).
main :- \+ (B+A+A=_*(A*B)+B*B+B).
main :- \+ ( B=B*(A*B),A=B*B,A=B, A == B ).
main :- write(ok), nl.

