:-initialization(main).

main :- \+ (B=_*(A*B),A=B*B,A=B, write(ok1), nl).
main :- \+ (B=B*(A*B),A=B*B,A=B, write(ok2), nl).
main :- \+ (B=B*(A*B),A=B*B,A=B,A == B, write(ok3), nl).
main :- \+ (B+A+A=_*(A*B)+B*B+B, write(ok4), nl).
main :- \+ (B=B*(A*B),A=B*B,A=B,A == B, write(ok5), nl).
main :- write(done), nl.

