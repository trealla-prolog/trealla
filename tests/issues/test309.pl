:-initialization(main).

main :-
	\+ (A + B + C = (C*2) + (A*2) + (C*1*2), A=B, write(oops), nl), write(ok), nl,
	\+ (A1=B1*1, B1=B1*A1*A1, A1=B1, write(oops1), nl), write(ok1), nl, fail.
main.

