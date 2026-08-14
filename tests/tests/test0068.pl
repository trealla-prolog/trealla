main :-
	limit(5, offset(5, between(1,20,I))), writeq(I), nl, fail.
main.

:- initialization(main).
