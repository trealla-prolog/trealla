:-initialization(main).

main :-
	?=(a,a), write(here3), nl,
	unifiable(a,a,X2), write(here2), nl,
	unifiable(a,A,X1), write(here1), nl,
	true.
