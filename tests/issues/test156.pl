:-initialization(main).

main :-
	?=(a,a), writeln(here3),
	unifiable(a,a,X2), writeln(here2),
	unifiable(a,A,X1), writeln(here1),
	true.
