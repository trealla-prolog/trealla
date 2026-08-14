:-initialization(main).

main :-
	catch((
		length(_, E),
		(E is 15 -> (write(ok), nl, throw(err(halt))) ; true),
		X is 2^E,
		write(X), nl,
		length(_Ls, X),
		fail),
		err(halt),
		fail).
main.
