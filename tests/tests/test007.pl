:-initialization(main).

g(a). g(b).

main :-
	ignore((g(X),X==z)),
	ignore((g(X),X==a)),
	write(X), nl.
