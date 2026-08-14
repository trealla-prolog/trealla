:-initialization(main).

main :- atom_concat(X, Y, abcdef),
			write(X), write(' <==> '), write(Y), nl, fail.
main.
