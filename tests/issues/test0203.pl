:-initialization(main).

main :-
	B = a([B]), A = [a([B])], A = [a(A)],
	B = a([B]), A = [a(A)], A = [a([B])].
