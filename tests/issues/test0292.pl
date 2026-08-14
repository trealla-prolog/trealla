:-initialization(main).

main :-
	L=[_|"2"],
	length(L,N),
	write(N), nl,
	length([1,2|"3"],L3),
	write(L3), nl.
