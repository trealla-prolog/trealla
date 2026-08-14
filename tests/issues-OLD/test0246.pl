:-initialization(main).

main :-
	call_nth(between(1,100,_), M), M =:= 50, write(M), nl.
