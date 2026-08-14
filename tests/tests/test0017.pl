:-initialization(main).

integers(Low,High,[Low|Rest]) :-
	Low =< High,
	!,
	M is Low+1,
	integers(M,High,Rest).
integers(_,_,[]).

main :- integers(1, 100000, L), L=[H|_], write(H), nl.
