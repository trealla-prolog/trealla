:-initialization(main).

qsort([X|L],R,R0) :-
	mypartition(L,X,L1,L2),
	qsort(L2,R1,R0),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

mypartition([X|L],Y,[X|L1],L2) :-
	X < Y,
	mypartition(L,Y,L1,L2).
mypartition([X|L],Y,L1,[X|L2]) :-
	mypartition(L,Y,L1,L2).
mypartition([],_,[],[]).

main :-
	list50(L),
	qsort(L,X,[]),
	write(X), nl.

list50([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18,92,40,53,59,8]).
