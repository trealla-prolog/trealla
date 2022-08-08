qsort([X|L],R,R0) :-
	partition(L,X,L1,L2),
	% write(L), nl, write(L1), nl, write(L2), nl, nl,
	qsort(L2,R1,R0),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([X|L],Y,[X|L1],L2) :-
	X < Y,
	partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
	partition(L,Y,L1,L2).
partition([],_,[],[]).

test :-
	list50(L),
	sort50(X),
	qsort(L,Y,[]), !,
	ground(Y),
	write(Y), nl,
	X=Y,
	write('PASSED'), nl.

list50([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18,92,40,53,59,8]).
sort50([0,2,4,6,7,8,10,11,11,17,18,18,21,27,27,28,28,28,29,31,32,33,37,39,40,46,47,51,53,53,55,59,61,63,65,66,74,74,75,81,82,83,85,85,90,92,94,95,99,99]).
