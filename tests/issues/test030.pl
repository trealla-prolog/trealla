:-initialization(main).


isl({L},N) :- L = (_ is _+N).

testmem(Lim) :- findall(Z,(between(1,Lim,N), isl({X}, N),
                           X = (Y is B+_), findall(Y,(between(1,3,B),X),Z)),
	                M),
	        M1 =[[2,3,3],[3,4,4],[4,5,5],[5,6,6],[6,7,7]],
	        !,
	        member(Q,M1), \+member(Q,M),
	        findall([_,_,Q3], member([_,_,Q3],M),F),
                length(F,Lf), write(Lf), nl, false.


main :- testmem(1000000);true.

