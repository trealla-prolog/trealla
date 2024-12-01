main :-
	L = [_,_,_| L], copy_term(L,V), V=[_,_,_|T], T == V,
	writeq(L), nl,
	writeq(V), nl,
	L = V.

:- initialization(main).
