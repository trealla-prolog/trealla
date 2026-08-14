main :-
	L=[aa,bb,cc],L=[_|T],copy_term(T,T2),
	writeq(T2), nl.

:- initialization(main).
