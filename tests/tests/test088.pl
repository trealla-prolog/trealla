main :-
	L1=[1|L1], L2=[1|L2], L1=L2,
	S2=f(1,S1), S2=f(1,S2), S1=S2.

:- initialization(main).
