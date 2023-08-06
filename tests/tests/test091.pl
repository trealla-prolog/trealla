:-initialization(main).

main :-
	X1=f(L1),copy_term([123|X1],C1), write(C1), nl,
	X2=f(L2),L2=[123|X2],copy_term(L2,C2), write(C2), nl.
