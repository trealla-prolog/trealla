:-initialization(main).

main :-
	X1=f(L1),copy_term([123|X1],C1), writeln(C1),
	X2=f(L2),L2=[123|X2],copy_term(L2,C2), writeln(C2).
