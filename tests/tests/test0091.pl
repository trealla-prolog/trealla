:-initialization(main).

main :-
	X1=f(_),copy_term([123|X1],C1), C1 = [123|f(Copy1)], write_term(C1, [variable_names(['Copy1'=Copy1])]), nl,
	X2=f(L2),L2=[123|X2],copy_term(L2,C2), write(C2), nl.
