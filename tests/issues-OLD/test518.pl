main :-
	V1=V1-X1, W1=V1, unify_with_occurs_check(V1,W1),
	V2=V2-X2, W2=W2-X2, unify_with_occurs_check(V2,W2),
	V3=V3-X3, W3=W3-Y3, unify_with_occurs_check(V3,W3),
	V4=V4-X4, W4=W4-s(X4), \+ unify_with_occurs_check(V4,W4),
	V5=V5-X5, unify_with_occurs_check(V5,W5),
	\B=A, C=[B|A], acyclic_term([C,C]),
	\D=A, C=[B|A], \+ unify_with_occurs_check(B,[C|A]),
	write(ok), nl.

:- initialization(main).
