:- initialization(main).

main :-
	S = s(s(A,s(B,A)),1), T = s(s(C,C),1), \+ unify_with_occurs_check(S,T).
