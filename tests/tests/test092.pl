:-initialization(main).

main :-
	A1=[[]|B1],B1=[[],[]|B1],A1=B1,
	A2=[A2|A2],B2=[A2|B2],
	\+ unify_with_occurs_check(A3,[[[]]|A3]).

