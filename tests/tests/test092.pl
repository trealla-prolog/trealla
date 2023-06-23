:-initialization(main).

main :-
	A=[[]|B],B=[[],[]|B],A=B,
	A=[A|A],B=[A|B],
	\+ unify_with_occurs_check(A,[[[]]|A]).

