:-initialization(main).

main :- main1, main2.

main1 :-
	A1=[[]|B1],B1=[[],[]|B1],A1=B1,
	A2=[A2|A2],B2=[A2|B2],
	\+ unify_with_occurs_check(A3,[[[]]|A3]).

main2 :-
	\+ (A4=[A4|A4],A4=[B4,A4]),
	\+ (A5=B5,A5=[A5|A5],A5=[B5,A5]).
