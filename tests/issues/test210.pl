:-initialization(main).

main :-
	\+ (dif(A,B),A=[A|A],B=[B|B]).
