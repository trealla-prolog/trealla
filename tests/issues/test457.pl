:- initialization(main).

main :-
	length(L,4), append(L,[_|Z],LT), [X,Y,Z] = [x,[X,_|Z],[_,y|Z]], Y = LT , Z = LT, [_,_,_,_,X3,Y3|_] = LT.
