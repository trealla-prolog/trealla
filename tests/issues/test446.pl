:- initialization(main).

main :-
	([c|Y1],b,a) = (X1,Y1), write({X1,Y1}),nl,
	{["c"|Y2],"b,a"} = {X2,Y2}, write((X2,Y2)),nl.


