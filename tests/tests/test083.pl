goal_expansion(calln(F,A1), P) :-
	P =.. [F, A1].

goal_expansion(calln(F,A1,A2), P) :-
	P =.. [F,A1,A2].

main :-
	X = hello,
	calln(writeq,user_output,X),
	nl.

:- initialization(main).
