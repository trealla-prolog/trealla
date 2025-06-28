:- initialization(main).

main :-
	L="0"||_,
	format("~q~n",[L]),
	L0="0"||abc,
	format("~q~n",[L0]),
	L1="0"||123,
	format("~q~n",[L1]).
