:- initialization(main).

main :-
	format("~`*t NICE TABLE ~`*t~61|~n", []),
	format("*~t~d~20|~t~d~t~40|~d~t~40|~t*~61|~n", [123,45,678]),
	true.
