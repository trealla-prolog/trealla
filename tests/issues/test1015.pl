:- initialization(main).

main :-
	% Exact example from issue #1015
	format('~w~t~w~t~w~t~w~t~w~15|', [a,b,c,d,e]), nl,
	% Same example bracketed so the column-15 boundary is visible
	format('[~w~t~w~t~w~t~w~t~w~15|]', [a,b,c,d,e]), nl,
	% Evenly divisible (no remainder) still works
	format('[~w~t~w~t~w~t~w~t~w~17|]', [a,b,c,d,e]), nl,
	% Single tab
	format('[~w~t~w~10|]', [a,b]), nl,
	% Two tabs with remainder 1
	format('[~w~t~w~t~w~6|]', [a,b,c]), nl,
	% Column directive with no tabs
	format('[~w~w~10|x]', [a,b]), nl,
	true.
