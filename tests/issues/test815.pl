:- initialization(main).

main :-
	L="0"||_,
	write_term(L, [double_quotes(true)]), nl,
	L0="0"||abc,
	write_term(L0, [double_quotes(true)]), nl,
	L1="0"||123,
	write_term(L1, [double_quotes(true)]), nl.
