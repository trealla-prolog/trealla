:- initialization(main).

main :-
	write_term(S+T,[quoted(true),ignore_ops(true),variable_names(['s'=S,'t'=T])]), nl,
	write_term(S+T,[quoted(true),variable_names(['s'=S,'t'=T])]), nl.
