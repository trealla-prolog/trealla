:-initialization(main).

main :-
	write_term(Ö-Œ = s-t, [quoted(true),variable_names(['O'=Ö, 'E'=Œ])]), nl,
	write_term(Ö-Œ = s-t, [quoted(true),variable_names(['O'=Ö, 'E'=Œ])]), nl,
	writeq(-'Ö'-'Œ'+(.)+'A'), nl.
