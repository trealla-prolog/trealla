:- initialization(main).

test(G, Error, Context) :- catch(G, error(Error,Context), (writeq(ok), nl)).

main :-
	test(term_variables(t,[_,_|a]), E, C),
	E = type_error(list, [First,Second|a]),
	write_term(E, [variable_names(['First'=First, 'Second'=Second])]), nl,
	write(C), nl.
