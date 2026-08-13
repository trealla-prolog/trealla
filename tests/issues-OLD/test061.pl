:- initialization(main).

:- use_module(library(lists)).

main :-
    maplist(col([[1,2],[3,4]]), [1,2], X1),
    writeq(X1), nl,
    maplist(col([[A,2],[3,A]]), [1,2], X2),
	write_term(X2, [variable_names(['A'=A])]), nl,
	maplist(length, M, [2,2]), M = [[M11,M12],[M21,M22]],
	write_term(M, [quoted(true),variable_names(['M11'=M11, 'M12'=M12, 'M21'=M21, 'M22'=M22])]), nl,
    maplist(col(M), [1,2], X),
	write_term(X, [quoted(true),variable_names(['M11'=M11, 'M12'=M12, 'M21'=M21, 'M22'=M22])]), nl.

col(Matrix, N, Column) :-
    maplist(nth1(N), Matrix, Column).
