:- initialization(main).
:- use_module(library(lists)).

main :-
	findall(I, member(I, [A,B,B,A]), L),
	L = [A1,B1,B2,A2],
	write_term(L, [quoted(true),variable_names(['A1'=A1, 'B1'=B1, 'B2'=B2, 'A2'=A2])]), nl, fail.
main.
