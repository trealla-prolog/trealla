:- initialization(main).
:- use_module(library(apply)).

main :-
	length(_, E), N is 2^E,
	writeq(E), nl,
	length(Ls, N),
	maplist(=(a), Ls),
	writeq(ok), nl.
