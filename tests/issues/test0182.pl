:-initialization(main).
:- use_module(library(dcgs)).
:- use_module(library(format)).

main :-
	phrase(format_("~q", [abc]), X),
	write(X), nl.
