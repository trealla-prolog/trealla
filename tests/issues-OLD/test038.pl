:-initialization(main).
:- use_module(library(format)).

main :-
	phrase(format_("~16r", [12]), Ls1), writeq(Ls1), nl,
	phrase(format_("~8r", [12]), Ls2), writeq(Ls2), nl.
