:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(V,writeln(here)),
	duplicate_term([V],L),
	'$list_attributed'(0,Atts),
	length(Atts,N),
	write(N), nl.
