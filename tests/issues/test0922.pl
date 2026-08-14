:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(V,writeln(here)),
	findall(V,L=[V],L),
	L=[V2],
	V2=1.
