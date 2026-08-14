:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(V1,writeln(hello(V1))), bb_put(key,V1), bb_get(key,V2), V1=99, V2=98.
