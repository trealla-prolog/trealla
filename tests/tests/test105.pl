:- use_module(library(freeze)).
:- initialization(main).

main :-
	freeze(V1,writeln(hello(V1))), bb_b_put(key,V1), bb_get(key,V2), V2=99.

