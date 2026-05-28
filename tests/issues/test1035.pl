:-use_module(library(iso_ext)).
:- initialization(main).

f :- bb_b_put(abc,123), bb_get(abc,V), write(V), nl, write(returning), nl.

main :- bb_put(abc,999), f, bb_get(abc,V), write(V), nl, fail;
	write(retry), nl, bb_get(abc,V), write(V), nl,
	write(done), nl.
