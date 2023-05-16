:- module(gensym, [gensym/2,
		   reset_gensym/1]).

:- use_module(library(error)).
:- use_module(library(si)).

gensym(Base, Unique) :-
	must_be(var, Unique),
	atom_si(Base),
	( bb_get(Base, N0)
	-> N is N0 + 1
	; N is 1
	),
	atomic_concat(Base, N, Unique),
    bb_put(Base, N).

reset_gensym(Base) :-
	atom_si(Base),
    bb_put(Base, 0).
