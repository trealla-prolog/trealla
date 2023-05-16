:- module(gensym, [gensym/2, reset_gensym/1]).

:- use_module(library(error)).
:- use_module(library(si)).

gensym_key(Base, BaseKey) :-
    atom_concat('gensym_', Base, BaseKey).

gensym(Base, Unique) :-
	must_be(var, Unique),
	atom_si(Base),
    gensym_key(Base, BaseKey),
	( bb_get(BaseKey, UniqueID0)
	-> N is UniqueID0 + 1
	; N is 1
	),
	atomic_concat(Base, N, Unique),
    bb_put(BaseKey, N).

reset_gensym(Base) :-
	atom_si(Base),
    gensym_key(Base, BaseKey),
    bb_put(BaseKey, 0).
