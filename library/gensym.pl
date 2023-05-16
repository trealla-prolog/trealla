:- module(gensym, [gensym/2,
		   reset_gensym/1]).

:- use_module(library(error)).
:- use_module(library(si)).

gensym(Base, Unique) :-
	must_be(var, Unique),
	atom_si(Base),
	( 'clause'('$gensym'(Base, N0), _)
	-> (N is N0 + 1, retract('$gensym'(Base,N0)))
	; N is 1
	),
	atomic_concat(Base, N, Unique),
	assertz('$gensym'(Base, N)).

reset_gensym(Base) :-
	atom_si(Base),
	retract('$gensym'(Base, _)).

