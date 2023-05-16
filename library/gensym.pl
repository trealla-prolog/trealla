:- module(gensym, [gensym/2,
		   reset_gensym/1]).

gensym(Base, Unique) :-
	( 'clause'('$gensym'(Base, N0), _)
	-> (N is N0 + 1, retract('$gensym'(Base,N0)))
	; N is 1
	),
	atomic_concat(Base, N, Unique),
	assertz('$gensym'(Base, N)).

reset_gensym(Base) :-
	retract('$gensym'(Base, _)).

