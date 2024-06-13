:- module(atts, [
		op(1199, fx, attribute)
	]).

:- use_module(library(apply)).
:- use_module(library(lists), [append/3]).

msb(_X, _N) :- writeln(oops_msb).
lsb(_X, _N) :- writeln(oops_lsb).
popcount(_X, _N) :- writeln(oops_popcount).
lcm(_X, _N, _) :- writeln(oops_lcm).

get_attr(Var, Module, Value) :-
	var(Var),
	Access =.. [Module, Value],
	get_atts(Var, Access).

put_attr(Var, Module, Value) :-
	Access =.. [Module, Value],
	put_atts(Var, Access).

del_attr(Var, Module) :-
	Access =.. [Module, _],
	var(Var) -> put_atts(Var, -Access); true.
