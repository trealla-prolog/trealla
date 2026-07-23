:- module(atts, [
		op(1199, fx, attribute),
		get_attr/3,
		put_attr/3,
		del_attr/2,
		get_atts/3,
		put_atts/3
	]).

% Compatability

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

user:goal_expansion(get_atts(V, M, A), M:get_atts(V, A)).
user:goal_expansion(put_atts(V, M, A), M:put_atts(V, A)).
