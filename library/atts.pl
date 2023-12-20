:- module(atts, [
		op(1199, fx, attribute),
		call_residue_vars/2,
		term_attributed_variables/2
	]).

:- use_module(library(apply)).
:- use_module(library(lists), [append/3]).

msb(_X, _N) :- writeln(oops_msb).
lsb(_X, _N) :- writeln(oops_lsb).
popcount(_X, _N) :- writeln(oops_popcount).
lcm(_X, _N, _) :- writeln(oops_lcm).

'$post_unify_hook' :-
	'$undo_trail'(Vars, State),
	process_vars_(Vars, [], Goals),
	'$redo_trail'(State),
	maplist(call, Goals).

process_vars_([], Goals, Goals).
process_vars_([Var-Val|Vars], SoFar, Goals) :-
	(	get_atts(Var, Atts) ->
		process_var_(Atts, Var, Val, SoFar, MoreGoals),
		process_vars_(Vars, MoreGoals, Goals)
	;	process_vars_(Vars, SoFar, Goals)
	).

process_var_([], _, _, Goals, Goals).
process_var_([Att|Atts], Var, Val, SoFar, Goals) :-
	functor(Att, F, A),
	attribute(M, F, A),
	M:verify_attributes(Var, Val, NewGoals),
	append(SoFar, NewGoals, MoreGoals),
	process_var_(Atts, Var, Val, MoreGoals, Goals).

term_attvars_([], VsIn, VsIn).
term_attvars_([H|T], VsIn, VsOut) :-
	(	'$attributed_var'(H) ->
		term_attvars_(T, [H|VsIn], VsOut)
	;	term_attvars_(T, VsIn, VsOut)
	).

term_attributed_variables(Term, Vs) :-
	term_variables(Term, Vs0),
	term_attvars_(Vs0, [], Vs).

:- help(term_attributed_variables(+term,-list), [iso(false)]).

call_residue_vars(Goal, Atts) :-
	Goal,
	term_attributed_variables(Goal, Atts).

:- meta_predicate(call_residue_vars(0, ?)).
:- help(call_residue_vars(:callable, -list), [iso(false), desc('Find residual attributed variables left by Goal. This predicate is intended for reasoning about and debugging programs that use coroutining or constraints. To see why this predicate is necessary, consider a predicate that poses contradicting constraints on a variable, and where that variable does not appear in any argument of the predicate and hence does not yield any residual goals on the toplevel when the predicate is invoked. Such programs should fail, but sometimes succeed because the constraint solver is too weak to detect the contradiction. Ideally, delayed goals and constraints are all executed at the end of the computation. The meta predicate call_residue_vars/2 finds variables that are given attributes or whose attributes are modified by Goal, regardless of whether or not these variables are reachable from the arguments of Goal.')]).

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
