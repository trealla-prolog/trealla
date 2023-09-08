:- module(atts, [
		op(1150, fx, attribute),
		call_residue_vars/2
	]).

:- use_module(library(apply)).
:- use_module(library(lists), [append/3]).

'$post_unify_hook' :-
	setup_call_cleanup(
		'$undo_trail'(Vars),
		process_vars_(Vars, [], Goals),
		'$redo_trail'),
	maplist(call, Goals).

process_vars_([], Goals, Goals) :- !.
process_vars_([Var-Val|Vars], SoFar, Goals) :-
	get_atts(Var, Atts),
	process_var_(Var, Val, Atts, SoFar, MoreGoals),
	process_vars_(Vars, MoreGoals, Goals).

process_var_(_, _, [], Goals, Goals) :- !.
process_var_(Var, Val, [Att|Atts], SoFar, Goals) :-
	functor(Att, F, A),
	attribute(M, F, A),
	ignore(M:verify_attributes(Var, Val, NewGoals)),
	append(SoFar, NewGoals, MoreGoals),
	process_var_(Var, Val, Atts, MoreGoals, Goals).

call_residue_vars(Goal, Atts) :-
	Goal,
	term_attvars(Goal, Atts).

:- meta_predicate(call_residue_vars(0,?)).
:- help(call_residue_vars(:callable, -list), [iso(false), desc('Find residual attributed variables left by Goal. This predicate is intended for reasoning about and debugging programs that use coroutining or constraints. To see why this predicate is necessary, consider a predicate that poses contradicting constraints on a variable, and where that variable does not appear in any argument of the predicate and hence does not yield any residual goals on the toplevel when the predicate is invoked. Such programs should fail, but sometimes succeed because the constraint solver is too weak to detect the contradiction. Ideally, delayed goals and constraints are all executed at the end of the computation. The meta predicate call_residue_vars/2 finds variables that are given attributes or whose attributes are modified by Goal, regardless of whether or not these variables are reachable from the arguments of Goal.')]).
