:- module(atts, [
		op(1199, fx, attribute),
		term_attributed_variables/2,
		copy_term/3
	]).

:- use_module(library(apply)).
:- use_module(library(lists), [append/3]).

msb(_X, _N) :- writeln(oops_msb).
lsb(_X, _N) :- writeln(oops_lsb).
popcount(_X, _N) :- writeln(oops_popcount).
lcm(_X, _N, _) :- writeln(oops_lcm).

term_attvars_([], VsIn, VsIn).
term_attvars_([H|T], VsIn, VsOut) :-
	(	'$attributed_var'(H) ->
		term_attvars_(T, [H|VsIn], VsOut)
	;	term_attvars_(T, VsIn, VsOut)
	).

term_attributed_variables(Term, Vs) :-
	can_be(Vs, list, term_attributed_variables/2, _),
	term_variables(Term, Vs0),
	term_attvars_(Vs0, [], Vs).

:- help(term_attributed_variables(+term,-list), [iso(false), desc('Return list of attributed variables in term')]).

collect_goals_(_, [], GsIn, GsIn).
collect_goals_(V, [H|T], GsIn, GsOut) :-
	nonvar(H),
	H =.. [M, _],
	catch(M:attribute_goals(V, Goal0, []), _, Goal0 = put_atts(V, +H)),
	!,
	(Goal0 = [H2] -> Goal = H2 ; Goal = Goal0),
	collect_goals_(V, T, [Goal|GsIn], GsOut).
collect_goals_(V, [_|T], GsIn, GsOut) :-
	collect_goals_(V, T, GsIn, GsOut).

collect_goals_([], GsIn, GsIn).
collect_goals_([V|T], GsIn, GsOut) :-
	get_atts(V, Ls),
	collect_goals_(V, Ls, GsIn, GsOut2),
	collect_goals_(T, GsOut2, GsOut).

copy_term(Term, Copy, Gs) :-
	duplicate_term(Term, Copy),
	term_attributed_variables(Copy, Vs),
	collect_goals_(Vs, [], Gs).

:- help(copy_term(+term,?term,+list), [iso(false)]).

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
