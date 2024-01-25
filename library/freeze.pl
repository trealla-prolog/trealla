:- module(freeze, [
	freeze/2,
	frozen/2
	]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).
:- use_module(library(lists, [flatten/2])).

:- meta_predicate(freeze(-, 0)).
:- attribute frozen/1.

frozen(Term, Goal) :-
	copy_term(Term, _, Gs),
	flatten(Gs, Gs2),
	list_to_conjunction(Gs2, Fresh),
	Fresh = Goal.

:- help(frozen(@term,-callable), [iso(false),desc('Unify Goal with the goal or conjunction of goals delayed on some attributed variable in Term.')]).

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, frozen(Fa)), !,       % are we involved?
	(   var(Other) ->                   % must be attributed then
		(   get_atts(Other,  frozen(Fb)) % has a pending goal?
		->  put_atts(Other,  frozen((Fb,Fa))) % rescue conjunction
		;   put_atts(Other,  frozen(Fa)) % rescue the pending goal
		),
		Goals = []
	;   Goals = [Fa]
	).
verify_attributes(_, _, []).

freeze(X, Goal) :-
	put_atts(Fresh, frozen(Goal)),
	Fresh = X.

:- help(freeze(+var,:callable), [iso(false),desc('Delay the execution of Goal until Var is bound (i.e., is not a variable or attributed variable).')]).

attribute_goals(Var) -->
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze(Var, Goals)].

