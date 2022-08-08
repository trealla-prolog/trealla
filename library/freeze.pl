:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).

:- meta_predicate(freeze(?, 0)).
:- attribute frozen/1.

frozen(Term, Goal) :-
	copy_term(Term, _, Gs),
	flatten(Gs, Gs2),
	toconjunction(Gs2, Goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, frozen(Fa)), !,       % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other,  frozen(Fb)) % has a pending goal?
            ->  ( var(Fb) ->
				  put_atts(Other,  frozen(Fa))
				; put_atts(Other,  frozen((Fb,Fa))) % rescue conjunction
				)
            ;   put_atts(Other,  frozen(Fa)) % rescue the pending goal
            ),
            Goals = []
        ;   Goals = [Fa]
        ).
verify_attributes(_, _, []).

freeze(X, Goal) :-
    put_atts(Fresh, frozen(Goal)),
    Fresh = X.

attribute_goals(Var) -->
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze:freeze(Var, Goals)].

