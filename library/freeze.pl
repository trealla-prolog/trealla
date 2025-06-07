:- module(freeze, [ freeze/2, frozen/2]).

/** Provides the constraint `freeze/2`.
*/

:- use_module(library(atts)).
:- use_module(library(dcgs)).

:- meta_predicate(freeze(-, 0)).
:- attribute frozen/1.

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

%% freeze(Var, Goal)
%
%  Schedules Goal to be executed when Var is instantiated. This can
%  be useful to observe the exact moment a variable becomes bound to a
%  more concrete term, for example when creating animations of search
%  processes. Higher-level constructs such as `phrase_from_file/2` can
%  also be implemented with `freeze/2`, by scheduling a goal that
%  reads additional data from a file as soon as it is needed.

freeze(X, Goal) :-
    put_atts(Fresh, frozen(Goal)),
    Fresh = X.

attribute_goals(Var) -->
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze(Var, Goals)].



/** Provides the predicate `frozen/2`.
*/

:- use_module(library(lists)).

frozen(Term, Goal) :-
	copy_term(Term, Term2, Gs),
	Term = Term2,
	flatten_(Gs, Gs2),
	list_to_conjunction(Gs2, Fresh),
	Fresh = Goal.

:- help(freeze(+var,:callable), [iso(false),desc('Delay the execution of Goal until Var is bound (i.e., is not a variable or attributed variable).')]).
:- help(frozen(@term,-callable), [iso(false),desc('Unify Goal with the goal or conjunction of goals delayed on some attributed variable in Term.')]).

list_to_conjunction(List0, T) :-
	reverse(List0, List),
	toconjunction_(List, true, T).

toconjunction_([], In, In).
toconjunction_([H|T], true, Out) :- !,
	Out2 = H,
	toconjunction_(T, Out2, Out).
toconjunction_([H|T], In, Out) :-
	Out2 = (H, In),
	toconjunction_(T, Out2, Out).

conjunction_to_list(T, List) :-
	tolist_(T, List).

tolist_((T1,T2), [T1|Rest]) :- !,
	tolist_(T2, Rest).
tolist_(T, [T|[]]).
