:- module(attrmod, [domain/2, test/0]).

:- use_module(library(dif)).		% get from scryer
:- use_module(library(ordsets)).
:- use_module(library(atts)).

:- attribute domain/1.

verify_attributes(Var, Other, []) :-
	(   get_atts(Var, domain(Dom1)) ->
		(   var(Other),
			get_atts(Other, domain(Dom2)) ->
			ord_intersection(Dom1, Dom2, Dom),
			dif(Dom, []),
			(   Dom = [Value] ->
				Other = Value
			;   put_atts(Other, domain(Dom))
			)
		;   ord_memberchk(Other, Dom1)
		)
	;   true
	).

attribute_goals(Var) -->
	(   { get_atts(Var, domain(Dom)) } ->
		[domain(Var, Dom)]
	;   []
	).

domain(X, List) :-
	list_to_ord_set(List, Dom),
	put_atts(X, domain(Dom)).

test :- attrmod:domain(X, [a,b,c]), attrmod:domain(Y, [c,d,e]), X=Y, write(X), nl.
