:- help(subsumes_term(+term,+term), [iso(true)]).

subsumes_term(G, S) :-
	\+ \+ (
	 term_variables(S, V1),
	 G = S,
	 term_variables(V1, V2),
	 V2 == V1
	).

:- meta_predicate(countall(0,?)).
:- help(countall(:callable,?integer), [iso(true)]).

countall(_, N) :-
	can_be(N, integer, countall/2, _),
	integer(N),
	(N >= 0 -> true; throw(error(domain_error(not_less_than_zero, N), countall/2))),
	fail.
countall(G, N) :-
	'$countall'(call(G), N0),
	N = N0.

:- meta_predicate(call_cleanup(0,0)).
:- help(call_cleanup(:callable,:callable), [iso(false)]).

call_cleanup(G, C) :-
	(var(C) -> throw(error(instantiation_error, call_cleanup/3)); true),
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		(catch(ignore(C), _, true), throw(Err))
	).

:- meta_predicate(setup_call_cleanup(0,0,0)).
:- help(setup_call_cleanup(:callable,:callable,:callable), [iso(false)]).

setup_call_cleanup(S, G, C) :-
	once(S),
	(var(C) -> throw(error(instantiation_error, setup_call_cleanup/3)); true),
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		(catch(ignore(C), _, true), throw(Err))
	).

:- meta_predicate(forall(0,0)).
:- help(forall(:callable,:callable), [iso(false)]).

forall(Cond, Action) :-
	\+ (Cond, \+ Action).

:- help(succ(?integer,+integer), [iso(false)]).
:- help(succ(+integer,-integer), [iso(false)]).

succ(I, S) :-
    can_be(not_less_than_zero, I),
    can_be(not_less_than_zero, S),
    (   integer(S) ->
        S > 0,
        I is S-1
    ;   integer(I) ->
        S is I+1
    ;   instantiation_error(succ/2)
    ).

:- help(cfor(+evaluable,+evaluable,-var), [iso(false),desc('C-style for loop')]).

cfor(I0,J0,K) :-
	I is I0,
	J is J0,
	between(I, J, K).

:- help(variant(+term,+term), [iso(false)]).

variant(X,Y) :-
	\+ \+ ( copy_term(X,XC),
		subsumes_term(XC,Y),
		subsumes_term(Y,XC)
	).

:- help(call_det(:callable,?boolean), [iso(false)]).
:- meta_predicate(call_det(0,?)).

call_det(G, Det) :-
	'$get_level'(L1),
	call(G),
	'$get_level'(L2),
	(L1 = L2 -> Det = true; Det = false).

:- meta_predicate(findall(?,0,-,?)).
:- help(findall(+term,:callable,-list,+list), [iso(false)]).

findall(T, G, B, Tail) :-
	can_be(B, list, findall/4, _),
	can_be(Tail, list, findall/4, _),
	findall(T, G, B0),
	append(B0, Tail, B), !.

:- meta_predicate(call_with_time_limit(+,0)).
:- help(call_with_time_limit(+millisecs,:callable), [iso(false)]).

call_with_time_limit(Time, Goal) :-
	Time0 is truncate(Time * 1000),
	'$alarm'(Time0),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E))) ->
		'$alarm'(0)
	;	('$alarm'(0), fail)
	).

:- meta_predicate(time_out(0,+,-)).
:- help(time_out(:callable,+integer,?atom), [iso(false)]).

time_out(Goal, Time, Result) :-
	'$alarm'(Time),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E))) ->
		('$alarm'(0), Result = success)
	;	('$alarm'(0), fail)
	).

:- help(not(:callable), [iso(false),deprecated(true)]).
:- meta_predicate(not(0)).

not(X) :- X, !, fail.
not(_).

:- help(term_variables(+term,-list,?tail), [iso(false)]).

term_variables(P1, P2, P3) :-
	term_variables(P1, P4),
	append(P4, P3, P2).

