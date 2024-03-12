% Mockup of blackboarding. Needs to be
% rewritten as builtins.

bb_put(K, V) :-
	'$must_be'(K, atom, bb_put/2, _),
	prolog_load_context(module, M),
	ignore(retractall(M:'$bb_key'(K, _, _))),   % both 'b' & 'nb'
	asserta(M:'$bb_key'(K, V, nb)).

:- help(bb_put(+atom,+term), [iso(false)]).

bb_get(K, V) :-
	'$must_be'(K, atom, bb_get/2, _),
	prolog_load_context(module, M),
	M:'$bb_key'(K, V, _),						% either 'b' or 'nb'
	!.

:- help(bb_get(+atom,?term), [iso(false)]).

bb_delete(K, V) :-
	'$must_be'(K, atom, bb_delete/2, _),
	prolog_load_context(module, M),
	M:'$bb_key'(K, V0, _),
	!,
	V0 = V,
	retractall(M:'$bb_key'(K, _, _)).			% both 'b' & 'nb'

:- help(bb_delete(+atom,+term), [iso(false)]).

bb_update(K, O, N) :-
	bb_get(K, O),
	bb_put(K, N).

:- help(bb_update(+atom,+term,+term), [iso(false)]).

% extension:

bb_b_put(K, V) :-
	'$must_be'(K, atom, bb_b_put/2, _),
	prolog_load_context(module, M),
	ignore(retract(M:'$bb_key'(K, _, b))),		% just 'b'
	asserta(M:'$bb_key'(K, V, b), Ref),
	'$retract_on_backtrack'(Ref).

:- help(bb_b_put(+atom,+term), [iso(false), desc('Backtrackable version of bb_put/2')]).

countall(_, N) :-
	integer(N),
	(N >= 0 -> true; throw(error(domain_error(not_less_than_zero, N), countall/2))),
	fail.
countall(G, N) :-
	'$countall'(call(G), N0),
	N = N0.

:- meta_predicate(countall(0,?)).
:- help(countall(:callable,?integer), [iso(true)]).

call_cleanup(G, C) :-
	(var(C) -> throw(error(instantiation_error, call_cleanup/3)); true),
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		( catch(ignore(C), _, true), throw(Err))
	).

:- meta_predicate(call_cleanup(0,0)).
:- help(call_cleanup(:callable,:callable), [iso(false)]).

setup_call_cleanup(S, G, C) :-
	once(S),
	(var(C) -> throw(error(instantiation_error, setup_call_cleanup/3)); true),
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		(catch(ignore(C), _, true), throw(Err))
	).

:- meta_predicate(setup_call_cleanup(0,0,0)).
:- help(setup_call_cleanup(:callable,:callable,:callable), [iso(false)]).

forall(Cond, Action) :-
	\+ (call(Cond), \+ call(Action)).

:- meta_predicate(forall(0,0)).
:- help(forall(:callable,:callable), [iso(false)]).


succ(X,S) :- nonvar(X), Y=1, nonvar(Y),
	'$must_be'(X, integer, succ/2, _), '$must_be'(Y, integer, succ/2, _), !,
	(	X >= 0 -> true
	; 	throw(error(domain_error(not_less_than_zero, X), succ/2))
	),
	S is X + Y.
succ(X,S) :- var(X), Y=1, nonvar(Y), nonvar(S),
	'$must_be'(S, integer, succ/2, _), '$must_be'(Y, integer, succ/2, _), !,
	(S >= 0 -> true ; throw(error(domain_error(not_less_than_zero, S), succ/2))),
	!,
	S > 0,
	X is S - Y.
succ(_,_) :-
	throw(error(instantiation_error, succ/2)).

:- help(succ(?integer,+integer), [iso(false)]).
:- help(succ(+integer,-integer), [iso(false)]).

