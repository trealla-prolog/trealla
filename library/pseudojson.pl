/*
	Almost but not quite entirely unlike JSON.
	(Ab)uses Prolog terms to parse/generate JSON.
	Very fast but definitely not good for validation.

	json_value/2 uses the same format as library(json):
	- booleans: boolean(true).
	- numbers:  number(123).
	- strings:  string("foo").
	- arrays:   list([Value1, Value2, ...]).
	- objects:  pairs([string("key")-Value, ...])
	- null:     null.
*/
:- module(pseudojson, [json_chars/2, json_value/2]).

json_chars(JSON, Cs) :-
	ground(JSON),
	json(JSON),
	write_term_to_chars(JSON, [], Cs),
	!.
json_chars(JSON, Cs) :-
	string(Cs),
	read_term_from_chars(Cs, JSON, []),
	ground(JSON),
	once(json(JSON)).

json_value(JSON, Value) :-
	once(json_value_(JSON, Value)).

json_value_(true, boolean(true)).
json_value_(false, boolean(false)).
json_value_(Cs, string(Cs)) :- json_string(Cs).
json_value_(N, number(N)) :- json_number(N).
json_value_(K:V0, string(K)-V) :-
	once(json_value_(V0, V)),
	json_field(K:V0).
json_value_([], list([])).
json_value_(L0, list(L)) :-
	once(maplist(json_value_, L0, L)).
json_value_({}, pairs([])).
json_value_({Fields}, pairs(L)) :-
	is_list(L),
	once(maplist(json_value_, L0, L)),
	list_comma(L0, Fields).
json_value_({Fields}, pairs(L)) :-
	json_object({Fields}),
	list_comma(L0, Fields),
	once(maplist(json_value_, L0, L)).
json_value_(null, null).

json_bool(true).
json_bool(false).

json_string(Cs) :- string(Cs).

json_number(N) :- number(N).

json_field(K:V) :-
	json_string(K),
	once(json(V)).

json_object({}).
json_object({Fields}) :-
	apply_comma(json_field, Fields).

json_list(V) :- is_list(V).

json(V) :- V == null.
json(V) :- json_bool(V).
json(V) :- json_string(V).
json(V) :- json_number(V).
json(V) :- json_object(V).
json(V) :- json_list(V).

% predicates for manipulating comma lists (_, _) 

member_comma(X, (H,_)) :- member_comma(X, H).
member_comma(X, (_,T)) :- !, member_comma(X, T).
member_comma(X, X).

list_comma([H|T1], (H, T2)) :- list_comma(T1, T2).
list_comma([X], X).

apply_comma(Goal, X) :- apply_comma(Goal, _, X).
apply_comma(Goal, X, (H,_)) :- call(Goal, H), apply_comma(Goal, X, H).
apply_comma(Goal, X, (_,T)) :- !, call(Goal, T), apply_comma(Goal, X, T).
apply_comma(Goal, X, X) :- call(Goal, X).