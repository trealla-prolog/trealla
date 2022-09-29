/*
	This is a JSON toplevel that WASM ports can use to grab answers from Trealla programmatically.
	Very experimental and not in the upstream.

	Current format:
	ASCII START OF TEXT (0x02), stdout text as-is, then ASCII END OF TEXT (0x03), then a JSON response, then a line break.

	{
		"result": "success" | "failure" | "error",
		"answer": { "X": "<substitution for X>", ... },
		"error": "<throw/1 exception term>"
	}
*/
:- module(js_toplevel, [js_toplevel/0, js_ask/1]).

:- use_module(library(lists)).

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	catch(
		read_term_from_chars(Query, [variable_names(Vars)], Input),
		Error,
		(
			write('\x2\\x3\'),
			write_result(error, _, Error),
			flush_output
		)
	),
	catch(
		query(Query, Status),
		Error,
		Status = error
	),
	write('\x3\'),
	write_result(Status, Vars, Error),
	flush_output.

write_result(success, Vars, _) :-
	once(solution_json(Vars, Solution)),
	write({"result":"success", "answer":Solution}),
	nl.

write_result(failure, _, _) :-
	'$put_chars'("{\"result\":\"failure\"}\n").

write_result(error, Vars, Error0) :-
	once(term_json(Vars, Error0, Error)),
	write({"result":"error", "error":Error}),
	nl.

query(Query, Status) :-
	write('\x2\'),  % START OF TEXT
	(   call(Query)
	*-> Status = success
	;   Status = failure
	).

solution_json([], {}).
solution_json(Vars, {Subs}) :- foldl(solution_json_(Vars), Vars, [], Subs).
solution_json_(Vars, V0, [], V) :- sub_json(Vars, V0, V), !.
solution_json_(Vars, V0, Vs, (Vs, V)) :- sub_json(Vars, V0, V).

sub_json(Vars, Var0=Value0, Var:Value) :-
	atom_chars(Var0, Var),
	once(term_json_top(Vars, Value0, Value)).

term_json(_, Value, []) :- Value == [].

term_json(_, Value0, {"functor":Value}) :-
	atom(Value0),
	atom_chars(Value0, Value).

term_json(_, Value, Value) :- string(Value).

term_json(_, Value, {"stream":Value}) :- is_stream(Value).

term_json(_, Value, Value) :- number(Value).

term_json(Vars, Value0, Value) :-
	is_list(Value0),
	once(maplist(term_json(Vars), Value0, Value)).

term_json(Vars, Value, {"functor":Functor, "args":Args}) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	atom_chars(Functor0, Functor),
	once(maplist(term_json(Vars), Args0, Args)).

term_json(Vars, Value, {"var":Name}) :-
	var(Value),
	once(var_name(Vars, Value, Name)).

term_json(_, Value, {"blob":Cs}) :-
	write_term_to_chars(Value, [], Cs).

term_json_top(Vars, Value, {"var":Name, "attr":Attr}) :-
	var(Value),
	once(var_name(Vars, Value, Name)),
	attvar_json(Vars, Value, Attr).
term_json_top(Vars, Value, JS) :- term_json(Vars, Value, JS).

var_name([K=V|_], Var, Name) :-
	V == Var,
	atom_chars(K, Name).
var_name([_=V|Vs], Var, Name) :-
	V \== Var,
	var_name(Vs, Var, Name).
var_name([], _, "_").

attvar_json(Vars, Var, JS) :-
	copy_term(Var, _, Attr),
	once(term_json(Vars, Attr, JS)).
