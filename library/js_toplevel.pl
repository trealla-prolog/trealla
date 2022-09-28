/*
	This is a JSON toplevel that WASM ports can use to grab answers from Trealla programmatically.
	Very experimental and not in the upstream.

	Current format:
	ASCII START OF TEXT (0x02), stdout text as-is, then ASCII END OF TEXT (0x03), then a JSON response

	{
		"result": "success" | "failure" | "error",
		"answers": [{ "X": "<substitution for X>", ... }, ...],
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

write_result(success, Solution0, _) :-
	solution_json(Solution0, Solution),
	write({"result":"success", "answer":Solution}),
	nl.

write_result(failure, _, _) :-
	'$put_chars'("{\"result\":\"failure\"}\n").

write_result(error, _, Error0) :-
	term_json(Error0, Error),
	write({"result":"error", "error":Error}),
	nl.

query(Query, Status) :-
	write('\x2\'),  % START OF TEXT
	(   call(Query)
	*-> Status = success
	;   Status = failure
	).  % END OF TEXT

solution_json([], {}) :- !.
solution_json(Vs, {Vars}) :- foldl(solution_json_, Vs, [], Vars).
solution_json_(V0, [], V) :- var_json(V0, V), !.
solution_json_(V0, Vs, (Vs, V)) :- var_json(V0, V).

var_json(Var0=Value0, Var:Value) :-
	atom_chars(Var0, Var),
	term_json(Value0, Value).

term_json(Value0, Value) :-
	atom(Value0),
	atom_chars(Value0, Value),
	!.
term_json(Value, Value) :-
	string(Value),
	!.
term_json(Value, Value) :-
	number(Value),
	!.
term_json(Value0, Value) :-
	is_list(Value0),
	maplist(term_json, Value0, Value),
	!.
term_json(Value, {"functor":Functor, "args":Args}) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	atom_chars(Functor0, Functor),
	maplist(term_json, Args0, Args),
	!.
term_json(Value, {"var":"_"}) :-
	var(Value),
	!.
