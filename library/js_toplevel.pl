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
:- use_module(library(dcgs)).
:- use_module(library(json)).

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	catch(
		read_term_from_chars(Query, [variable_names(Vars)], Input),
		Error,
		(
			write('\x2\\x3\'),
			write_result(error, Error),
			flush_output
		)
	),
	query(Query, Vars, Status, Solutions),
	write_result(Status, Solutions),
	flush_output.

write_result(success, Solutions0) :-
	maplist(solution_json, Solutions0, Solutions),
	once(phrase(json_chars(pairs([
		string("result")-string("success"),
		string("answers")-list(Solutions)
	])), JSON)),
	maplist(write, JSON), nl.

write_result(failure, _) :-
	once(phrase(json_chars(pairs([
		string("result")-string("failure")
	])), JSON)),
	maplist(write, JSON), nl.

write_result(error, Error0) :-
	term_json(Error0, Error),
	once(phrase(json_chars(pairs([
		string("result")-string("error"),
		string("error")-Error
	])), JSON)),
	maplist(write, JSON), nl.

query(Query, Vars, Status, Solutions) :-
	( setup_call_cleanup(
		write('\x2\'), % START OF TEXT
		catch(bagof(Vars, call(Query), Solutions), Error, true),
		write('\x3\')  % END OF TEXT
	) -> OK = true
	  ;  OK = false
	),  
	query_status(OK, Error, Status),
	(  nonvar(Error)
	-> Solutions = Error
	;  true
	).

query_status(_OK, Error, error) :- nonvar(Error), !.
query_status(true, _, success).
query_status(false, _, failure).

solution_json(Vars0, pairs(Vars)) :- maplist(var_json, Vars0, Vars).

var_json(Var0=Value0, string(Var)-Value) :-
	atom_chars(Var0, Var),
	term_json(Value0, Value).

term_json(Value0, string(Value)) :-
	atom(Value0),
	atom_chars(Value0, Value),
	!.
term_json(Value, string(Value)) :-
	string(Value),
	!.
term_json(Value, number(Value)) :-
	number(Value),
	!.
term_json(Value0, list(Value)) :-
	is_list(Value0),
	maplist(term_json, Value0, Value),
	!.
term_json(Value, pairs([string("functor")-string(Functor), string("args")-list(Args)])) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	atom_chars(Functor0, Functor),
	maplist(term_json, Args0, Args),
	!.
term_json(Value, pairs([string("var")-string("_")])) :-
	var(Value),
	!.
