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
:- module(js, [js_toplevel/0, js_ask/1, js_eval/2, js_eval_json/2, wasm_yield/0]).

:- use_module(library(lists)).
:- use_module(library(pseudojson)).

% Guest (Trealla) → Host (WASM)

js_eval_json(Expr, Result) :-
	(  js_eval(Expr, Cs)
	-> true
	;  throw(error(wasm_error(host_call_failed), js_eval_json/2))
	),
	(  json_chars(Result, Cs)
	-> true
	;  throw(error(wasm_error(invalid_json, Cs), js_eval_json/2))
	).
	% json_value(JSON, V),
	% term_json(_, Result, V).

js_eval(Expr, Cs) :-
	'$host_call'(Expr, Cs), !
	; wasm_yield, fail
	; '$host_resume'(Cs).

wasm_yield :-
	write(stdout, '\x16\'),
	flush_output(stdout),
	yield.

% Host (WASM) → Guest (Trealla)

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	catch(
		read_term_from_chars(Input, Query, [variable_names(Vars)]),
		Error,
		(
			write(stdout, '\x2\\x3\'),
			result_json(error, Vars, Error, JSON),
			write_result(JSON),
			flush_output(stdout)
		)
	),
	catch(
		query(Query, Status),
		Error,
		Status = error
	),
	write(stdout, '\x3\'),
	result_json(Status, Vars, Error, JSON),
	write_result(JSON),
	flush_output(stdout).

query(Query, Status) :-
	write(stdout, '\x2\'),  % START OF TEXT
	(   call(Query)
	*-> Status = success
	;   Status = failure
	).

write_result(JSON) :-
	json_value(JS, JSON),
	json_chars(JS, Cs),
	'$put_chars'(stdout, Cs),
	nl.

result_json(success, Vars, _, pairs([string("result")-string("success"), string("answer")-Solution])) :-
	once(solution_json(Vars, Solution)).
result_json(failure, _, _, pairs([string("result")-string("failure")])).
result_json(error, Vars, Error, pairs([string("result")-string("error"), string("answer")-ErrorJS])) :-
	once(term_json(Vars, Error, ErrorJS)).

solution_json([], pairs([])).
solution_json(Vars, pairs(Subs)) :- maplist(sub_json(Vars), Vars, Subs).

sub_json(Vars, Var0=Value0, string(Var)-Value) :-
	atom_chars(Var0, Var),
	once(term_json_top(Vars, Value0, Value)).

term_json(_, Value, list([])) :- Value == [].

term_json(_, Value0, pairs([string("functor")-string(Value)])) :-
	atom(Value0),
	atom_chars(Value0, Value).

term_json(_, Value, string(Value)) :- string(Value).

term_json(_, Value, number(Value)) :- number(Value).

term_json(Vars, Value, list(L)) :-
	is_list(Value),
	once(maplist(term_json(Vars), Value, L)).

term_json(Vars, Value, pairs([string("functor")-string(Functor), string("args")-list(Args)])) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	atom_chars(Functor0, Functor),
	once(maplist(term_json(Vars), Args0, Args)).

term_json(Vars, Value, pairs([string("var")-string(Name)])) :-
	var(Value),
	once(var_name(Vars, Value, Name)).

term_json(_, Value, pairs([string("stream")-number(-1)])) :-
	% TODO: grab alias/fd from stream
	is_stream(Value).

term_json(_, Value, pairs([string("blob")-string(Cs)])) :-
	write_term_to_chars(Value, [], Cs).

term_json_top(Vars, Value, pairs([string("var")-string(Name), string("attr")-Attr])) :-
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
	copy_term(Var, Var, Attr),
	once(term_json(Vars, Attr, JS)).
