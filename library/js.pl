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
:- module(js, [js_toplevel/0, js_ask/1, js_eval/2, js_eval_json/2, js_fetch/3, http_consult/1]).

:- use_module(library(lists)).
:- use_module(library(pseudojson)).
:- use_module(library(errors)).

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
	; yield, fail
	; '$host_resume'(Cs).

% TODO: form encoding
% TODO: content-type negotiation
js_fetch(URL, Result, Opts) :-
	must_be(chars, URL),
	( memberchk(as(As), Opts) -> true ; As = string ),
	( memberchk(method(Method), Opts) -> true ; Method = get ),
	( memberchk(body(Body), Opts) -> true ; Body = '' ),
	( memberchk(headers(Hdrs), Opts) -> true ; Hdrs = [] ),
	( fetch_expr(URL, As, Method, Body, Hdrs, Expr) -> true
	; domain_error(fetch, Opts, js_fetch/3)),
	js_eval_json(Expr, Result),
	!.

fetch_expr(URL, As, Method, Body, Hdr, Expr) :-
	fetch_then(As, Then),
	fetch_obj(Method, Body, Hdr, Obj),
	once(phrase(format_("return fetch(~q,~w).then(x => x.~a());", [URL, Obj, Then]), Expr)).

fetch_obj(Method, Body, L0, Obj) :-
	atom_string(Method, Ms0),
	string_upper(Ms0, Ms),
	maplist(fetch_header, L0, L),
	json_value(HdrJS, pairs(L)),
	once(body_js(Body, BodyJS)),
	(  no_body(Ms)
	-> Obj = {"method":Ms, "headers":HdrJS}
	;  Obj = {"method":Ms, "headers":HdrJS, "body":BodyJS}
	).

fetch_header(K0-V0, string(Ks)-string(Vs)) :- atom_string(K0, Ks), atom_string(V0, Vs).

fetch_then(string, text).
fetch_then(json, json).

body_js('', undefined).
body_js(JS, Cs) :- json_chars(JS, Cs).

no_body("GET").
no_body("HEAD").

% TODO: set module name to URL
http_consult(URL) :-
	( js_fetch(URL, Cs, [as(string)]) -> true
	; throw(error(js_error(fetch_failed, URL)))),
	consult_string(Cs).

% Host (WASM) → Guest (Trealla)

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	catch(
		read_term_from_chars(Input, Query, [variable_names(Vars)]),
		Error,
		(
			write(stdout, '\u0002\u0003'),
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
	write(stdout, '\u0003'),
	result_json(Status, Vars, Error, JSON),
	write_result(JSON),
	flush_output(stdout).

query(Query, Status) :-
	write(stdout, '\u0002'),  % START OF TEXT
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

term_json(_, Value, number(Value)) :- float(Value).
term_json(_, Value, number(Value)) :-
	number(Value),
	% safe value range for JS integers
	Value =< 9007199254740991,
	Value >= -9007199254740991.
term_json(_, Value, pairs([string("number")-string(Cs)])) :-
	number(Value),
	number_chars(Value, Cs).

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

atom_string(X, X) :- string(X), !.
atom_string(A, X) :- atom_chars(A, X).

consult_string(Cs) :-
	wall_time(T),
	random(Rand),
	once(phrase(format_("/tmp/consult_~w~w.pl", [T, Rand]), Filename)),
	atom_chars(File, Filename),
	setup_call_cleanup(
		open(File, write, S),
		(
			'$put_chars'(S, Cs),
			flush_output(S),
			close(S),
			consult(File)
		),
		delete_file(Filename)
	).