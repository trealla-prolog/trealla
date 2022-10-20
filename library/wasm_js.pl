
:- module(wasm_js, [js_eval/1, js_eval/2, js_eval_json/2,
	js_fetch/3, http_consult/1, consult_string/1,
	crypto_data_hash/3]).

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(format)).
:- use_module(library(pseudojson)).

% Guest (Trealla) → Host (Javascript)

js_eval(Expr, Cs) :-
	'$host_call'(Expr, Cs), !
	; yield, '$host_resume'(Cs).

js_eval_json(Expr, Result) :-
	js_eval_(Expr, Result, js_eval_json/2).

js_eval(Expr) :-
	catch(
		js_eval_(Expr, _, js_eval/1),
		error(wasm_error(invalid_json, _), _),
		true
	).

js_eval_(Expr, Result, Context) :-
	(  js_eval(Expr, Cs)
	-> true
	;  throw(error(wasm_error(host_call_failed), Context))
	),
	(  json_chars(Result, Cs)
	-> true
	;  throw(error(wasm_error(invalid_json, Cs), Context))
	),
	throw_if_error_result(Result, Context).

throw_if_error_result({"$error": Error}, Ctx) :-
	nonvar(Error),
	throw(error(js_error(Error), Ctx)).
throw_if_error_result(_, _).

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
	js_eval_(Expr, Result, js_fetch/3),
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

crypto_data_hash(Data, Hash, Options) :-
	must_be(chars, Data),
	must_be(list, Options),
	ignore(member(algorithm(Algo), Options)),
	js_subtle_hash(Data, Hash, Algo).

hash_algo(sha256, "SHA-256").
hash_algo(sha384, "SHA-384").
hash_algo(sha512, "SHA-512").
hash_algo(sha1, "SHA-1").

js_subtle_hash(Data, Hash, Algo) :-
	(  hash_algo(Algo, AlgoCs)
	-> true
	;  domain_error(algorithm, Algo, crypto_data_hash/3)
	),
	subtle_digest_expr(Data, AlgoCs, Expr),
	js_eval_json(Expr, Hash).

subtle_digest_expr(Data, Algo, Expr) :-
	once(phrase(format_(
		"return crypto.subtle.digest(~q, new TextEncoder().encode(~q)).then(sum => [...new Uint8Array(sum)].map(c => c.toString(16).padStart(2, '0')).join(''));",
		[Algo, Data]), Expr)).

atom_string(X, X) :- string(X), !.
atom_string(A, X) :- atom_chars(A, X).
