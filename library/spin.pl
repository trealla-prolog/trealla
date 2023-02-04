:- module(spin, [http_handler/3, current_http_uri/1, current_http_method/1, current_http_body/1,
	current_http_param/2, current_http_header/2]).

:- dynamic(current_http_uri/1).
:- dynamic(current_http_method/1).
:- dynamic(current_http_body/1).

% param is currently unused?
% :- dynamic(current_http_param/2).

:- dynamic(current_http_header/2).

:- dynamic(http_handler/4).
:- multifile([http_handler/4]).

init :-
	map_create(_, [alias(http_headers)]),
	'$memory_stream_create'(_, [alias(http_body)]).

:- initialization(init).	

http_handle_request(URI, Method) :-
	assertz(current_http_uri(URI)),
	assertz(current_http_method(Method)),
	( try_consult(lib) ; try_consult(init) ),
	fail.
http_handle_request(URI, Method) :-
	% findall(K:V, current_http_param(K, V), Params),
	findall(K:V, current_http_header(K, V), Headers),
	(  current_http_body(Body)
	-> true
	;  Body = []
	),
	% get("/index.pl")
	Handle =.. [Method, URI/*, Params*/],
	http_handle_(Handle, Headers, Body, Status),
	map_set(http_headers, "status", Status).

http_handle_(Handle, Headers, Body, Status) :-
	catch(
		http_handler(Handle, Headers, Body, Status),
		Error,
		(
			Status = 500,
			write(http_body, 'Internal server error'),
			format(stderr, "Unhandled exception: ~w in ~w", [Error, Handle])
		)
	),
	!.
http_handle_(_, _, _, 404) :-
	write(http_body, 'Not found\n').

http_fetch(URL, Result, Options) :-
	must_be(chars, URL),
	setup_call_cleanup(
		(
			map_create(RequestMap, []),
			map_create(RequestHeaders, []),
			map_create(ResponseMap, []),
			map_create(ResponseHeaders, [])
		),
		http_fetch_(URL, Options, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders, Result),
		(
			map_close(RequestMap),
			map_close(RequestHeaders),
			map_close(ResponseMap),
			map_close(ResponseHeaders)
		)
	).

http_fetch_(URL, Options, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders, Result) :-
	outbound_request_options(Options, RequestMap, RequestHeaders),
	'$wasi_outbound_http'(URL, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders),
	spin_http_result(ResponseMap, ResponseHeaders, Result).

outbound_request_options(Options, Map, HeaderMap) :-
	( memberchk(method(Method), Options) ; Method = get ),
	must_be(atom, Method),
	map_set(Map, method, Method),
	(  memberchk(body(Body), Options), Body \= []
	-> must_be(chars, Body), map_set(Map, body, Body)
	;  true
	),
	( memberchk(headers(Headers), Options) ; Headers = [] ),
	must_be(list, Headers),
	maplist(map_set_kv(HeaderMap), Headers).

map_set_kv(Map, Key:Value) :-
	map_set(Map, Key, Value).

spin_http_result(Map, HeaderMap, response(Status, Headers, Body)) :-
	map_get(Map, status, Status),
	map_get(Map, body, Body0),
	atom_chars(Body0, Body),
	map_list(HeaderMap, Headers).

try_consult(Name) :-
	catch(consult(Name), error(existence_error(source_sink, _), _), fail).