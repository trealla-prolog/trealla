:- module(spin, [http_handler/3]).

:- dynamic(http_header/2).
:- dynamic(http_body/1).

:- dynamic(http_handler/3).
:- multifile([http_handler/3]).

init :-
	map_create(_, [alias(http_headers)]),
	'$memory_stream_create'(_, [alias(http_body)]).

:- initialization(init).	

http_handle_request(_, Method) :-
	assertz(http_method(Method)),
	(try_consult(lib) ; try_consult(init)),
	fail.
http_handle_request(URI, Method) :-
	http_handle_(Method, URI, Status),
	map_set(http_headers, "status", Status).
	
http_handle_(Method, URI, Status) :-
	http_handler(Method, URI, Status),
	!.
http_handle_(_, _, 404) :-
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

% todo: change to K:V?
map_set_kv(Map, Key-Value) :-
	map_set(Map, Key, Value).

spin_http_result(Map, HeaderMap, response(Status, Headers, Body)) :-
	map_get(Map, status, Status),
	map_get(Map, body, Body0),
	atom_chars(Body0, Body),
	map_list(HeaderMap, Headers).

try_consult(Name) :-
	catch(consult(Name), error(existence_error(source_sink, _), _), fail).