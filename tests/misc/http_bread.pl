% Regression test for a self-inflicted bug found while fixing the
% get_char/getline family (see stream_timeout.pl, stream_buffered_read.pl):
% making every non-task socket non-blocking (bif_net.c) exposed
% '$bread'/3 (src/bif_streams.c, backs library(http)'s Content-Length and
% chunked body reads - see read_body/3, read_chunks/3 in library/http.pl)
% to the exact same EAGAIN-vs-EOF ambiguity, except its fixed-length read
% loop had no wait at all: a short, non-EOF tpl_read() on a non-task query
% just looped straight back to the top and reissued tpl_read() again,
% spinning at full CPU instead of waiting for more of the body to arrive.
%
% This sends the response headers and body in two separate writes with a
% real gap between them, so the client's '$bread'/3 call is guaranteed to
% see the body only partially (in fact not at all yet) on its first read
% attempt and must wait rather than immediately succeeding - unlike
% sending the whole response in one write, which would let a single
% tpl_read() satisfy the whole Content-Length without ever exercising the
% retry path.

:- use_module(library(socket)).
:- use_module(library(http)).
:- use_module(library(iso_ext)).
:- initialization(main).

drain_headers(S) :-
	getline(S, Line),
	(   (Line == "\r" ; Line == "" ; Line == '' ; Line == [])
	->  true
	;   drain_headers(S)
	).

run_server :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3425), tcp_listen(Srv, 5),
	tcp_accept(Srv, Cl, _),
	tcp_open_socket(Cl, S),
	drain_headers(S),
	format(S, "HTTP/1.1 200 OK\r~nContent-Length: 20\r~n\r~n", []),
	flush_output(S),
	sleep(0.3),
	format(S, "~s", ["0123456789abcdefghij"]),
	flush_output(S),
	close(S),
	tcp_close_socket(Srv).

main :-
	thread_create(run_server, T, []),
	sleep(0.1),
	catch(
	    call_with_time_limit(10.0, http_get("http://127.0.0.1:3425/", Data, [])),
	    E,
	    (format("http_bread: UNEXPECTED TIMEOUT ~q~n", [E]), halt(1))
	),
	thread_join(T),
	(   Data == "0123456789abcdefghij"
	->  writeln('http_bread: all ok')
	;   format("http_bread: MISMATCH ~q~n", [Data])
	).
