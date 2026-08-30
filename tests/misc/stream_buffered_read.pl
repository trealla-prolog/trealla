% Regression test for a bug introduced by an earlier (reverted) attempt at
% the fix in stream_timeout.pl: polling the raw fd with poll() *before*
% every read() ignores that libc's stdio buffering can already hold the
% next byte in userspace after a single read() pulled a multi-byte burst
% off the wire. A socket stream with "abc\n" sitting in one TCP segment
% would get the first byte fine (the read that fills the buffer), then
% hang forever on the second, third and fourth get_char/2 calls: poll()
% sees the kernel socket buffer already drained and waits for more
% network traffic that is never coming, even though the bytes are already
% sitting in the stdio buffer waiting to be handed over. This broke a
% real downstream user - Logtalk's http_static_site example - the first
% time it shipped.
%
% The fix (see retry_getc()/tpl_getline() in bif_streams.c/network.c)
% always attempts the real read first and only waits on the fd after an
% actual EAGAIN, so already-buffered data is never second-guessed.

:- use_module(library(socket)).
:- use_module(library(iso_ext)).
:- initialization(main).

main :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3421), tcp_listen(Srv, 5),
	tcp_socket(Cl), tcp_connect(Cl, '127.0.0.1':3421),
	tcp_accept(Srv, Sl, _),
	tcp_open_socket(Cl, C),
	tcp_open_socket(Sl, S),

	% One write, one TCP segment, four bytes - then the peer sends nothing
	% else. Reading them one get_char/2 at a time must not touch the
	% network after the first call.
	format(S, "abc~n", []), flush_output(S),

	catch(
	    call_with_time_limit(2.0, (
	        get_char(C, Ch1), get_char(C, Ch2), get_char(C, Ch3), get_char(C, Ch4)
	    )),
	    E,
	    (format("UNEXPECTED TIMEOUT: ~q~n", [E]), halt(1))
	),

	(   [Ch1,Ch2,Ch3,Ch4] == ['a','b','c','\n']
	->  writeln('stream_buffered_read: all ok')
	;   format("stream_buffered_read: MISMATCH ~q~n", [[Ch1,Ch2,Ch3,Ch4]])
	),

	close(C), close(S), tcp_close_socket(Srv).
