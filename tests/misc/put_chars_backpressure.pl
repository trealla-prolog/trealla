% Regression test for '$put_chars'/1,2 (bif_streams.c), which backs
% library(pio)'s phrase_to_stream/2: unlike '$bwrite'/2 (see
% stream_write_backpressure.pl), it did a single tpl_write() call with no
% retry loop at all, so any short write - not just a full backpressure
% stall - silently dropped the unwritten remainder. Both are now the same
% shared write_all() helper in bif_streams.c.
%
% The client deliberately delays before reading, forcing the server's
% phrase_to_stream/2 write to stall against a full kernel send buffer at
% least once - unless the payload is bigger than the buffer and the stall
% is real, this test would pass either way and prove nothing.

:- use_module(library(socket)).
:- use_module(library(pio)).
:- initialization(main).

xs([]) --> [].
xs([x|T]) --> [x], xs(T).

run_server(N) :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3436), tcp_listen(Srv, 5),
	tcp_accept(Srv, Cl, _),
	tcp_open_socket(Cl, S),
	length(L, N),
	phrase_to_stream(xs(L), S),
	close(S),
	tcp_close_socket(Srv).

main :-
	N = 8000000,	% 8 MB - several times any default socket send buffer
	thread_create(run_server(N), T, []),
	sleep(0.2),
	tcp_socket(C), tcp_connect(C, '127.0.0.1':3436),
	tcp_open_socket(C, S),
	% Deliberately do not read for a while, so the server's write is
	% guaranteed to hit a full send buffer at least once.
	sleep(1.0),
	'$bread'(S, N, Got0),
	string_length(Got0, Got),
	close(S),
	thread_join(T),
	(   Got == N
	->  writeln('put_chars_backpressure: all ok')
	;   format("put_chars_backpressure: MISMATCH got ~d expected ~d~n", [Got, N])
	).
