% Regression test for a bug found while auditing the write side after
% fixing the read-side EAGAIN gaps (see stream_timeout.pl,
% stream_buffered_read.pl, http_bread.pl, edin_redo.pl): '$bwrite'/2 and
% format/3's stream-write loop (bif_streams.c, bif_format.c) checked
% ferror()/feof() on str->fp - the *read*-side FILE* - instead of
% str->fp_out, the one tpl_write() actually writes through (see the
% separate fdopen() calls in bif_net.c). Since nothing ever reads on that
% FILE* during a write, the check was always false, so the branch that
% should notice backpressure and wait was dead code.
%
% That went unnoticed while sockets stayed blocking for a non-task query:
% a full send buffer just blocked fwrite() inside the kernel, so a false
% (zero-progress) return from tpl_write() essentially never happened. Once
% non-task sockets went non-blocking (see bif_net.c), a full send buffer
% instead made tpl_write() return 0 with EAGAIN - and because the ferror
% check never saw it, the loop above just spun calling tpl_write() again
% immediately, tens of millions of times, until the peer drained enough to
% finish. Confirmed empirically: reverting the str->fp_out fix while
% keeping non-blocking sockets turned a payload send with a slow reader
% into ~22 million syscalls and ~90% CPU for the whole transfer, versus
% about a dozen syscalls and near-zero CPU once fixed.
%
% The client deliberately delays before reading, forcing the server's
% write to stall against a full kernel send buffer at least once - unless
% the payload is bigger than the buffer and the stall is real, this test
% would pass either way and prove nothing.

:- use_module(library(socket)).
:- initialization(main).

run_server(N) :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3432), tcp_listen(Srv, 5),
	tcp_accept(Srv, Cl, _),
	tcp_open_socket(Cl, S),
	format(atom(Payload), "~*c", [N, 0'x]),
	'$bwrite'(S, Payload),
	close(S),
	tcp_close_socket(Srv).

main :-
	N = 8000000,	% 8 MB - several times any default socket send buffer
	thread_create(run_server(N), T, []),
	sleep(0.2),
	tcp_socket(C), tcp_connect(C, '127.0.0.1':3432),
	tcp_open_socket(C, S),
	% Deliberately do not read for a while, so the server's write is
	% guaranteed to hit a full send buffer at least once.
	sleep(1.0),
	'$bread'(S, N, Got0),
	string_length(Got0, Got),
	close(S),
	thread_join(T),
	(   Got == N
	->  writeln('stream_write_backpressure: all ok')
	;   format("stream_write_backpressure: MISMATCH got ~d expected ~d~n", [Got, N])
	).
