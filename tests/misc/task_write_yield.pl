% Regression test for the task write TODO in write_all() (bif_streams.c):
% a task's own write to a backpressured socket used to just fail
% outright on the first EAGAIN, since resuming it re-runs this same bif
% call from scratch with the original, untouched Prolog arguments - no
% memory of how many bytes already went out. str->wbuf now supplies that
% memory: the remaining bytes get copied into it once, on the first
% stall, and do_yield_on_stream(..., true) parks the task until POLLOUT.
%
% The payload is four distinct blocks rather than one repeated byte, so
% a wrong resume point (duplicated or dropped range) shows up as a wrong
% block boundary, not just a wrong total length - matching bytes with
% the wrong count would still pass a length-only check.
%
% The client deliberately delays before reading, so the task's write is
% guaranteed to actually stall against a full send buffer, exercising
% the park/resume path rather than completing in one call.

:- use_module(library(socket)).
:- initialization(main).

pattern_string(N, S) :-
	Q is N // 4,
	format(string(A), "~*c", [Q, 0'A]),
	format(string(B), "~*c", [Q, 0'B]),
	format(string(C), "~*c", [Q, 0'C]),
	R is N - (Q*3),
	format(string(D), "~*c", [R, 0'D]),
	string_concat(A, B, AB),
	string_concat(AB, C, ABC),
	string_concat(ABC, D, S).

do_write(S, N) :-
	pattern_string(N, Payload),
	'$bwrite'(S, Payload).

run_server(N) :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3443), tcp_listen(Srv, 5),
	tcp_accept(Srv, Cl, _),
	tcp_open_socket(Cl, S),
	call_task(do_write(S, N)),
	wait,
	close(S),
	tcp_close_socket(Srv).

main :-
	N = 8000000,	% 8 MB - several times any default socket send buffer
	thread_create(run_server(N), T, []),
	sleep(0.2),
	tcp_socket(C), tcp_connect(C, '127.0.0.1':3443),
	tcp_open_socket(C, S),
	% Deliberately do not read for a while, so the task's write is
	% guaranteed to hit a full send buffer at least once.
	sleep(1.0),
	'$bread'(S, N, Got),
	pattern_string(N, Expect),
	close(S),
	thread_join(T),
	(   Got == Expect
	->  writeln('task_write_yield: all ok')
	;   ( string_length(Got, GL), string_length(Expect, EL),
	      format("task_write_yield: MISMATCH got_len=~d expect_len=~d~n", [GL, EL])
	    )
	).
