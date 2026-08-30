% Regression test for the same busy-loop bug fixed in '$bread'/3 (see
% http_bread.pl), found in the Edinburgh-legacy redo/1,2 (bif_edin_redo_1/2
% in src/bif_streams.c): its char-skipping loop had no EAGAIN handling at
% all. A non-task socket is now always non-blocking (see bif_net.c), so a
% short, non-EOF read used to spin the loop at full CPU re-issuing
% xgetc_utf8_lax() until the target character showed up, instead of
% waiting for it.
%
% The write is delayed so the client's redo/1 call is guaranteed to see
% no data at all on its first read attempt and must wait rather than
% matching immediately - unlike sending everything up front, which would
% never touch the retry path.

:- use_module(library(socket)).
:- initialization(main).

run_server :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3426), tcp_listen(Srv, 5),
	tcp_accept(Srv, Cl, _),
	tcp_open_socket(Cl, S),
	sleep(0.3),
	format(S, "xxxZ", []),
	flush_output(S),
	close(S),
	tcp_close_socket(Srv).

main :-
	thread_create(run_server, T, []),
	sleep(0.1),
	tcp_socket(C), tcp_connect(C, '127.0.0.1':3426),
	tcp_open_socket(C, S),
	set_input(S),
	redo(0'Z),
	set_input(user_input),
	close(S),
	thread_join(T),
	writeln('edin_redo: all ok').
