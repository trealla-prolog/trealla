:- use_module(library(sockets)).

:- initialization((main1,main3,main4)).

main1 :-
	thread_create(main11, T1, []),
	thread_create(main12, T2, []),
	thread_join(T1),
	thread_join(T2),
	writeln(done),
	true.

main11 :-
	socket_server_open(8080, S, []),
	socket_server_accept(S, _, C, []),
	read_term(C, hello, []),
	write_term(C, world, [fullstop(true), nl(true)]),
	close(C),
	close(S).

main12 :-
	socket_client_open(localhost:8080, C, []),
	write_term(C, hello, [fullstop(true), nl(true)]),
	read_term(C, T, []),
	T = world,
	close(C),
	writeln(ok).

:- use_module(library(sockets)).

% main3/main4 run a server and a client in two threads. This test used
% to writeln/1 from inside each thread as it went and compare against a
% fixed transcript, which asserted two things the sockets code does not
% promise:
%
%   1. A PARTICULAR INTERLEAVING of two independent threads. Three
%      pairs were racy: server_delay vs client_read (the client can
%      connect and print before the server prints at all), client_read
%      vs server_accepted, and server_close vs client_got (once the
%      write flushes, both sides run independently). Passed on an idle
%      machine, failed 10 runs out of 10 under CPU contention - which
%      is how it surfaced, during a suite run on a loaded box.
%
%   2. ABSOLUTE STREAM NUMBERS. Whether main4's client gets stream 3 or
%      4 depends on whether main3's streams have been closed and
%      recycled yet. Also timing.
%
% Each thread now buffers its own lines and main prints them in blocks
% after the joins: ordering within a thread is still checked, ordering
% across threads is no longer asserted. Streams are logged under fixed
% names rather than handles.
%
% Real stream aliases would be the natural way to write that last part,
% but `alias(+Alias)` is documented in library/sockets.pl and not
% implemented - bif_net.c fills the alias slot with the hostname and
% never reads the option, so close(my_alias) raises
% existence_error(stream, my_alias). Nothing is lost here by naming
% them in the test: each log line uses the same Prolog variable that
% the socket call did, so printing the handle only ever confirmed that
% a variable equals itself.

:- dynamic(slog/1).
:- dynamic(clog/1).

srv(X) :- assertz(slog(X)).
cli(X) :- assertz(clog(X)).

log_reset :- retractall(slog(_)), retractall(clog(_)).

dump :-
	forall(slog(X), writeln(X)),
	forall(clog(X), writeln(X)).

server3(S) :-
	srv(server_delay),
	socket_server_accept(S, _, C, []),
	srv(server_accepted),
	srv([server_write,srv_conn,xyz]),
	write_term(C, xyz, [fullstop(true), nl(true)]),
	srv([server_close,srv_conn,srv_sock]),
	close(C),
	close(S).

client3 :-
	socket_client_open(localhost:8080, C, []),
	cli([client_read,cli_conn]),
	read_term(C, Term, []),
	cli([client_got,Term]),
	cli([client_close,cli_conn]),
	close(C).

main3 :-
	writeln('main3...'),
	log_reset,
	socket_server_open(8080, S, []),
	thread_create(server3(S), T1, []),
	thread_create(client3, T2, []),
	thread_join(T1),
	thread_join(T2),
	dump.

server4(S) :-
	srv(server_delay),
	socket_server_accept(S, _, C, [type(binary)]),
	srv(server_accepted),
	Term = 0'x,
	srv([server_write,srv_conn,Term]),
	put_byte(C, Term),
	srv([server_close,srv_conn,srv_sock]),
	close(C),
	close(S).

client4 :-
	socket_client_open(localhost:8080, C, [type(binary)]),
	cli([client_read,cli_conn]),
	get_byte(C, Term),
	Term = 0'x,
	cli([client_got,Term]),
	cli([client_close,cli_conn]),
	close(C).

main4 :-
	writeln('main4...'),
	log_reset,
	socket_server_open(8080, S, []),
	thread_create(server4(S), T1, []),
	thread_create(client4, T2, []),
	thread_join(T1),
	thread_join(T2),
	dump.
