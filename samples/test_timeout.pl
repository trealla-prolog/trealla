:- initialization(main3).

main1 :-
	thread_create(catch(call_with_time_limit(3.0, run1), _, writeln(catch1)), T1, []),
	thread_join(T1),
	writeln('done1').

run1 :-
	get_char(_).

main2 :-
	thread_create(catch(call_with_time_limit(1.0, run2(here1)), _, writeln(catch1)), T1, []),
	thread_create(catch(call_with_time_limit(2.0, run2(here2)), _, writeln(catch2)), T2, []),
	thread_join(T1),
	writeln('\tdone1'),
	thread_join(T2),
	writeln('\tdone2').

run2(Msg) :-
	repeat, writeln(Msg), sleep(0.25), fail.

:- use_module(library(sockets)).

server :-
	socket_server_open(':8080', S, []),
	socket_server_accept(S, C, _, []),
	writeln(server_delay),
	sleep(2.0),
	writeln([server_write,C]),
	write_term(C, xyz, [fullstop(true), nl(true)]),
	sleep(1.0),
	writeln([server_close,C,S]),
	close(C),
	close(S).

main3 :-
	thread_create(server, T, []),
	sleep(0.1),
	socket_client_open(inet(localhost,8080), C, []),
	writeln([client_read,C]),
	read_term(C, Term, []),
	writeln([client_got,Term]),
	writeln([client_close,C]),
	close(C),
	thread_join(T).
