:- use_module(library(sockets)).

:- initialization(main2).

main1 :-
	thread_create(main11, T1, []),
	thread_create(main12, T2, []),
	thread_join(T1),
	thread_join(T2),
	writeln(done),
	true.

main11 :-
	socket_server_open(':8080', S, []),
	socket_server_accept(S, C, _, []),
	read_term(C, hello, []),
	write_term(C, world, [fullstop(true), nl(true)]),
	sleep(0.1),
	close(C),
	close(S).

main12 :-
	socket_client_open(inet(localhost,8080), C, []),
	write_term(C, hello, [fullstop(true), nl(true)]),
	read_term(C, T, []),
	T = world,
	close(C),
	writeln(ok).

main2 :-
	thread_create(main21, T1, []),
	thread_create(main22, T2, []),
	thread_join(T1),
	thread_join(T2),
	writeln(done),
	true.

main21 :-
	socket_server_open(':8080', S, []),
	socket_server_accept(S, C, _, []),
	catch(call_with_time_limit(1.0, read_term(C, _, [])), E, true),
	write_term(C, E, [fullstop(true), nl(true)]),
	close(C),
	close(S).

main22 :-
	socket_client_open(inet(localhost,8080), C, []),
	read_term(C, E, []),
	close(C),
	writeln(E).

