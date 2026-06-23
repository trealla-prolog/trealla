:- use_module(library(sockets)).

:- initialization(main1).

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
	close(C),
	close(S).

main12 :-
	socket_client_open(inet(localhost,8080), C, []),
	write_term(C, hello, [fullstop(true), nl(true)]),
	read_term(C, T, []),
	T = world,
	close(C),
	writeln(ok).

