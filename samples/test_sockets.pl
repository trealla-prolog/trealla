:- use_module(library(sockets)).

:- initialization(main).

main :-
	thread_create(main1, T1, []),
	thread_create(main2, T2, []),
	thread_join(T1),
	thread_join(T2),
	true.

main1 :-
	socket_server_open(':8080', S, []),
	socket_server_accept(S, C, _, []),
	read_term(C, hello, []),
	write_term(C, world, [fullstop(true), nl(true)]),
	close(C),
	close(S).

main2 :-
	socket_client_open(inet(localhost,8080), C, []),
	write_term(C, hello, [fullstop(true), nl(true)]),
	read_term(C, world, []),
	close(C),
	writeln(ok).

