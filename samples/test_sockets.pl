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

server3(S) :-
	writeln([server_delay,S]),
	socket_server_accept(S, C, _, []),
	writeln([server_accepted,S,C]),
	writeln([server_write,C,xyz]),
	write_term(C, xyz, [fullstop(true), nl(true)]),
	writeln([server_close,C]),
	close(C),
	writeln([server_close,S]),
	close(S).

client3 :-
	socket_client_open(inet(localhost,8080), C, []),
	writeln([client_read,C]),
	read_term(C, Term, []),
	writeln([client_got,Term]),
	writeln([client_close,C]),
	close(C).

main3 :-
	writeln('main3...'),
	socket_server_open(':8080', S, []),
	thread_create(server3(S), T1, []),
	thread_create(client3, T2, []),
	thread_join(T1),
	thread_join(T2).

server4(S) :-
	writeln(server_delay),
	socket_server_accept(S, C, _, [type(binary)]),
	writeln(server_accepted),
	Term = 0'x,
	writeln([server_write,C,Term]),
	put_byte(C, Term),
	writeln([server_close,C,S]),
	close(C),
	close(S).

client4 :-
	socket_client_open(inet(localhost,8080), C, [type(binary)]),
	writeln([client_read,C]),
	get_byte(C, Term),
	Term = 0'x,
	writeln([client_got,Term]),
	writeln([client_close,C]),
	close(C).

main4 :-
	writeln('main4...'),
	socket_server_open(':8080', S, []),
	thread_create(server4(S), T1, []),
	thread_create(client4, T2, []),
	thread_join(T1),
	thread_join(T2).
