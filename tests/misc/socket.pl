% library(socket) - the SWI-compatible interface. Phase 2 of
% docs/socket-swi-design.md: address conversion, the handle lifecycle,
% and the TCP client path.
%
% The client is checked against library(sockets)'s server rather than
% against itself, so a bug in the handle layer cannot cancel out.
%
% Ports are fixed and in the 34xx range; if the suite ever runs
% concurrently with itself these will collide.

:- use_module(library(socket)).
:- use_module(library(sockets)).
:- initialization(main).

:- dynamic(saw_failure/0).

t(L, G) :-
	(  catch(G, E, (R = err(E)))
	-> (var(R) -> R = ok ; true)
	;  R = failed
	),
	(  R == ok
	-> true
	;  format("SOCKET-FAIL ~w: ~q~n", [L, R]),
	   (  saw_failure -> true ; assertz(saw_failure) )
	).

main :-
    % address conversion
    t(ip4_name,   (ip_name(ip(127,0,0,1), N1), N1 == '127.0.0.1')),
    t(name_to_ip, (ip_name(I2, '10.0.0.7'), I2 == ip(10,0,0,7))),
    t(gethostname,(gethostname(H), atom(H))),
    t(host_to_addr,(tcp_host_to_address(localhost, A), nonvar(A))),

    % handle lifecycle without any I/O
    t(socket_create, (tcp_socket(S1), S1 = '$socket'(_))),
    t(close_fresh,   (tcp_socket(S2), tcp_close_socket(S2))),
    t(open_fresh_err,(tcp_socket(S3), catch(tcp_open_socket(S3,_), error(permission_error(_,_,_),_), true))),
    t(bad_socket,    catch(tcp_open_socket(not_a_socket,_), error(type_error(socket,_),_), true)),
    t(setopt_ok,     (tcp_socket(S4), tcp_setopt(S4, reuseaddr), tcp_setopt(S4, nodelay), tcp_close_socket(S4))),
    t(setopt_refused,(tcp_socket(S5), catch(tcp_setopt(S5, broadcast), error(domain_error(socket_option,_),_), true), tcp_close_socket(S5))),

    % real connect to a sockets.pl server
    socket_server_open(3401, Srv, []),
    t(connect,   (tcp_socket(C), tcp_connect(C, '127.0.0.1':3401),
                  tcp_open_socket(C, Str), format(Str, "hi~n", []), flush_output(Str),
                  socket_server_accept(Srv, _Client, In, []),
                  getline(In, Line), Line == "hi",
                  tcp_getopt(C, file_no(FD)), integer(FD),
                  tcp_close_socket(C), close(In))),
    t(connect3,  (tcp_connect('127.0.0.1':3401, P, []),
                  format(P, "yo~n", []), flush_output(P),
                  socket_server_accept(Srv, _C2, In2, []),
                  getline(In2, L2), L2 == "yo", close(P), close(In2))),
    socket_server_close(Srv),
    (  saw_failure
    -> format("socket: FAILURES above~n")
    ;  format("socket: all ok~n")
    ).
