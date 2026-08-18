% library(socket) - the SWI-compatible interface. Phases 2 and 3 of
% docs/socket-swi-design.md: address conversion, the handle lifecycle,
% and the TCP client and server paths.
%
% The client is checked against library(sockets)'s server rather than
% against itself, so a bug in the handle layer cannot cancel out. The
% server path is then checked against this library's own client, which
% by then has been independently verified.
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

    % server path, this library on both ends
    t(roundtrip, (tcp_socket(Sv), tcp_bind(Sv, '127.0.0.1':3402), tcp_listen(Sv, 5),
                  tcp_socket(Cl), tcp_connect(Cl, '127.0.0.1':3402),
                  tcp_open_socket(Cl, CS), format(CS, "ping~n", []), flush_output(CS),
                  tcp_accept(Sv, Slave, Peer), Peer == ip(127,0,0,1),
                  tcp_open_socket(Slave, SS), getline(SS, L3), L3 == "ping",
                  format(SS, "pong~n", []), flush_output(SS),
                  getline(CS, L4), L4 == "pong",
                  tcp_close_socket(Slave), tcp_close_socket(Cl), tcp_close_socket(Sv))),

    % an unbound port is reported back by the bind, as SWI does
    t(ephemeral, (tcp_socket(Se), tcp_bind(Se, '127.0.0.1':Port),
                  integer(Port), Port > 0,
                  tcp_socket(Ce), tcp_connect(Ce, '127.0.0.1':Port),
                  tcp_accept(Se, Sl2, _), tcp_close_socket(Sl2),
                  tcp_close_socket(Ce), tcp_close_socket(Se))),

    % failures name their cause and surface at the right predicate
    t(bind_in_use, (tcp_socket(B1), tcp_bind(B1, '127.0.0.1':3403),
                    tcp_socket(B2),
                    catch(tcp_bind(B2, '127.0.0.1':3403),
                          error(socket_error(eaddrinuse,_), tcp_bind/2), true),
                    tcp_close_socket(B2), tcp_close_socket(B1))),
    t(connect_refused, (tcp_socket(R),
                    catch(tcp_connect(R, '127.0.0.1':3499),
                          error(socket_error(econnrefused,_), tcp_connect/2), true),
                    tcp_close_socket(R))),

    % the phase machine refuses out-of-order use
    t(listen_unbound, (tcp_socket(P1),
                    catch(tcp_listen(P1, 5), error(permission_error(listen,_,_),_), true),
                    tcp_close_socket(P1))),
    t(accept_unbound, (tcp_socket(P2),
                    catch(tcp_accept(P2,_,_), error(permission_error(accept,_,_),_), true),
                    tcp_close_socket(P2))),
    t(rebind, (tcp_socket(P3), tcp_bind(P3, '127.0.0.1':3404),
                    catch(tcp_bind(P3, '127.0.0.1':3405), error(permission_error(bind,_,_),_), true),
                    tcp_close_socket(P3))),

    (  saw_failure
    -> format("socket: FAILURES above~n")
    ;  format("socket: all ok~n")
    ).
