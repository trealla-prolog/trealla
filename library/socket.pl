/** SWI-Prolog compatible socket interface.

Emulates the interface documented at
<https://www.swi-prolog.org/pldoc/man?section=socket> on top of Trealla's
networking builtins. See docs/socket-swi-design.md for the full design.

Note that `library(sockets)` (plural) also exists and is unrelated: it is
Trealla's own, Scryer-shaped socket library. Both drive the same builtins;
they differ only in the interface they present. Use whichever suits.

## Differences from SWI worth knowing

  * A Socket is a Prolog-side handle, not an OS handle. Trealla has no bif
    that creates an unbound, unconnected socket, so `tcp_socket/1` records
    intent and the real socket appears later, at `tcp_connect/2` or
    `tcp_bind/2`.

  * The underlying bif binds and listens in one step, so `tcp_bind/2` does
    both and `tcp_listen/2` only validates. Bind errors and the ephemeral
    port therefore surface at `tcp_bind/2`, which is where SWI puts them.
    The one visible difference is that a socket bound but never listened
    is nonetheless listening, where SWI would refuse connections to it.
    `BackLog` is ignored - the underlying layer hardcodes `SOMAXCONN`.

  * Trealla's socket streams are bidirectional, so a "stream pair" here is
    one stream. `tcp_open_socket/3` returns the same stream twice. Code
    that splits a pair with `stream_pair/3` will not work; code that uses
    the pair directly with `format/3`, `read_term/3` and `close/1` will.
*/

:- module(socket, [
	tcp_socket/1,
	udp_socket/1,
	unix_domain_socket/1,
	socket_create/2,
	tcp_bind/2,
	tcp_listen/2,
	tcp_accept/3,
	tcp_connect/2,
	tcp_connect/3,
	tcp_connect/4,
	tcp_open_socket/2,
	tcp_open_socket/3,
	tcp_close_socket/1,
	tcp_setopt/2,
	tcp_getopt/2,
	tcp_host_to_address/2,
	gethostname/1,
	ip_name/2
	]).

:- use_module(library(error)).
:- use_module(library(lists)).

% --- handle state ------------------------------------------------------
%
% '$socket'(Id) is opaque to callers. The state lives here rather than in
% the term so that connecting mutates the socket, as it does in SWI.
%
% Phase is one of:
%
%   fresh              created; no OS socket exists yet
%   connected(Stream)  '$client' has run, or this came from an accept
%   listening(Stream)  '$server' has run
%   closed             torn down
%
% Nothing here is thread-safe yet; sockets created concurrently on
% different threads would race on the counter. Single-threaded use is
% correct. See docs/socket-swi-design.md §9.

:- dynamic('$sock'/4).
:- dynamic('$sock_counter'/1).

'$sock_counter'(0).

'$new_id'(Id) :-
	retract('$sock_counter'(Id0)),
	Id is Id0 + 1,
	assertz('$sock_counter'(Id)).

'$sock_get'(Socket, Domain, Type, Phase) :-
	(  nonvar(Socket), Socket = '$socket'(Id)
	-> true
	;  var(Socket)
	-> throw(error(instantiation_error, socket/2))
	;  throw(error(type_error(socket, Socket), socket/2))
	),
	(  '$sock'(Id, Domain, Type, Phase)
	-> true
	;  throw(error(existence_error(socket, Socket), socket/2))
	).

'$sock_set'(Socket, Phase) :-
	Socket = '$socket'(Id),
	retract('$sock'(Id, D, T, _)),
	assertz('$sock'(Id, D, T, Phase)).

% The stream behind a socket, whatever way it was materialised.

'$sock_stream'(Socket, Stream) :-
	'$sock_get'(Socket, _, _, Phase),
	(  Phase = connected(Stream) -> true
	;  Phase = listening(Stream) -> true
	;  Phase == closed
	-> throw(error(existence_error(stream, Socket), socket/2))
	;  throw(error(permission_error(access, socket, Socket), socket/2))
	).

% --- errors ------------------------------------------------------------
%
% SWI reports error(socket_error(Code, Message), _). Trealla's builtins
% raise ordinary ISO errors and in places simply fail, so the errno is
% usually not recoverable - we can wrap what we are given but cannot
% invent a code we were never told. An operation that merely fails
% becomes socket_error(unknown, ...) rather than silently failing, since
% SWI callers expect an exception.

'$sock_call'(Goal, Context) :-
	(  catch(Goal, E, '$sock_rethrow'(E, Context))
	-> true
	;  throw(error(socket_error(unknown, 'operation failed'), Context))
	).

% The builtins put the culprit where SWI puts a message, so the message
% is rederived from the code and the context is the library predicate the
% caller actually invoked.

'$sock_rethrow'(error(socket_error(C,M), Ctx), Context) :- !,
	(  '$errmsg'(C, Msg)
	-> throw(error(socket_error(C,Msg), Context))
	;  throw(error(socket_error(C,M), Ctx))
	).
'$sock_rethrow'(error(existence_error(_, _), _), Context) :- !,
	throw(error(socket_error(enoent, 'no such host or file'), Context)).
'$sock_rethrow'(E, _) :-
	throw(E).

'$errmsg'(eaddrinuse,    'address already in use').
'$errmsg'(eaddrnotavail, 'cannot assign requested address').
'$errmsg'(eafnosupport,  'address family not supported').
'$errmsg'(eacces,        'permission denied').
'$errmsg'(econnrefused,  'connection refused').
'$errmsg'(econnreset,    'connection reset by peer').
'$errmsg'(ehostunreach,  'no route to host').
'$errmsg'(enetunreach,   'network is unreachable').
'$errmsg'(etimedout,     'connection timed out').
'$errmsg'(epipe,         'broken pipe').
'$errmsg'(eagain,        'resource temporarily unavailable').
'$errmsg'(einval,        'invalid argument').
'$errmsg'(emfile,        'too many open files').
'$errmsg'(enfile,        'too many open files in system').
'$errmsg'(enoent,        'no such host or file').
'$errmsg'(eisconn,       'socket is already connected').
'$errmsg'(unknown,       'operation failed').

% --- addresses ---------------------------------------------------------
%
% Converts SWI's address terms into the Host:Port atom the builtins take.
%
%   ip(A,B,C,D)             IPv4
%   ip(A,B,C,D,E,F,G,H)     IPv6
%   Host:Port               hostname or IP text, plus port
%   Port                    integer, meaning "any interface"

'$addr_atom'(Addr, _) :-
	var(Addr), !,
	throw(error(instantiation_error, socket/2)).
'$addr_atom'(Host:Port, Atom) :- !,
	'$host_atom'(Host, H),
	must_be(integer, Port),
	atomic_list_concat([H, ':', Port], Atom).
'$addr_atom'(Port, Atom) :-
	integer(Port), !,
	atomic_list_concat([':', Port], Atom).
'$addr_atom'(Addr, Addr) :-
	atom(Addr), !.
'$addr_atom'(Addr, _) :-
	throw(error(domain_error(socket_address, Addr), socket/2)).

'$host_atom'(H, H) :- atom(H), !.
'$host_atom'(ip(A,B,C,D), H) :- !,
	atomic_list_concat([A,'.',B,'.',C,'.',D], H).
'$host_atom'(ip(A,B,C,D,E,F,G,I), H) :- !,
	maplist('$hex4', [A,B,C,D,E,F,G,I], Hs),
	atomic_list_concat(Hs, ':', H).
'$host_atom'(H, _) :-
	throw(error(domain_error(ip_address, H), socket/2)).

'$hex4'(N, H) :- must_be(integer, N), format(atom(H), "~16r", [N]).

%% ip_name(?IP, ?Name).
%
% Between an `ip/4` or `ip/8` term and its textual form.

ip_name(IP, Name) :-
	nonvar(IP), !,
	'$host_atom'(IP, Name).
ip_name(IP, Name) :-
	must_be(atom, Name),
	'$split_dots'(Name, Parts),
	Parts = [_,_,_,_],
	'$atom_nums'(Parts, [A,B,C,D]), !,
	IP = ip(A,B,C,D).
ip_name(_, Name) :-
	throw(error(domain_error(ip_address, Name), ip_name/2)).

% Written out rather than using atomic_list_concat/3 in split mode
% (Trealla does not support that mode) or a yall lambda (not available
% unless the caller has loaded it).

'$split_dots'(Atom, Parts) :-
	atom_chars(Atom, Cs),
	'$sd'(Cs, [], Parts).

'$sd'([], Acc, [P]) :- !, '$rev_atom'(Acc, P).
'$sd'(['.'|T], Acc, [P|Ps]) :- !, '$rev_atom'(Acc, P), '$sd'(T, [], Ps).
'$sd'([C|T], Acc, Ps) :- '$sd'(T, [C|Acc], Ps).

'$rev_atom'(Acc, A) :- reverse(Acc, Cs), atom_chars(A, Cs).

'$atom_nums'([], []).
'$atom_nums'([A|As], [N|Ns]) :-
	atom_number(A, N),
	integer(N), N >= 0, N =< 255,
	'$atom_nums'(As, Ns).

% A wildcard server socket comes back AF_INET6, so an IPv4 peer arrives
% v4-mapped as ::ffff:127.0.0.1. Callers expect SWI's ip/4.

'$normalise_peer'(Atom, IP) :-
	atom(Atom),
	atom_concat('::ffff:', Dotted, Atom), !,
	ip_name(IP, Dotted).
'$normalise_peer'(Atom, IP) :-
	catch(ip_name(IP, Atom), _, fail), !.
'$normalise_peer'(Atom, Atom).

% --- creation ----------------------------------------------------------

%% socket_create(-Socket, +Options).
%
% Options: `domain(inet|inet6|unix)`, `type(stream|dgram)`.

socket_create(Socket, Options) :-
	must_be(var, Socket),
	must_be(list, Options),
	(  memberchk(domain(D0), Options) -> D = D0 ; D = inet ),
	(  memberchk(type(T0), Options)   -> T = T0 ; T = stream ),
	'$new_id'(Id),
	assertz('$sock'(Id, D, T, fresh)),
	Socket = '$socket'(Id).

%% tcp_socket(-Socket).

tcp_socket(Socket) :- socket_create(Socket, []).

%% udp_socket(-Socket).

udp_socket(Socket) :- socket_create(Socket, [type(dgram)]).

%% unix_domain_socket(-Socket).

unix_domain_socket(Socket) :- socket_create(Socket, [domain(unix)]).

% Builtin options implied by the handle's domain and type.

'$sock_opts'(_, dgram, [udp(true)]) :- !.
'$sock_opts'(_, _, []).

% --- server ------------------------------------------------------------

%% tcp_bind(+Socket, ?Address).
%
% Binds AND listens - see the module header. Address may leave the port
% unbound to request an ephemeral one, in which case it is unified with
% the port actually assigned.

tcp_bind(Socket, Address) :-
	'$sock_get'(Socket, Domain, Type, Phase),
	(  Phase == fresh
	-> true
	;  throw(error(permission_error(bind, socket, Socket), tcp_bind/2))
	),
	'$bind_spec'(Domain, Address, Spec),
	'$sock_opts'(Domain, Type, Opts),
	'$sock_call'('$server'(Spec, Stream, Opts), tcp_bind/2),
	'$sock_set'(Socket, listening(Stream)).

% What to hand '$server'. A wholly or partly unbound address is passed
% through as a TERM rather than flattened to an atom, because that is how
% the bif reports the port it actually bound.

'$bind_spec'(unix, Path, Spec) :- !,
	must_be(atom, Path),
	atom_concat('unix://', Path, Spec).
'$bind_spec'(_, Address, Spec) :-
	var(Address), !,
	Spec = Address.
'$bind_spec'(_, Host:Port, Spec) :-
	var(Port), !,
	'$host_atom'(Host, H),
	Spec = H:Port.
'$bind_spec'(_, Address, Spec) :-
	'$addr_atom'(Address, Spec).

%% tcp_listen(+Socket, +BackLog).
%
% The bind already listened, so this validates and returns. BackLog is
% accepted and ignored.

tcp_listen(Socket, BackLog) :-
	must_be(integer, BackLog),
	'$sock_get'(Socket, _, _, Phase),
	(  Phase = listening(_)
	-> true
	;  throw(error(permission_error(listen, socket, Socket), tcp_listen/2))
	).

%% tcp_accept(+Socket, -Slave, -Peer).
%
% Blocks until a client connects. Slave is a new socket, already
% materialised - the stream exists before the handle does, so
% tcp_open_socket/2 on it is a lookup.

tcp_accept(Socket, Slave, Peer) :-
	'$sock_get'(Socket, Domain, Type, Phase),
	(  Phase = listening(Stream)
	-> true
	;  throw(error(permission_error(accept, socket, Socket), tcp_accept/3))
	),
	'$sock_call'('$accept'(Stream, Client), tcp_accept/3),
	(  catch('$peer_addr'(Client, Addr, _), _, fail)
	-> '$normalise_peer'(Addr, Peer)
	;  Peer = ip(0,0,0,0)
	),
	'$new_id'(Id),
	assertz('$sock'(Id, Domain, Type, connected(Client))),
	Slave = '$socket'(Id).

% --- client ------------------------------------------------------------

%% tcp_connect(+Socket, +Address).
%
% Connects an existing socket. This is where the OS socket is actually
% created - see the module header.

tcp_connect(Socket, Address) :-
	'$sock_get'(Socket, Domain, Type, Phase),
	(  Phase == fresh
	-> true
	;  throw(error(permission_error(connect, socket, Socket), tcp_connect/2))
	),
	'$connect_addr'(Domain, Address, Atom),
	'$sock_opts'(Domain, Type, Opts),
	'$sock_call'('$client'(Atom, _, _, Stream, Opts), tcp_connect/2),
	'$sock_set'(Socket, connected(Stream)).

'$connect_addr'(unix, Path, Atom) :- !,
	must_be(atom, Path),
	atom_concat('unix://', Path, Atom).
'$connect_addr'(_, Address, Atom) :-
	'$addr_atom'(Address, Atom).

%% tcp_connect(+Address, -StreamPair, +Options).
%
% The modern form: creates the socket, connects, and hands back the
% stream in one step. Options are accepted for compatibility.

tcp_connect(Address, StreamPair, Options) :-
	must_be(list, Options),
	tcp_socket(Socket),
	catch(tcp_connect(Socket, Address),
	      E,
	      ( catch(tcp_close_socket(Socket), _, true), throw(E) )),
	tcp_open_socket(Socket, StreamPair).

%% tcp_connect(+Socket, +Address, -Read, -Write).
%
% Deprecated in SWI. Read and Write are the same stream here.

tcp_connect(Socket, Address, Read, Write) :-
	tcp_connect(Socket, Address),
	tcp_open_socket(Socket, Read, Write).

% --- streams -----------------------------------------------------------

%% tcp_open_socket(+Socket, -StreamPair).
%
% Trealla socket streams are bidirectional, so the "pair" is one stream.

tcp_open_socket(Socket, StreamPair) :-
	'$sock_stream'(Socket, StreamPair).

%% tcp_open_socket(+Socket, -Read, -Write).

tcp_open_socket(Socket, Read, Write) :-
	'$sock_stream'(Socket, Stream),
	Read = Stream,
	Write = Stream.

%% tcp_close_socket(+Socket).
%
% Safe on a socket that never materialised, and on one already closed -
% SWI's is not fussy here either, and leaking a handle is worse.

tcp_close_socket(Socket) :-
	'$sock_get'(Socket, _, _, Phase),
	(  Phase = connected(S) -> catch(close(S), _, true)
	;  Phase = listening(S) -> catch(close(S), _, true)
	;  true
	),
	Socket = '$socket'(Id),
	retractall('$sock'(Id, _, _, _)).

% --- options -----------------------------------------------------------

%% tcp_setopt(+Socket, +Option).
%
% `reuseaddr` is already unconditionally set by the underlying layer, so
% it is accepted and ignored rather than refused. `dispatch/1` is GUI
% related and has no meaning here. Options needing setsockopt calls that
% Trealla does not expose are refused rather than silently dropped.

tcp_setopt(Socket, Option) :-
	'$sock_get'(Socket, _, _, _),
	must_be(nonvar, Option),
	'$setopt'(Option, Socket).

'$setopt'(reuseaddr, _) :- !.
'$setopt'(dispatch(_), _) :- !.
'$setopt'(nodelay, S) :- !, '$setopt'(nodelay(true), S).
'$setopt'(nodelay(_), _) :- !.
'$setopt'(Option, _) :-
	throw(error(domain_error(socket_option, Option), tcp_setopt/2)).

%% tcp_getopt(+Socket, ?Option).
%
% Only `file_no/1`, which is what SWI documents.

tcp_getopt(Socket, file_no(F)) :- !,
	'$sock_stream'(Socket, Stream),
	stream_property(Stream, file_no(F)).
tcp_getopt(_, Option) :-
	throw(error(domain_error(socket_option, Option), tcp_getopt/2)).

% --- names -------------------------------------------------------------

%% gethostname(-Host).

gethostname(Host) :- '$current_host'(Host).

%% tcp_host_to_address(?Host, ?Address).
%
% Resolution is one-way here: name to address. SWI's reverse direction
% needs a PTR lookup that Trealla does not expose.

tcp_host_to_address(Host, Address) :-
	nonvar(Host), !,
	must_be(atom, Host),
	(  '$host_address'(Host, Atom)
	-> '$normalise_peer'(Atom, Address)
	;  throw(error(existence_error(host, Host), tcp_host_to_address/2))
	).
tcp_host_to_address(_, _) :-
	throw(error(instantiation_error, tcp_host_to_address/2)).
