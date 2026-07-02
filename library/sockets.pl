:- module(sockets, [socket_client_open/3,
	socket_server_open/3,
	socket_server_accept/4,
	socket_server_close/1,
	current_host/1
	]).

:- use_module(library(error)).

%% socket_client_open(+Addr, -Stream, +Options).
%
% Open a socket to a server, returning a stream. Addr must satisfy:
%
%    `Addr = unix(Path)`.
%    `Addr = inet(Address,Port)`.
%    `Addr = Address:Port`.
%    `Addr = Port`.
%
% The following options are available:
%
%  * `udp(+Boolean)`: Socket is UDP (default is TCP)
%  * `ssl(+Boolean)`: Socket is SSL/TLS (default is normal) if TCP
%  * `certfile(+Filename)`: Certificate file
%  * `alias(+Alias)`: Set an alias to the stream
%  * `eof_action(+Action)`: Defined what happens if the end of the stream is reached. Values: `error`, `eof_code` and `reset`.
%  * `type(+Type)`: Type can be `text` or `binary`. Defines the type of the stream, if it's optimized for plain text
%    or just binary
%
socket_client_open(Addr0, Stream, Options) :-
	( var(Addr0) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr0 = unix(Path), !,
	must_be(var, Stream),
	must_be(list, Options),
	atom(Path),
	atom_concat('unix://', Path, Addr),
	'$client'(Addr, _, _, Stream, Options),
	set_stream(Stream, Options).

socket_client_open(Addr0, Stream, Options) :-
	( var(Addr0) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr0 = inet(Address,Port), !,
	must_be(var, Stream),
	must_be(list, Options),
	(  Addr = Address:Port,
	atom(Address),
	( atom(Port) ; integer(Port) ) ->
		true
	; throw(error(type_error(socket_address, Addr), socket_client_open/3))
	),
	'$client'(Addr, _, _, Stream, Options),
	set_stream(Stream, Options).

socket_client_open(Addr, Stream, Options) :-
	( var(Addr) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr = Address:Port,
	must_be(var, Stream),
	must_be(list, Options),
	atom(Address),
	(( atom(Port) ; integer(Port) ) ->
		true
	; throw(error(type_error(socket_address, Addr), socket_client_open/3))
	),
	'$client'(Addr, _, _, Stream, Options),
	set_stream(Stream, Options).

socket_client_open(Addr, Stream, Options) :-
	( var(Addr) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr = Port,
	must_be(var, Stream),
	must_be(list, Options),
	(( atom(Port) ; integer(Port) ) ->
		true
	; throw(error(type_error(socket_address, Addr), socket_client_open/3))
	),
	'$client'(Addr, _, _, Stream, Options),
	set_stream(Stream, Options).



%% socket_server_open(+Addr, -ServerSocket, +Options).
%
% Open a server socket, returning a ServerSocket. Use that ServerSocket to accept incoming connections in
% `socket_server_accept/3`. Addr must satisfy `Addr = Address:Port`. Depending on the operating system
% configuration, some ports might be reserved for superusers. Address must satisfy:
%
%    `Addr = unix(Path)`.
%    `Addr = inet(Address,Port)`.
%    `Addr = Address:Port`.
%    `Addr = Port`.
%
% The following options are available:
%
%  * `udp(+Boolean)`: Socket is UDP (default is TCP)
%  * `ssl(+Boolean)`: Socket is SSL/TLS (default is normal) if TCP
%  * `keyfile(+Filename)`: Key file
%  * `certfile(+Filename)`: Certificate file
%
socket_server_open(Addr0, ServerSocket, Options) :-
	( var(Addr0) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr0 = unix(Path), !,
	must_be(var, ServerSocket),
	must_be(list, Options),
	atom(Path),
	atom_concat('unix://', Path, Addr),
	'$server'(Addr, ServerSocket, Options).

socket_server_open(Addr0, ServerSocket, Options) :-
	( var(Addr0) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr0 = inet(Address,Port), !,
	must_be(var, Stream),
	must_be(list, Options),
	(  Addr = Address:Port,
	atom(Address),
	( atom(Port) ; integer(Port) ) ->
		true
	; throw(error(type_error(socket_address, Addr), socket_client_open/3))
	),
	'$server'(Addr, ServerSocket, Options).

socket_server_open(Addr, ServerSocket, Options) :-
	( var(Addr) ->
		throw(error(instantiation_error, socket_client_open/3))
	; true
	),
	Addr = Address:Port,
	must_be(var, Stream),
	must_be(list, Options),
	atom(Address),
	(( atom(Port) ; integer(Port) ) ->
		true
	; throw(error(type_error(socket_address, Addr), socket_client_open/3))
	),
	'$server'(Addr, ServerSocket, Options).

socket_server_open(Addr0, ServerSocket, Options) :-
	must_be(var, ServerSocket),
	( integer(Addr0) ->
		( number_codes(Addr0, Codes), atom_codes(Addr1, Codes), atom_concat(':', Addr1, Addr) )
	; Addr = Addr0
	),
	'$server'(Addr, ServerSocket, Options).



%% socket_server_accept(+ServerSocket, -Client, -Stream, +Options).
%
% Given a ServerSocket and a list of Options, accepts a incoming connection, returning data from the Client and
% a Stream to read or write data.
%
% The following options are available:
%
%  * `alias(+Alias)`: Set an alias to the stream
%  * `eof_action(+Action)`: Defined what happens if the end of the stream is reached. Values: `error`, `eof_code` and `reset`.
%  * `type(+Type)`: Type can be `text` or `binary`. Defines the type of the stream, if it's optimized for plain text
%    or just binary
%
socket_server_accept(ServerSocket, Client, Stream, Options) :-
	must_be(var, Client),
	must_be(var, Stream),
	'$accept'(ServerSocket, Stream),
	Client = Stream,
	set_stream(Stream, Options).



%% socket_server_close(+ServerSocket).
%
% Stops listening on that ServerSocket. It's recommended to always close a ServerSocket once it's no longer needed
socket_server_close(ServerSocket) :-
	close(ServerSocket).



%% current_hostname(-HostName).
%
% Returns the current hostname of the computer in which Trealla Prolog is executing right now
current_host(HostName) :-
	'$current_host'(HostName).
