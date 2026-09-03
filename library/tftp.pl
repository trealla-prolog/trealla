/*  A TFTP client (RFC 1350) in Prolog, over library(socket)'s UDP.

	?- use_module(library(tftp)).
	?- tftp_get('127.0.0.1', 6969, 'hello.txt', Bytes).

	Data is carried as lists of byte values throughout: every datagram uses
	encoding(octet) AND as(codes). Both are needed - as/1 defaults to
	string, so encoding(octet) on its own hands back a string rather than
	the byte list this code takes apart. That matters: library(socket) sends text as UTF-8
	by default, which would turn every byte over 127 into two on the wire
	and corrupt any file that is not ASCII.

	The protocol logic here is deliberately free of anything hosted: given
	udp_socket/1, udp_send/4 and udp_receive/4 with the same meanings, this
	same file is what a freestanding image would run. See
	docs/rpi4-networking-proposal.md.

	Lost datagrams are retransmitted: udp_receive/4's timeout(Ms) option
	fails rather than throwing when nothing arrives, so the retry is an
	ordinary if-then-else. Note that call_with_time_limit/2 is no use here -
	its alarm is cooperative and cannot interrupt a blocked recvfrom.
*/

:- module(tftp, [
	tftp_get/3, tftp_get/4,
	tftp_put/3, tftp_put/4
	]).

:- use_module(library(socket)).
:- use_module(library(lists)).

tftp_default_port(69).
tftp_block_size(512).
tftp_timeout_ms(1000).
tftp_retries(5).

opcode(rrq,   1).
opcode(wrq,   2).
opcode(data,  3).
opcode(ack,   4).
opcode(error, 5).

%% tftp_get(+Host, +File, -Bytes) is det.
%% tftp_get(+Host, +Port, +File, -Bytes) is det.
%
% Read File from the server as a list of byte values.

tftp_get(Host, File, Bytes) :-
	tftp_default_port(Port),
	tftp_get(Host, Port, File, Bytes).

tftp_get(Host, Port, File, Bytes) :-
	setup_socket(Socket),
	request_packet(rrq, File, Packet),
	udp_send(Socket, Packet, Host:Port, [encoding(octet)]),
	catch(collect(Socket, 1, none, Host:Port, Packet, [], Blocks), E,
		(tcp_close_socket(Socket), throw(E))),
	tcp_close_socket(Socket),
	append(Blocks, Bytes).

%% tftp_put(+Host, +File, +Bytes) is det.
%% tftp_put(+Host, +Port, +File, +Bytes) is det.
%
% Write Bytes to the server as File.

tftp_put(Host, File, Bytes) :-
	tftp_default_port(Port),
	tftp_put(Host, Port, File, Bytes).

tftp_put(Host, Port, File, Bytes) :-
	setup_socket(Socket),
	request_packet(wrq, File, Packet),
	udp_send(Socket, Packet, Host:Port, [encoding(octet)]),
	% The server acknowledges a WRQ with block 0, from its new port.
	catch(( recv_retry(Socket, none, Host:Port, Packet, Peer0, Reply),
			check_error(Reply),
			expect_ack(Reply, 0),
			send_blocks(Socket, Peer0, 1, Bytes)
		  ), E, (tcp_close_socket(Socket), throw(E))),
	tcp_close_socket(Socket).

% A receiving socket has to be bound, and binding to port 0 takes an
% ephemeral one - which is exactly the client TID the RFC asks for.

setup_socket(Socket) :-
	udp_socket(Socket),
	tcp_bind(Socket, _EphemeralPort).

% RRQ/WRQ: opcode, NUL-terminated filename, NUL-terminated mode. Always
% octet mode - netascii would mangle line endings, and this client deals
% in bytes.

request_packet(Kind, File, Packet) :-
	opcode(Kind, Op),
	atom_codes(File, FileCodes),
	atom_codes(octet, ModeCodes),
	append(FileCodes, [0|ModeCodes], Body0),
	append(Body0, [0], Body),
	Packet = [0, Op|Body].

% --- reading -----------------------------------------------------------
%
% Peer starts as none: the reply to a request comes from a NEW port the
% server picks (its TID), and every later packet of the transfer must come
% from that same port. Anything else is ignored, which is what stops a
% stray or spoofed datagram from joining the conversation.
%
% Resend/To are what to retransmit, and where, if nothing arrives: the
% request until the first block lands, then the most recent ACK.

collect(Socket, Block, Peer0, To, Resend, Acc, Blocks) :-
	recv_retry(Socket, Peer0, To, Resend, Peer, Packet),
	check_error(Packet),
	(	packet_data(Packet, Block, Data)
	->	ack_packet(Block, Ack),
		udp_send(Socket, Ack, Peer, [encoding(octet)]),
		tftp_block_size(Size),
		length(Data, Len),
		(	Len < Size			% a short block ends the transfer
		->	reverse([Data|Acc], Blocks)
		;	next_block(Block, Next),
			collect(Socket, Next, Peer, Peer, Ack, [Data|Acc], Blocks)
		)
	;	% A block already taken - the server retransmitted because an ACK
		% went astray. Acknowledge it again and keep waiting for the one
		% actually wanted, rather than counting it twice.
		(	packet_data(Packet, Duplicate, _)
		->	ack_packet(Duplicate, DupAck),
			udp_send(Socket, DupAck, Peer, [encoding(octet)])
		;	true
		),
		collect(Socket, Block, Peer, To, Resend, Acc, Blocks)
	).

ack_packet(Block, [0, Op, Hi, Lo]) :-
	opcode(ack, Op),
	block_bytes(Block, Hi, Lo).

% --- writing -----------------------------------------------------------

send_blocks(Socket, Peer, Block, Bytes) :-
	tftp_block_size(Size),
	split_block(Bytes, Size, Chunk, Rest),
	opcode(data, Op),
	block_bytes(Block, Hi, Lo),
	append([0, Op, Hi, Lo], Chunk, Packet),
	udp_send(Socket, Packet, Peer, [encoding(octet)]),
	recv_retry(Socket, Peer, Peer, Packet, _, Reply),
	check_error(Reply),
	expect_ack(Reply, Block),
	length(Chunk, Len),
	(	Len < Size			% a short block ends the transfer
	->	true
	;	next_block(Block, Next),
		send_blocks(Socket, Peer, Next, Rest)
	).

split_block(Bytes, Size, Chunk, Rest) :-
	length(Bytes, Len),
	(	Len >= Size
	->	length(Chunk, Size),
		append(Chunk, Rest, Bytes)
	;	Chunk = Bytes,
		Rest = []
	).

% --- packets -----------------------------------------------------------
%
% Receive, retransmitting Resend to To whenever the wait expires, and
% ignoring anything that arrives from an address other than the transfer's
% peer. Gives up after tftp_retries/1 silent intervals.

recv_retry(Socket, Peer0, To, Resend, Peer, Packet) :-
	tftp_retries(N),
	recv_retry(Socket, Peer0, To, Resend, N, Peer, Packet).

recv_retry(Socket, Peer0, To, Resend, Retries, Peer, Packet) :-
	tftp_timeout_ms(Ms),
	(	udp_receive(Socket, Packet0, From,
			[encoding(octet), as(codes), timeout(Ms)])
	->	(	( Peer0 == none ; From == Peer0 )
		->	Peer = From, Packet = Packet0
		;	% Not our peer. Still our wait, so do not spend a retry on it.
			recv_retry(Socket, Peer0, To, Resend, Retries, Peer, Packet)
		)
	;	Retries > 0
	->	udp_send(Socket, Resend, To, [encoding(octet)]),
		Left is Retries - 1,
		recv_retry(Socket, Peer0, To, Resend, Left, Peer, Packet)
	;	throw(error(tftp_timeout, tftp/4))
	).

packet_data([0, Op, Hi, Lo|Data], Block, Data) :-
	opcode(data, Op),
	block_number(Hi, Lo, Block).

expect_ack([0, Op, Hi, Lo], Block) :-
	opcode(ack, Op),
	block_number(Hi, Lo, Block).

% An ERROR packet is the server's way of saying no. Turn it into an
% exception carrying its code and message rather than a silent failure.

check_error([0, Op, Hi, Lo|Rest]) :-
	opcode(error, Op), !,
	block_number(Hi, Lo, Code),
	message_text(Rest, Text),
	throw(error(tftp_error(Code, Text), tftp/4)).
check_error(_).

message_text(Bytes, Text) :-
	(	append(Codes, [0], Bytes)
	->	true
	;	Codes = Bytes
	),
	atom_codes(Text, Codes).

% Block numbers are 16-bit and wrap round rather than stopping, which
% matters for files over 32MB.

block_number(Hi, Lo, Block) :- Block is Hi * 256 + Lo.

block_bytes(Block, Hi, Lo) :-
	Hi is (Block >> 8) /\ 0xff,
	Lo is Block /\ 0xff.

next_block(Block, Next) :- Next is (Block + 1) /\ 0xffff.
