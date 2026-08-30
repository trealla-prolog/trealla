% Regression test for the alarm/interrupt gap described in
% docs/DESIGN-GUSTTO.md ("Follow-on (2026-08-25): hit in practice,
% stopgapped, not fixed properly"): a non-blocking socket stream with
% nothing to read yet has to wait out EAGAIN via tpl_wait_fd_readable()
% (see network.c's retry_getc()/tpl_getline()), because this build's
% alarms are delivered by polling a deadline rather than by a real SIGALRM
% that could break a blocked read with EINTR.
%
% If any of the affected predicates (get_char/1,2, get_code/1,2,
% get_byte/1,2, peek_char/1,2, peek_code/1,2, peek_byte/1,2, getline/1,2,3,
% read_line_to_string/2, read_line_to_codes/2) regress to a plain blocking
% read on a stream that never went non-blocking in the first place, this
% whole test hangs until the suite's own 60s per-test timeout kills it,
% rather than reaching "all ok".
%
% get_code/2, peek_char/2, peek_code/2 and peek_byte/2 aren't covered
% directly: they require mode(read) exactly, which library(socket)'s
% bidirectional streams never are. Their /1 (current_input) forms have no
% such check and share the same guard, so they stand in below.

:- use_module(library(socket)).
:- use_module(library(iso_ext)).
:- initialization(main).

:- dynamic(saw_failure/0).

t(Label, Goal) :-
	(   catch(
	        (call_with_time_limit(0.2, Goal) -> Outcome = unexpected_success ; Outcome = unexpected_failure),
	        error(time_limit_exceeded(_,_), _),
	        Outcome = ok
	    )
	->  true
	;   Outcome = uncaught_failure
	),
	(   Outcome == ok
	->  true
	;   format("STREAM-TIMEOUT-FAIL ~w: ~w~n", [Label, Outcome]),
	    (saw_failure -> true ; assertz(saw_failure))
	).

% A fresh connected pair on the shared listener. The slave side is never
% written to, so the client stream genuinely has nothing pending.
quiet_pair(Srv, Client, Slave) :-
	tcp_socket(Cl), tcp_connect(Cl, '127.0.0.1':3420),
	tcp_accept(Srv, Sl, _),
	tcp_open_socket(Cl, Client),
	tcp_open_socket(Sl, Slave).

test_direct(Srv, Label, Pred) :-
	quiet_pair(Srv, S, SS),
	G =.. [Pred, S, _],
	t(Label, G),
	close(S), close(SS).

test_direct_binary(Srv, Label, Pred) :-
	quiet_pair(Srv, S, SS),
	set_stream(S, [type(binary)]),
	G =.. [Pred, S, _],
	t(Label, G),
	close(S), close(SS).

test_current_input(Srv, Label, Goal) :-
	quiet_pair(Srv, S, SS),
	set_input(S),
	t(Label, Goal),
	set_input(user_input),
	close(S), close(SS).

test_current_input_binary(Srv, Label, Goal) :-
	quiet_pair(Srv, S, SS),
	set_stream(S, [type(binary)]),
	set_input(S),
	t(Label, Goal),
	set_input(user_input),
	close(S), close(SS).

main :-
	tcp_socket(Srv), tcp_bind(Srv, '127.0.0.1':3420), tcp_listen(Srv, 5),

	test_direct(Srv, get_char_2, get_char),
	test_direct_binary(Srv, get_byte_2, get_byte),
	test_direct(Srv, getline_2, getline),
	test_direct(Srv, read_line_to_string_2, read_line_to_string),
	test_direct(Srv, read_line_to_codes_2, read_line_to_codes),

	quiet_pair(Srv, S1, SS1),
	t(getline_3, getline(S1, _, [])),
	close(S1), close(SS1),

	test_current_input(Srv, get_char_1, get_char(_)),
	test_current_input(Srv, get_code_1, get_code(_)),
	test_current_input(Srv, peek_char_1, peek_char(_)),
	test_current_input(Srv, peek_code_1, peek_code(_)),
	test_current_input_binary(Srv, peek_byte_1, peek_byte(_)),
	test_current_input(Srv, getline_1, getline(_)),

	tcp_close_socket(Srv),

	(   saw_failure
	->  format("stream_timeout: FAILURES above~n")
	;   format("stream_timeout: all ok~n")
	).
