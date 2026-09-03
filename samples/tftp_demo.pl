/*  Fetch a file over TFTP and report what came back.

	Needs a TFTP server to talk to. Any will do - tftpd-hpa, dnsmasq's
	--enable-tftp, or the one built into a router. Port 69 is privileged,
	so a test server usually sits somewhere higher:

		tpl -f samples/tftp_demo.pl -g "get('127.0.0.1', 6969, 'hello.txt')"

	Deliberately without initialization/1: the whole point is to name a
	host, and auto-running a default fetch on every load would just print
	a failed attempt before whatever was actually asked for. `-g main`
	tries localhost:69 for anyone who wants a default.
*/

:- use_module(library(tftp)).
:- use_module(library(lists)).

main :-
	get('127.0.0.1', 69, 'hello.txt').

get(Host, Port, File) :-
	(	catch(tftp_get(Host, Port, File, Bytes), Error, true)
	->	report(File, Bytes, Error)
	;	format("~w: transfer failed~n", [File])
	).

report(File, _, Error) :-
	nonvar(Error), !,
	explain(Error, Text),
	format("~w: ~w~n", [File, Text]).
report(File, Bytes, _) :-
	length(Bytes, Len),
	format("~w: ~d bytes~n", [File, Len]),
	preview(Bytes).

% Show the first line or so, printably: a TFTP transfer is bytes, and the
% file may well not be text at all.

preview(Bytes) :-
	length(Preview, 60),
	(	append(Preview, _, Bytes) -> true ; Preview = Bytes ),
	maplist(printable, Preview, Safe),
	atom_codes(Atom, Safe),
	format("  ~w~n", [Atom]).

printable(C, C) :- C >= 32, C =< 126, !.
printable(0'\n, 0'\n) :- !.
printable(_, 0'.).

explain(error(tftp_error(Code, Message), _), Text) :- !,
	format(atom(Text), "server said ~w (code ~d)", [Message, Code]).
explain(error(tftp_timeout, _), 'no reply - is a server listening?') :- !.
explain(Error, Error).
