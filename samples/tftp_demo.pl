/*  Fetch a file over TFTP, or serve a directory, and report what happens.

	Port 69 is privileged, so a test setup usually sits somewhere higher.
	In one terminal:

		tpl -f samples/tftp_demo.pl -g "serve('/tmp/tftproot', 6969)"

	and in another:

		tpl -f samples/tftp_demo.pl -g "get('127.0.0.1', 6969, 'hello.txt')"

	Any other TFTP implementation works just as well at either end -
	tftpd-hpa, dnsmasq's --enable-tftp, or the one in a router.

	Deliberately without initialization/1: the whole point is to name a
	host, and auto-running a default fetch on every load would just print
	a failed attempt before whatever was actually asked for. `-g main`
	tries localhost:69 for anyone who wants a default.
*/

:- use_module(library(tftp)).
:- use_module(library(lists)).

main :-
	get('127.0.0.1', 69, 'hello.txt').

% Serves until interrupted, one transfer at a time.

serve(Root, Port) :-
	format("serving ~w on port ~d, ^C to stop~n", [Root, Port]),
	tftp_serve(Root, Port).

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
	% length/2 has to stay inside the condition: bind Prefix to 60 fresh
	% variables first and the fallback can no longer unify it with a
	% shorter file, so anything under 60 bytes prints nothing at all.
	(	length(Prefix, 60),
		append(Prefix, _, Bytes)
	->	true
	;	Prefix = Bytes
	),
	maplist(printable, Prefix, Safe),
	atom_codes(Atom, Safe),
	format("  ~w~n", [Atom]).

printable(C, C) :- C >= 32, C =< 126, !.
printable(0'\n, 0'\n) :- !.
printable(_, 0'.).

explain(error(tftp_error(Code, Message), _), Text) :- !,
	format(atom(Text), "server said ~w (code ~d)", [Message, Code]).
explain(error(tftp_timeout, _), 'no reply - is a server listening?') :- !.
explain(Error, Error).
