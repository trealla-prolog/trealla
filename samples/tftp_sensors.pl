/*  A TFTP server whose "files" are readings rather than files.

	The name in a read request is just a string; nothing says it has to be
	a file. So a board doing bare-metal work can be asked what it knows by
	any TFTP client on the network - no bespoke tooling:

		tpl -f samples/tftp_sensors.pl -g "main(6969)"

		$ tftp 127.0.0.1 6969
		tftp> get sensors/temp0
		tftp> quit
		$ cat temp0
		reading(temp0, 21.44, celsius, 1757032411).

	Virtual names are matched before any filesystem check, so they can carry
	'/' and build a hierarchy without ever becoming a path. They are also
	read-only: a write to one is refused.

	Each reading is one Prolog term, so a Prolog client can read_term/2 the
	answer straight back - while `tftp` and `cat` still work for anyone else.
*/

:- use_module(library(tftp)).

main(Port) :-
	format("readings on port ~d, ^C to stop~n", [Port]),
	tftp_serve('/nonexistent', Port, [virtual(reading)]).

% The namespace is just a predicate: adding a reading is adding a clause.

reading('sensors/temp0', Codes) :-
	get_time(Now),
	Celsius is 20.0 + (truncate(Now) mod 60) / 10.0,
	term_codes(reading(temp0, Celsius, celsius, Now), Codes).

reading('sensors/count', Codes) :-
	get_time(Now),
	Count is truncate(Now) mod 1000,
	term_codes(reading(count, Count), Codes).

reading('status/uptime', Codes) :-
	cpu_time(Seconds),
	term_codes(uptime(Seconds, seconds), Codes).

reading('status/index', Codes) :-
	findall(N, reading_name(N), Names),
	term_codes(readings(Names), Codes).

reading_name(N) :- member(N, ['sensors/temp0', 'sensors/count',
	'status/uptime', 'status/index']).

term_codes(Term, Codes) :-
	format(atom(A), "~q.~n", [Term]),
	atom_codes(A, Codes).
