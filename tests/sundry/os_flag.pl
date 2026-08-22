:- initialization(main).

% current_prolog_flag(os, OS) names the host operating system. Its
% value differs by platform, so what is checked here is everything
% about it that does not: that it is a known atom, that it agrees with
% the unix flag, that it enumerates, and that it cannot be set.
%
% The unix flag said true on every host, Windows included, until it was
% derived from the same answer.

:- use_module(library(lists)).

known(windows). known(linux). known(macos). known(android).
known(freebsd). known(openbsd). known(netbsd). known(dragonfly).
known(solaris). known(haiku). known(riscos). known(cygwin).
known(wasi). known(emscripten).

% The hosts with no POSIX to speak of; everything else is a unix.

not_unix(windows). not_unix(wasi). not_unix(riscos).

check(Name, Goal) :-
	(	catch(call(Goal), E, (write(Name), write(' THREW '), writeq(E), nl, fail))
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED'), nl
	).

% it is bound to one of the names the build knows how to report

named :-
	current_prolog_flag(os, OS),
	atom(OS),
	(	known(OS)
	->	true
	;	write('  (unrecognised os: '), writeq(OS), write(')'), nl,
		fail
	).

% asking twice gives the same answer, and asking for the wrong one fails
% rather than erroring

stable :-
	current_prolog_flag(os, A),
	current_prolog_flag(os, B),
	A == B,
	\+ current_prolog_flag(os, 'no-such-os').

% it turns up in the enumeration, not only when asked for by name

enumerated :-
	current_prolog_flag(os, OS),
	findall(V, (current_prolog_flag(F, V), F == os), Vs),
	Vs == [OS].

% unix says what os implies

agrees_with_unix :-
	current_prolog_flag(os, OS),
	current_prolog_flag(unix, U),
	(	not_unix(OS)
	->	U == false
	;	U == true
	).

% read-only

read_only :-
	catch(set_prolog_flag(os, linux), error(E, _), true),
	nonvar(E),
	E = permission_error(modify, flag, os).

main :-
	check(named, named),
	check(stable, stable),
	check(enumerated, enumerated),
	check(agrees_with_unix, agrees_with_unix),
	check(read_only, read_only).
