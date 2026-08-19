#!/bin/sh

# library(syslog). The openlog `perror` option copies each message to our
# own stderr, so this can check the messages really reach syslog(3)
# without a log daemon running.
#
# The prefix perror writes is not portable - macOS prepends a timestamp
# and an empty hostname, glibc writes only "ident[pid]: " - so the test
# looks for the message text rather than diffing the whole line.

set -e
TPL=${TPL:-./tpl}
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"' EXIT

cat > "$DIR/t.pl" <<'EOF'
:- use_module(library(syslog)).
:- initialization(main).

main :-
	openlog(trealla_syslog_test, [perror,pid], user),
	syslog(info, 'an atom message'),
	syslog(notice, "a string message"),
	syslog(debug, [99,111,100,101,115]),
	syslog(err, "formatted ~w", [argument]),
	closelog,
	% no connection open: this one goes to user_error instead
	syslog(warning, "fallback ~w", [message]),
	% names the platform does not define are rejected, not ignored
	chk(bad_facility, openlog(x, [], no_such_facility),
	    error(domain_error(syslog_facility, _), _)),
	chk(bad_option, openlog(x, [no_such_option], user),
	    error(domain_error(syslog_option, _), _)),
	chk(bad_priority, syslog(no_such_priority, m),
	    error(domain_error(syslog_priority, _), _)),
	chk(bad_ident, openlog(1, [], user),
	    error(type_error(atom, _), _)).

chk(Label, Goal, Expected) :-
	(  catch(Goal, E, true)
	-> (  nonvar(E), subsumes_term(Expected, E)
	   -> format("~w: raised~n", [Label])
	   ;  format("~w: WRONG ~q~n", [Label, E])
	   )
	;  format("~w: FAILED~n", [Label])
	).
EOF

timeout 30 "$TPL" -q -f -g halt "$DIR/t.pl" >"$DIR/out" 2>"$DIR/err"

check() {
	if grep -q -- "$1" "$DIR/err"; then echo "found: $1"; else echo "MISSING: $1"; fi
}

check "an atom message"
check "a string message"
check "codes"
check "formatted argument"
check "warning: fallback message"

# The ident must appear, or openlog's first argument is being ignored.
if grep -q "trealla_syslog_test" "$DIR/err"; then echo "ident ok"; else echo "IDENT MISSING"; fi

echo "--- error handling ---"
cat "$DIR/out"
