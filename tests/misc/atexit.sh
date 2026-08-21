#!/bin/sh

# halt/0 and halt/1 run ignore(atexit) before '$halt', and atexit/0 is
# dynamic, so a library registers shutdown work by asserting a clause.
# Three properties matter to anyone doing that, and none is obvious:
#
#   - the exit status survives the hook;
#   - ignore/1 commits to the first solution, so a clause that SUCCEEDS
#     ends the chain and a later registration never runs. Ending with
#     fail lets the next one through;
#   - an exception in the hook aborts the goal before '$halt' is
#     reached, and the requested exit status is lost.
#
# The last of those records current behaviour rather than endorsing it.
# If halt/1 is ever hardened to keep its status across a throwing hook,
# this test fails and should be updated to match.

set -e
TPL=${TPL:-./tpl}
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"' EXIT

# stdin is closed: a goal that throws never reaches halt, and the
# toplevel would otherwise sit waiting for input.
run() {
	out=$(timeout 20 "$TPL" -q -f "$1" </dev/null 2>/dev/null) && rc=0 || rc=$?
	[ -n "$out" ] && printf '%s\n' "$out"
	echo "exit=$rc"
}

cat > "$DIR/plain.pl" <<'EOF'
:- initialization(main).
main :- assertz((atexit :- write(hook), nl)), halt.
EOF

cat > "$DIR/status.pl" <<'EOF'
:- initialization(main).
main :- assertz((atexit :- write(hook), nl)), halt(3).
EOF

cat > "$DIR/chain.pl" <<'EOF'
:- initialization(main).
main :-
	assertz((atexit :- write(first), nl, fail)),
	assertz((atexit :- write(second), nl)),
	halt(5).
EOF

cat > "$DIR/commit.pl" <<'EOF'
:- initialization(main).
main :-
	assertz((atexit :- write(first), nl)),
	assertz((atexit :- write(second), nl)),
	halt.
EOF

cat > "$DIR/throws.pl" <<'EOF'
:- initialization(main).
main :- assertz((atexit :- throw(oops))), halt(2).
EOF

echo "--- halt/0 runs the hook ---"
run "$DIR/plain.pl"

echo "--- halt/1 runs it and keeps the status ---"
run "$DIR/status.pl"

echo "--- a failing clause chains to the next ---"
run "$DIR/chain.pl"

echo "--- a succeeding clause ends the chain ---"
run "$DIR/commit.pl"

echo "--- a throwing hook: no crash, status lost ---"
run "$DIR/throws.pl"
