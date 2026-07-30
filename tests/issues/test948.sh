#!/bin/sh

# Regression for issue #948: when the reader closes a fifo, the writer should
# get existence_error(stream, ...) from portray_clause/2 rather than keep
# writing forever. A follow-on bug left portray_clause's fullstop/nl flags
# set across the throw, so the exception ball was printed as
# "error(...).\n." and parse_to_heap then reported a spurious syntax error.

set -e
TPL=${TPL:-./tpl}
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"' EXIT

mkfifo "$DIR/fifo"

cat > "$DIR/writer.pl" <<EOF
:- initialization(main).

main :-
	open('$DIR/fifo', write, S),
	between(1, 100000, I),
	catch(portray_clause(S, p(I)), error(Err, _), true),
	nonvar(Err),
	!,
	format("Err = ~q~n", [Err]).
EOF

cat > "$DIR/reader.pl" <<EOF
:- initialization(main).

main :-
	open('$DIR/fifo', read, S),
	read(S, T),
	close(S),
	format("T = ~q~n", [T]).
EOF

# Writer first (blocks in open), then reader. Cap output so a regression that
# spins can't fill the disk; closing the pipe also takes a looping writer down.
timeout 30 "$TPL" -q -f -g halt "$DIR/writer.pl" >"$DIR/w.out" 2>"$DIR/w.err" &
WPID=$!
sleep 0.3
timeout 20 "$TPL" -q -f -g halt "$DIR/reader.pl" >"$DIR/r.out" 2>"$DIR/r.err"
wait "$WPID" || true

echo "--- reader ---"
head -c 500 "$DIR/r.out"
echo
echo "--- writer ---"
head -c 500 "$DIR/w.out"
echo
echo "--- writer stderr ---"
# Empty is the pass: any leftover "Error: syntax error..." fails the golden.
head -c 500 "$DIR/w.err"
