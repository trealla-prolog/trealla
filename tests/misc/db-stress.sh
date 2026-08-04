#!/bin/sh
#
# db-stress: the database/index stress tests, with a memory regression guard.
#
# Was samples/db-stress.pl, which ran the six workloads but only ever
# reported that they finished. All six are meant to run in CONSTANT
# memory: each round fills a predicate and empties it again, so round 10
# should cost no more than round 1. Nothing checked that, and an index
# that failed to withdraw its entries went unnoticed until it was found
# by hand.
#
# Checked here by running each workload at two round counts and comparing
# peak RSS. A workload that reclaims properly is flat; one that leaks
# grows with the round count. Sampled with `ps -o rss=`, which is KB on
# both Linux and macOS - /usr/bin/time's memory flags are not portable
# between GNU and BSD.
#
# A seventh workload is included that the original six do not cover. All
# six drain their predicate to empty, and emptying it destroys every
# index wholesale at cnt == 0 - which sweeps up any entry that was never
# withdrawn. That hides exactly the bug the guard is for. The seventh
# churns without ever emptying, so each index has to withdraw entry by
# entry. Verified to discriminate: with per-clause removal from idx1a
# deleted, tests 1-6 stay flat at 1.00x and test 7 grows to 1.50x.

TPL=${TPL:-./tpl}

N=200000        # clauses per round; well over the 500 index threshold

# Test 8 runs smaller. Its shape puts ~5% of clauses in the two side
# lists, and a side list under the one-tenth cap is merged into EVERY
# lookup - drained and re-sorted into a fresh prefetch each time - so
# cost per lookup grows with the predicate. At 200000 it is quadratic and
# does not finish; at 20000 it still populates all four structures and
# still catches a leak in any of them. Pre-existing behaviour of the
# side-list merge, not something this test is trying to measure.
N8=20000
R_LO=2
R_HI=6
LIMIT=120       # hundredths: fail over 1.20x. Measured noise is <= 1.01x.

PL=$(mktemp /tmp/db-stress-XXXXXX.pl)
trap 'rm -f "$PL"' EXIT

cat > "$PL" <<'PLEOF'
:- dynamic(f/1).
:- dynamic(h/2).
:- initialization(main).

fill(N)  :- between(1,N,I), assertz(f(I)), fail.
fill(_).
fillg(N) :- between(1,N,I), assertz(f(g(I))), fail.
fillg(_).

t(1,N) :- fill(N), retractall(f(_)).
t(2,N) :- fill(N), abolish(f/1).
t(3,N) :- fillg(N), ( retract(f(_)), fail ; true ).
t(4,N) :- fillg(N), ( unwind(N), fail ; true ).
t(5,N) :- fillg(N), ( clause(f(_),_), fail ; true ), retractall(f(_)).
t(6,N) :- fillg(N), ( f(_), fail ; true ), retractall(f(_)).

% Churn without ever emptying: f(keep) holds the predicate above zero,
% so the cnt == 0 teardown never fires and every index must withdraw its
% own entries clause by clause.
t(7,N) :- ( f(keep) -> true ; assertz(f(keep)) ), fillg(N),
	( between(1,N,I), retract(f(g(I))), fail ; true ).

% Everything above is arity 1, so idx2 and the arg2 side list never
% engage and a leak in either would go unseen. This one is arity 2 and
% shaped to populate all four structures at once: mostly ground in both
% args (idx1, idx1a, idx2), a fortieth with a var arg1 (wild1a) and a
% thirty-seventh with a var arg2 (wild2). Both side lists stay under the
% one-tenth cap, so the merges actually run rather than falling back to a
% chain walk. Retracted by unique keys, and h(keep,keep) holds the
% predicate above zero as in test 7.
t(8,N) :-
	( h(keep,keep) -> true ; assertz(h(keep,keep)) ),
	( between(1,N,I), hput(I), fail ; true ),
	( between(1,N,I), hdel(I), fail ; true ).

hput(I) :- 0 is I mod 40, !, assertz(h(_, a(I))).
hput(I) :- 0 is I mod 37, !, assertz(h(b(I), _)).
hput(I) :- assertz(h(c(I), d(I))).

hdel(I) :- 0 is I mod 40, !, retract(h(_, a(I))).
hdel(I) :- 0 is I mod 37, !, retract(h(b(I), _)).
hdel(I) :- retract(h(c(I), d(I))).

unwind(0) :- !.
unwind(I) :- retract(f(g(I))), I2 is I-1, unwind(I2).

left(8, C) :- !, catch(findall(x, h(_,_), L), _, L = []), length(L, C).
left(_, C) :- catch(findall(x, f(_), L), _, L = []), length(L, C).

main :-
	getenv('DBS_T', TS), atom_number(TS, T),
	getenv('DBS_N', NS), atom_number(NS, N),
	getenv('DBS_R', RS), atom_number(RS, R),
	( between(1, R, _), t(T, N), fail ; true ),
	left(T, C),
	format("~w~n", [C]),
	halt.
PLEOF

# Peak RSS in KB of a run, by polling: peak_rss <test> <rounds> <size>.
# Prints "<peak> <last line of output>".
peak_rss() {
	_out=$(mktemp /tmp/db-stress-out-XXXXXX)
	DBS_T=$1 DBS_N=$3 DBS_R=$2 "$TPL" -q -f "$PL" >"$_out" 2>&1 &
	_pid=$!
	_peak=0
	while kill -0 "$_pid" 2>/dev/null; do
		_rss=$(ps -o rss= -p "$_pid" 2>/dev/null | tr -d ' ')
		case "$_rss" in
			''|*[!0-9]*) ;;
			*) [ "$_rss" -gt "$_peak" ] && _peak=$_rss ;;
		esac
		sleep 0.05
	done
	wait "$_pid"
	echo "$_peak $(tail -1 "$_out")"
	rm -f "$_out"
}

name_1='1. retractall'
name_2='2. abolish'
name_3='3. retract all'
name_4='4. retract by key'
name_5='5. clause'
name_6='6. match'
name_7='7. churn without emptying'
name_8='8. arity 2, both side lists'

# clauses expected to survive a round
want_1=0; want_2=0; want_3=0; want_4=0; want_5=0; want_6=0; want_7=1; want_8=1

pass=0
for t in 1 2 3 4 5 6 7; do
	eval "name=\$name_$t; want=\$want_$t"

	case $t in 8) size=$N8 ;; *) size=$N ;; esac

	set -- $(peak_rss "$t" "$R_LO" "$size"); lo=$1; got_lo=$2
	set -- $(peak_rss "$t" "$R_HI" "$size"); hi=$1; got_hi=$2

	if [ "$got_lo" != "$want" ] || [ "$got_hi" != "$want" ]; then
		echo "$name: WRONG left $got_lo/$got_hi want $want"
	elif [ "$lo" -le 0 ]; then
		echo "$name: NO MEASUREMENT"
	elif [ $(( hi * 100 )) -gt $(( lo * LIMIT )) ]; then
		echo "$name: GREW $(( hi * 100 / lo ))% of ${R_LO}-round peak over $R_HI rounds"
	else
		echo "$name: constant"
		pass=$(( pass + 1 ))
	fi
done

echo "db-stress: $pass/8 constant memory"
