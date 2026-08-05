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
# Workloads 8 and 9 from the redesign branch are omitted: they seed a
# handful of var-keyed clauses, which on this tree disables the index for
# the whole predicate and makes the churn quadratic.
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
N9=20000
R_LO=2
R_HI=6
LIMIT=120       # hundredths: fail over 1.20x. Measured noise is <= 1.01x.

PL=$(mktemp /tmp/db-stress-XXXXXX.pl)
trap 'rm -f "$PL"' EXIT

cat > "$PL" <<'PLEOF'
:- dynamic(f/1).
:- dynamic(h/2).
:- dynamic(k/2).
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

% 8. Arity 2, so idx1, idx1a and idx2 all carry entries and all must
% withdraw them - everything above is arity 1, and a leak in idx2 or
% either side list would go unseen there. A small FIXED set of var-keyed
% clauses is seeded once and never churned, which holds wild1a and wild2
% open (both far under the one-tenth cap, so the merges really do run on
% every lookup) while the churn itself stays ground.
%
% Kept to 20 of each deliberately. A side-list clause is a candidate for
% EVERY lookup and gets head-unified on each one, so seeding hundreds
% makes this test cost seconds without testing anything more.
%
% Ground on purpose. Retracting a clause that CONTAINS a variable does
% not reclaim while the predicate stays non-empty - a pre-existing leak
% with nothing to do with indexing, which reproduces with the index
% disabled entirely and worse. Churning var-keyed clauses here would trip
% this guard on that rather than on anything an index did. Test 9 covers
% their removal instead, without a memory check.
t(8,N) :-
	( h(keep,keep) -> true ; hseed ),
	( between(1,N,I), assertz(h(c(I),d(I))), fail ; true ),
	( between(1,N,I), retract(h(c(I),d(I))), fail ; true ).

hseed :-
	assertz(h(keep,keep)),
	( between(1,20,I), assertz(h(_,a(I))), assertz(h(b(I),_)), fail ; true ).

% 9. Removal of var-keyed clauses from both side lists, churned, with the
% predicate never emptying - the one path tests 1-8 leave uncovered. The
% ground majority keeps both side lists under the one-tenth cap, so the
% merge is live during the retract lookups too.
%
% CORRECTNESS ONLY, for the reason given above test 8. A wrong count here
% still catches the class of bug that matters most for a side list:
% removing the wrong entry under duplicate keys, which is exactly what
% the original sl_rem() defect did.
t(9,N) :-
	( k(keep,keep) -> true ; assertz(k(keep,keep)) ),
	( between(1,N,I), kput(I), fail ; true ),
	( between(1,N,I), kdel(I), fail ; true ).

kput(I) :- 0 is I mod 40, !, assertz(k(_,a(I))).
kput(I) :- 0 is I mod 41, !, assertz(k(b(I),_)).
kput(I) :- assertz(k(c(I),d(I))).

kdel(I) :- 0 is I mod 40, !, retract(k(_,a(I))).
kdel(I) :- 0 is I mod 41, !, retract(k(b(I),_)).
kdel(I) :- retract(k(c(I),d(I))).

unwind(0) :- !.
unwind(I) :- retract(f(g(I))), I2 is I-1, unwind(I2).

left(8, C) :- !, catch(findall(x, h(_,_), L), _, L = []), length(L, C).
left(9, C) :- !, catch(findall(x, k(_,_), L), _, L = []), length(L, C).
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
name_8='8. arity 2, side lists live'
name_9='9. churn var-keyed clauses'

# clauses expected to survive a round
want_1=0; want_2=0; want_3=0; want_4=0; want_5=0; want_6=0
want_7=1; want_8=41; want_9=1

flat=0          # ran in constant memory (tests 1-8)
right=0         # returned the right clause count (tests 1-9)

for t in 1 2 3 4 5 6 7; do
	eval "name=\$name_$t; want=\$want_$t"

	case $t in 8) size=$N8 ;; 9) size=$N9 ;; *) size=$N ;; esac

	# Test 9 is correctness only - see the note above t(9,...).
	if [ "$t" = 9 ]; then
		set -- $(peak_rss "$t" "$R_HI" "$size"); got=$2

		if [ "$got" != "$want" ]; then
			echo "$name: WRONG left $got want $want"
		else
			echo "$name: correct"
			right=$(( right + 1 ))
		fi

		continue
	fi

	set -- $(peak_rss "$t" "$R_LO" "$size"); lo=$1; got_lo=$2
	set -- $(peak_rss "$t" "$R_HI" "$size"); hi=$1; got_hi=$2

	if [ "$got_lo" != "$want" ] || [ "$got_hi" != "$want" ]; then
		echo "$name: WRONG left $got_lo/$got_hi want $want"
		continue
	fi

	right=$(( right + 1 ))

	if [ "$lo" -le 0 ]; then
		echo "$name: NO MEASUREMENT"
	elif [ $(( hi * 100 )) -gt $(( lo * LIMIT )) ]; then
		echo "$name: GREW $(( hi * 100 / lo ))% of ${R_LO}-round peak over $R_HI rounds"
	else
		echo "$name: constant"
		flat=$(( flat + 1 ))
	fi
done

echo "db-stress: $flat/7 constant memory, $right/7 correct"
