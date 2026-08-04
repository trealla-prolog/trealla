# db-stress

`tests/misc/db-stress.sh` — the database and index stress workloads, with a memory
regression guard. Replaces `samples/db-stress.pl`, which ran six workloads but only ever
reported that they finished.

## Why it exists

All six original workloads are meant to run in **constant memory**: each round fills a
predicate and empties it again, so round 10 should cost no more than round 1. Nothing
checked that, and an index that failed to withdraw its entries went unnoticed until it
was found by hand.

## How it checks

Each workload runs at two round counts and its peak RSS is compared. Sampled with
`ps -o rss=`, which is KB on both Linux and macOS — `/usr/bin/time`'s memory flags are
not portable between GNU and BSD. Fails over 1.20x; measured noise is ≤1.01x.

It is a `.sh` rather than a `.pl` because nothing in trealla reports process memory:
`statistics/2` offers `heap`, `frames`, `slots`, `trails`, `choices`, and all of them
are blind to the database. During a run that reaches 79 MB RSS, `statistics(heap, H)`
returns 84. The clause and index memory is `malloc`'d and unaccounted.

The Prolog is embedded in a heredoc rather than living beside the script, because
`run.sh` globs `tests/misc/*` and would otherwise pick a `.pl` up as a test in its own
right and diff it against the same `.expected`.

## The nine workloads

1–6 are the originals, unchanged: `retractall`, `abolish`, `retract` all, `retract` by
key, `clause/2`, and plain matching.

**7 — churn without emptying.** All six originals drain their predicate to empty, and
emptying it destroys every index wholesale at `cnt == 0`, which sweeps up any entry that
was never withdrawn. That hides exactly the class of bug this guard is for. Test 7 holds
one clause back so the predicate never reaches zero and every index has to withdraw
entry by entry.

Verified to discriminate: with per-clause removal from `idx1a` deleted, tests 1–6 all
stay flat at 1.00x and test 7 grows to 1.50x. **The original six could not have caught
the memory regression that prompted this.**

**8 — arity 2, side lists live.** Everything above is arity 1, so `idx2` and the two
side lists never engage and a leak in any of them goes unseen. Twenty var-keyed clauses
of each kind are seeded once and never churned, holding `wild1a` and `wild2` open so the
merges really run on every lookup, while the churn itself stays ground.

Twenty, not hundreds, deliberately: a side-list clause is a candidate for **every**
lookup and is head-unified on each one. At 100 of each, this test cost 4.8s for 5000
clauses without testing anything more.

Ground churn on purpose — see the note on test 9.

**9 — churn var-keyed clauses.** Removal from both side lists, with the predicate never
emptying: the one path tests 1–8 leave uncovered. **Correctness only.** It still catches
the bug class that matters most for a side list — removing the wrong entry under
duplicate keys, exactly what the original `sl_rem()` defect did — but memory is not
checked, because retracting a clause that *contains* a variable does not reclaim while
the predicate stays non-empty. That is a separate pre-existing leak with nothing to do
with indexing: it reproduces with the index disabled entirely, and worse. Churning
var-keyed clauses under a memory check would trip test 8 on that rather than on anything
an index did.

## What test 8 found on its first run

It failed on a clean build, and the cause was a real leak rather than a bad threshold.

The retract commit in `bif_database.c` paired `leave_predicate()` with `drop_choice()` —
the same pair as the working commit path in `match_clause()` — without releasing the
clause iterator first. `leave_predicate()` opens with `iter_reset()`, which NULLs the
handle *without freeing it*, so the prefetch was abandoned. `iter_release()` exists for
precisely this and was called from one of the nine sites that pair those two calls.

It only bites on a **merged** lookup. Without a side list a ground lookup returns a
single candidate and takes `iter_set_single`, which owns nothing; with one it returns
many and takes `iter_set_sl`, which owns a `tmp_idx` skiplist. So the symptom was: a
predicate holding even a handful of var-keyed clauses leaked on every assert or retract
of any *other* clause. The var-keyed clauses are never themselves retracted — their
presence alone is what turns a cheap lookup into a prefetched one.

| 2000 ground clauses churned per round | 2 rounds | 8 rounds |
|---|---|---|
| before | 13.8 MB | 37.8 MB |
| after | 6.5 MB | 6.5 MB |

Reverting the fix takes test 8 from 1.02x to 2.8x, so the guard detects the bug it was
written to detect.

## Known gaps

- Seven more sites pair `leave_predicate` with `drop_choice` without releasing:
  `query.c` 2034 and 2233, `bif_database.c` 99 and 176, and two cut/prune paths at
  `query.c` 1197 and 1248. The last two act on `ch->st.pr` rather than `q->st.pr` and
  raise a separate question — the choicepoint being dropped may own an iterator in
  `ch->st.iter` that nothing frees. Only the retract path is fixed, being the one test 8
  proves.
- Tests 8 and 9 run at 20000 clauses rather than 200000. A side list under the
  one-tenth cap is merged into every lookup with a head unification per entry, so cost
  per lookup grows with the predicate and these go quadratic at 200000. Pre-existing
  behaviour of the side-list merge, not something these tests are trying to measure.
