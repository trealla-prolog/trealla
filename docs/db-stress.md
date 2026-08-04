# db-stress — done. Test 8 passes, and it found a real leak on the way.

Against branch tip `31c46a59` (which already has `wild1a`). Test 8 was failing on a
clean build; the answer was not to loosen the test.

```
1. retractall: constant
2. abolish: constant
3. retract all: constant
4. retract by key: constant
5. clause: constant
6. match: constant
7. churn without emptying: constant
8. arity 2, side lists live: constant
9. churn var-keyed clauses: correct
db-stress: 8/8 constant memory, 9/9 correct
```

## The leak test 8 found

`bif_database.c`'s retract commit did `leave_predicate()` then `drop_choice()` — the
same pair as the working commit path in `match_clause()` — but without releasing the
clause iterator first. `leave_predicate()` opens with `iter_reset()`, which NULLs the
handle **without freeing it**, so the prefetch was abandoned.

`iter_release()` exists for exactly this and was called from **one** of the nine sites
that pair `leave_predicate` with `drop_choice`.

It only bites on a *merged* lookup. Without a side list a ground lookup returns one
candidate and takes `iter_set_single`, which owns nothing; with one it returns many and
takes `iter_set_sl`, which owns a `tmp_idx` skiplist. So the trigger is: **a predicate
holding even a handful of var-keyed clauses leaked on every assert/retract of any other
clause.** The var-keyed clauses are never themselves retracted — their presence alone is
enough, because they are what turns a single-candidate lookup into a prefetched one.

Fix is three lines plus an exported wrapper (`iter_release` is `static inline` in
`query.c`, the call site is in `bif_database.c`).

| | 2 rounds | 8 rounds | |
|---|---|---|---|
| before | 13.8 MB | 37.8 MB | leaks |
| after | 6.5 MB | 6.5 MB | flat, and lower |

`merge-prefetch-leak.pl` is the 8-line reproducer.

## Verification

| check | result |
|---|---|
| trealla suite | 335 / 1 pre-existing — unchanged |
| chess.pl | `7d198dd5` — unchanged |
| db-stress | 8/8 constant, 9/9 correct |
| **discrimination** | fix reverted → t8 goes 61 MB → 172 MB (2.8x) and **fails** |

That last row is the one that matters: the test detects the bug it was written to
detect, and the fix is what makes it pass.

## What's in the test now

**Test 8** — arity 2, so `idx1`/`idx1a`/`idx2` all carry entries and all must withdraw
them; tests 1–7 are arity 1 and cannot see a leak in `idx2` or either side list. Twenty
var-keyed clauses of each kind are seeded once and never churned, holding `wild1a` and
`wild2` open so the merges really run, while the churn stays ground.

Twenty, not hundreds, deliberately: **a side-list clause is a candidate for every lookup
and is head-unified on each one.** At 100 of each, test 8 cost 4.8s for 5000 clauses. That
is the real shape of the merge cost — sharper than the "drained and re-sorted" framing in
my earlier note, and it strengthens the case that the 1/10 cap being a *ratio* is wrong.

**Test 9** — churns the var-keyed clauses themselves. Correctness only, because it still
trips the separate pre-existing leak below. It catches the bug class that matters most
for a side list: removing the wrong entry under duplicate keys, exactly what the original
`sl_rem()` defect did.

## Left open

- **Seven more sites** pair `leave_predicate` with `drop_choice` and do not release:
  `query.c` 2034 and 2233, `bif_database.c` 99 and 176, plus two cut/prune paths at
  `query.c` 1197 and 1248 that act on `ch->st.pr` rather than `q->st.pr`. I fixed only
  the retract path — the one test 8 proves — and left the rest alone rather than
  guess. The two `ch->st.pr` ones are a different question: the choicepoint being
  dropped may own an iterator in `ch->st.iter`, which nothing currently frees.
- **A second, unrelated leak**: retracting a clause that *contains* a variable does not
  reclaim while the predicate stays non-empty. Reproduces with indexing disabled
  entirely, and worse — so not an index bug. `retract-var-clause-leak.pl`. This is why
  test 9 is correctness-only.
- `docs/db-stress.md` on the branch is the old checkpoint note and is now stale in three
  ways: it says `tests/slow`, says `samples/` is undeleted, and its
  "retracting a var-bearing clause leaks" diagnosis is superseded by the table above.
