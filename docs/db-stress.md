# db-stress — checkpoint (not finished)

Stopped on a usage limit mid-way. Tests 1–7 are done and validated; test 8 needs one
more change, described below. **Nothing has been deleted from `samples/` yet** and no
`.expected` file exists yet, so the suite is untouched — `db-stress.sh` currently just
sits alongside it.

## What works

`tests/slow/db-stress.sh` — shell test, as you chose, so no source change. `run.sh`
already handles `*.sh` via `env TPL=$TPL bash`. The Prolog is embedded in a heredoc
rather than living at `tests/slow/db-stress.pl`, because `run.sh` globs `tests/slow/*`
and would otherwise pick the `.pl` up as a test in its own right and diff it against the
same `.expected`.

All six original workloads are preserved. Memory is checked by running each at 2 rounds
and at 6 and comparing peak RSS, sampled with `ps -o rss=` (KB on both Linux and macOS —
`/usr/bin/time`'s memory flags are not portable between GNU and BSD). Fails over 1.20x;
measured noise is ≤1.01x. Whole script runs in ~30s, well inside `timeout 300`.

**Test 7 is new and is the one that matters.** All six original tests drain their
predicate to empty, and emptying it destroys every index wholesale at `cnt == 0` — which
sweeps up any entry that was never withdrawn, hiding exactly the bug the guard is for.
Test 7 churns without ever emptying. Validated by reintroducing the original regression
(per-clause removal from `idx1a` deleted):

| | tests 1–6 | test 7 |
|---|---|---|
| fixed build | constant | **constant** |
| bug reintroduced | constant — *all six miss it* | **GREW 149%** |

That is the whole value of the exercise: the original six could not have caught the
memory regression you reported.

## What's left — test 8

Test 8 covers arity 2, so `idx2` and the two side lists engage (tests 1–7 are all arity
1). It currently reports `GREW 129%` **on the fixed build**. That is not an index leak.
Chased it to the bottom:

- Growth is *worse* with indexing disabled entirely (880 KB/round vs 215), so it is not
  the index.
- Isolated to: **retracting a clause that contains a variable does not reclaim, when the
  predicate never drops to empty.** Goal side is innocent.

`retract-var-clause-leak.pl` is the minimal reproducer — three variants, 6000 clauses,
rounds 1 / 6 / 14:

| | 1 | 6 | 14 |
|---|---|---|---|
| ground clause, ground goal | 7808 KB | 7808 KB | 7808 KB |
| ground clause, **var goal** | 7936 KB | 7936 KB | 7936 KB |
| **var-bearing clause** | 7936 KB | 15872 KB | 28672 KB |

Pre-existing — reproduces on tip `4858906` and with no index at all. I did not get to
confirm how far back it goes.

**The fix for test 8**: split it in two, so it stops tripping on a leak it isn't meant
to measure.

- **t8** — churn *ground* arity-2 clauses, with a small fixed set of var-bearing clauses
  asserted once to keep `wild1a` and `wild2` populated so the merge path stays live.
  Memory-checked. Covers `idx1`/`idx1a`/`idx2` removal under churn.
- **t9** — churn the var-bearing clauses. **Correctness-checked only** (exact clause
  count after churn), memory deliberately not checked, with the reason documented
  inline. Still catches a wrong-clause-removed bug in `wild1a`/`wild2` — the same class
  as the original `sl_rem` duplicate-key bug.

Then: write `tests/slow/db-stress.expected`, delete `samples/db-stress.pl`, run the full
suite to confirm 336/1.

## Separate finding — the side-list merge is quadratic

Found while sizing test 8, worth its own look. A side list under the 1/10 cap is merged
into **every** lookup — drained and re-sorted into a fresh `tmp_idx` skiplist each time —
so cost per lookup scales with the predicate. Arity-2 predicate, ~5% of clauses in the
side lists, one fill-and-drain round:

| N | time |
|---|---|
| 5000 | 0.07s |
| 20000 | 0.56s |
| 50000 | 7.50s |

At 200000 it does not finish. **Pre-existing, and it is `wild2`, not `wild1a`** — tip
`4858906` measures 7.01s against this build's 7.50s at N=50000.

The implication is that the 1/10 cap being a *ratio* is wrong: for a 200k-clause
predicate a "small" 10% side list is 20,000 entries drained on every single lookup. It
probably wants to be `min(ratio, absolute)`. This compounds the cap finding in
`wild1a.md`, which argued from a 20000-clause benchmark that the ratio cap was too
*tight* — both can be true, and together they say the cap wants to be re-derived rather
than nudged.
