# Trealla first-argument indexing — state, and what's left

Written after tracing and fixing a series of defects in the ordered index. Supersedes
the earlier speculative version of this document: everything below is measured.

## Where it stands

Branch `redesign-indexing`, seven files, ~467 insertions against `9dd481c`.

**Logtalk `tools/sarif`, 22 tests:**

| | cpu | wall |
|---|---|---|
| original — silently returning wrong answers | 10.54s | 11.52s |
| correct, but predicates barred from the index | 14.80s | 15.65s |
| **shipped** | **8.17s** | **9.05s** |

22% faster than the starting point, and correct. **eyelet:** 88 cases, 73,546 indexed
lookups verified against a brute-force scan, zero mismatches; `deep-taxonomy-100000`
2.30s against 2.38s originally. **trealla suite:** 334 pass / 1 pre-existing failure.
**chess:** byte-identical throughout.

## The defects, in the order they were found

1. **`sl_rem()` could not remove under duplicate keys.** The descent advanced while
   `cmp <= 0`, walking past the whole equal-key run at any level where the target was
   absent. It failed on **394 of 400** removals in an isolated unit test, and
   first-argument indexing is nothing but duplicate keys. *(`skiplist_dup_key_test.c`)*
2. **`index_cmpkey_()` was not antisymmetric.** `cmp(list, f/2)` and `cmp(f/2, list)`
   were both positive, so the descent walked straight past list-headed clauses.
3. **`purge_properties()` spliced into the wrong predicate** — it walks the
   `$predicate_property/3` chain but updated `pr->head`/`pr->tail` of the predicate being
   purged, then `unload_realfile()` ran `abolish_predicate()` over the wreckage. Only
   reachable on file reload.
4. **`query_purge_dirty_list()` freed clauses without withdrawing index entries**, and
   `leave_predicate()` freed an iterator aliased by every choicepoint — a use-after-free
   that crashed Logtalk's `initialization_1`.
5. **Single- and multi-candidate lookups leaked their prefetch.** 4.7MB per 14,000
   retracts, 7.2MB per 300 multi-candidate lookups.
6. **Flags were set-only.** `is_var_in_first_arg` had this fix already and documents why;
   `is_key_var` repeated it. `$directive/1`: 2131 live clauses, none var-bearing, index
   disabled anyway.

## The root cause, demonstrated

`index_cmpkey_()` returns 0 at the first variable it reaches. That makes
`cmp(node, goal)` **non-monotonic** along the clause order the skiplist is built on. A
skiplist descent is a binary search and requires monotonicity. `TPL_SL_TRACE` on the
failing Logtalk lookup:

```
[sl] L0 advance over node cmp=-62
[sl] L0 stop, next cmp=64
[sl] landed, cmp=64 wild_card=0
```

It steps over −62, lands on +64, and the clauses that compare 0 are further along past
nodes at +115. No descent can work against that. `sl_next_key()`'s
`if (!wild_card && ok != 0)` — accepting any node once a wildcard was seen — is a patch
over the same wound, and is why non-candidates came back with `cmp = -10`.

**The condition for soundness:** a goal holding a variable is safe *only if* the clauses
matching its ground prefix are contiguous, which holds when every clause key is ground.
One var-bearing clause key was placed by comparisons calling it equal to everything it
met, so it sits at an arbitrary point and the matching run is no longer contiguous.

## What ships

- `sl_rem()` descends on strict `<`, walks the equal-key run for the exact
  `(key, value)` pair, carries `update[]` forward, unlinks at every level.
- `index_cmpkey_()` walks the list branch only when both sides are lists.
- `purge_properties()` delinks from `pr2`, in two passes.
- `clause_iter` held by value in `run_state`, with the ownership rule stated once;
  single-candidate lookups don't materialise; the prefetch is released when the goal's
  own choicepoint is dropped.
- `is_key_var` bars **idx1** (keyed on the whole head) for predicates holding a
  var-bearing key; recomputed, not set-only.
- **`wild2`** — clauses whose arg2 carries a variable are held in a `db_id`-keyed side
  list and merged into each **idx2** result, so the ordered index still carries the rest.
  Falls back past a tenth of the predicate.
- Deterministic skiplist: per-list seed from a process counter, xorshift32 level
  generation. `TPL_SKIPLIST_RANDOM=1` restores entropy for fuzzing.

## Diagnostics, and how to use them

**`--index-check`** verifies every indexed lookup against a brute-force scan:
`indexed set == { c in pr->head : index_cmpkey(head(c), key) == 0 }`. It tests the
structure against the comparator, which is where every defect lived. Reports and
continues, prints the goal, the missed clauses, what was returned, `cmp` for each, and
whether a missed clause is reachable by its own key — which separates a descent fault
from a placement fault. Counts verified lookups, because zero mismatches over zero
lookups says nothing: **chess never crosses the index threshold at all**, so early clean
sweeps of it were meaningless.

**`TPL_INDEX_STATS=1`** reports per predicate: clause count, percentage var-bearing in
the whole head / arg1 / arg2, lookups, and linear fallbacks.

**`TPL_SL_TRACE=1`** logs the `sl_find_key` descent. Noisy — one failing case at a time.

`--index-check` covers **idx1, idx2 and the `wild2` merge**, comparing against the key each index was
built on — the whole head for idx1, arg2 alone for idx2 — with merged side-list entries
recorded alongside the idx2 hits so the merge itself is covered. A missed clause that
lives in the side list says so, rather than claiming it was mis-filed. Verified by
sabotage: disabling the merge is caught and names both dropped clauses. eyelet coverage
went 12,088 → 34,092 verified lookups on the same corpus, and sarif — which reported
nothing at all beforehand, since idx1 is disabled for every predicate carrying its load —
now verifies **69,001 lookups, 0 mismatches**.

Checking costs roughly 45% on sarif (11.49s against 7.91s): a brute-force scan and an
allocation per lookup. Debug aid, not something to leave on.

## What's left

**One predicate, ~1.1s of sarif's 8.17s.**

```
$predicate_property/3:  7670 clauses  head=98.1% arg1=0.0% arg2=98.1%  7814 lookups, all linear
```

A property variable in arg3 disqualifies every head, so idx1 is barred. Neither cheap
index shape rescues it: arg1 is `predicate`/`function`, about two distinct values; arg2
is 98% var-bearing. It wants the whole-head discrimination it had originally — 0.03s on
a 20,000-lookup benchmark against 0.68s linear — which is exactly what is unsound.

**That needs prefix keys**, and this is now a concrete case rather than a theoretical
one. File each clause at the deepest **ground prefix** it supports, stopping at the first
variable: `('$predicate_property', predicate, foo/1)` here, which is the whole
discriminating part, with the trailing property variable costing nothing. Lookup becomes
an equality probe on a bucket rather than an ordered descent, so monotonicity is never
required and the entire class of defect above becomes unrepresentable.

Design sketch, in the order I would build it:

1. **Prefix key.** Walk arg1 left to right emitting tokens (functor/arity, atom, small
   int), stopping at the first variable or a depth cap of ~3. Clauses file under **every**
   prefix length 0..d; goals probe every prefix length 0..their own d, and results are
   unioned with dedup. Both directions are needed: a clause-side variable files shallow,
   a goal-side variable probes shallow, and only the intersection is correct.
2. **Intrusive membership.** `idx_prev`/`idx_next` on `rule` — 144 → 168 bytes, 24MB per
   million clauses. Removal becomes pointer surgery. This is what makes borrowed keys
   safe: they are only dereferenced during lookup, when every clause is live. Removal
   comparing its way to a node while clauses are freed around it was the whole hazard.
3. **Lazy k-way merge** instead of prefetch-and-sort — no allocation per lookup, and a
   goal wanting one solution pays for one.
4. **Delete** `index_cmpkey_` and the skiplist index, ~200 lines including every wildcard
   special case.

Smaller items, independent of the above:

- `--index-check` costs ~2% when off, since the branch sits in the lookup path.
  `#ifndef NDEBUG` around the two call sites removes it.
- The 500 threshold is a count-only heuristic. With the index cheaper to maintain it
  could trigger on observed lookups instead, so cold predicates never pay build cost.
- `#include <math.h>` in `skiplist.c` is now unused.

## Rejected, with measurements — so they aren't retried

| approach | why not |
|---|---|
| **Unification hashes** (Hendricks, Golog) | Tried in early trealla: lookup fast, updates a nightmare. Subset matching doesn't partition — a clause whose var slices are all 1s satisfies a family of query patterns, so anything avoiding a linear scan must enumerate supersets, and keeping that coherent under assert/retract is combinatorial. Also a filter, not an index: O(n) per lookup, 0.81 µs at 1k clauses, 75 µs at 10k, 2.56 ms at 100k. Bit dilution undercuts its own example — `sue` in `married([tim\|_],[_,sue\|_])` gets ~6 bits. |
| **Bar the index when the *goal* holds a var** | Correct, but 13x on assert/retract churn — `retract(f(I,_))` has a var, so every retract goes linear. The guard belongs on the clause side. |
| **Wild bucket on arg1 with a 10% fallback** | 65x on a predicate where every clause is var-bearing; and Logtalk's tables are var-bearing in arg1 specifically, so it degenerated exactly where it was needed. Works on **arg2**, which is what shipped. |
| **Leftmost-leaf heuristic** instead of whole-key | Kept the speed but is unsound: `f(1,_)` against a goal `f(1,2)` reaches the var once sub-arg 1 matches. No proof behind it, and the repro caught it. |
| **idx1a, keyed on arg1 alone** | 2.61s against 0.68s for the linear walk it replaced. `$predicate_property/3`'s arg1 has ~2 distinct values, so it returns half the predicate and materialises it. Sound and useless. |
| **Index hysteresis / sticky `is_indexed`** | Segfaulted. Stock already keeps the index unless `cnt` hits zero, and the empty-refill case is O(n) amortised against the n asserts that refilled it — it measured as a wash. |

## A note on method

Three separate fixes failed the same way: I put the guard on the **goal** side when it
belonged on the **clause** side. Soundness depends on how clauses are *placed*, not on
what a particular goal looks like. Each time the goal-side version was either far slower
or left the path unreachable for the exact shape it was written for.

And `clause-steps` (linear lookups × clauses) is a good *relative* signal between
predicates and a poor absolute one — a step is a unification attempt at roughly 18ns.
60M steps reads as enormous next to 488M but is about 1.1s. Convert before prioritising.

## Files

`CURRENT/` — the seven changed files plus `all-changes.patch`.
Repros: `nested_var_bug.pl`, `index_repro_700.pl`, `skiplist_dup_key_test.c`,
`wild2.pl`, `predicate_property_bench.pl`.
