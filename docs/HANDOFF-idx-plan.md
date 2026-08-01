# Plan: Real first-arg indexing (option 1)

Status: draft for review — not implemented.

## Problem

After ~500 dynamic clauses, Trealla builds skiplist indexes:

| Index | Key | Role today |
|-------|-----|------------|
| `idx1` | full head | primary lookup |
| `idx2` | 2nd arg | when 1st arg is unbound |

For queries like `e(G, _, _)` with ground `G`, `find_key` (`src/query.c`) uses **idx1**, finds all equals via wildcard compare, then **rebuilds a temporary skiplist sorted by `db_id`** so results are in database order. That re-sort is paid even for `e(G, _, _), !`.

SWI-style first-arg indexing: ground Arg1 → **chain of clauses already in assert order**. First solution is O(1) to the head of the chain; enumeration is O(k). No collect, no sort.

### Evidence (`/tmp/idx_bench`, 600×90 = 54k facts, 90 matches for `e(1,_,_)`)

| Engine | `e(1,_,_),!` | `e(1,1,2),!` | matches |
|--------|-------------:|-------------:|--------:|
| SWI | ~0.06 µs | ~0.06 µs | 90 |
| Trealla stock | ~6 µs | ~0.5 µs | 90 |
| Scryer | ~0.2 µs | ~1.5 µs | 90 |

giso_07 compile is dominated by `setof` / walks over `e(Graph, _, _)`. Microbench alone is not the gate; giso_07 wall time is.

## Why the idx0 stopgap was dropped

A third skiplist (`idx0`) keyed only by Arg1, used only when Arg1 is atomic and args 2..n are vars:

- Helped the microbench cut path (~6 µs → ~0.2 µs) once bugs were fixed.
- Did **not** change the model (still skiplist equals-walk, not a pre-ordered clause chain).
- Narrow query shape; did not show clear giso_07 benefit.
- Hit tmp-heap key lifetime bugs with `findall` (must keep `q->st.key` on the original call term).

## Goal

**Real first-arg index:** `Arg1 → bucket of rules in database order`.

- `e(G, _, _)` / `e(G, X, Y)` / `e(G, 1, 2)` with ground `G`: walk that bucket; unify filters remaining args.
- No per-query collect + `db_id` re-sort for the ground-Arg1 path.
- Match SWI’s cost shape on the bench and cut giso_07 **compile** time.

## Non-goals (this plan)

- Full SWI JITI / deep multi-argument indexes.
- Vector + `qsort` stopgap (old option 2) — obsolete if this lands.
- `assertz` throughput / parse-phase wins (separate follow-up if parse still dominates after compile improves).
- Changing static clause indexing.

## Design

### Data structures

```
predicate
  idx_fa      // map: first-arg key → fa_bucket*  (skiplist or hash)
  fa_var      // intrusive list of clauses whose Arg1 is a variable
  idx2        // keep for Arg1-unbound + Arg2-bound
  idx1        // phase A: leave in place but unused on ground-Arg1 path
              // phase B: stop building for dynamic preds if profiling allows

fa_bucket {
  rule *head, *tail;   // linked via rule->fa_next (optional fa_prev)
}
```

- Index keys: borrowed pointers into clause head cells (same discipline as today).
- Only **non-var** clause Arg1s go into `idx_fa`; var Arg1s go on `fa_var` and set `is_var_in_first_arg` (or equivalent).
- Build at the existing dynamic index threshold (500), or earlier once stable.

### Maintain on DB updates

| Operation | Action |
|-----------|--------|
| `assertz` | append rule on Arg1’s bucket (or `fa_var`) |
| `asserta` | prepend |
| `retract` | unlink from bucket / `fa_var` (explicit list unlink — do not rely on skiplist multi-key `sl_rem`) |
| `abolish` / destroy predicate | destroy map; clear links |

### Lookup (`find_key`)

| Query shape | Path |
|-------------|------|
| Arg1 ground, and we can use first-arg index | Lookup bucket(Arg1); start at `bucket->head`; also walk `fa_var` if non-empty. Unify full head (filters Arg2..n). Iterate via `rule->fa_next`. |
| Arg1 unbound, Arg2 ground | Existing **idx2** |
| Else | Linear `pr->head` |

**Hard requirements** (from stopgap fallout):

1. Keep `q->st.key` on the **original call term**, never a tmp-heap clone (`findall` / `sys_queue` call `init_tmp_heap` between solutions).
2. Prefer iterating **`dbe->fa_next`** over a live skiplist iterator (choice points already save `dbe`).
3. Use the first-arg path for **any** ground Arg1, not only “rest are vars”.

### What happens to idx1

- **Phase A:** Add `idx_fa`; route ground-Arg1 through it; leave idx1 built but unused on that path (safer rollback).
- **Phase B:** Stop building idx1 for dynamic predicates (or keep only if a profiled niche still needs full-head keys). Exact `e(1,1,2)` walking a ~90-clause bucket is acceptable (SWI does that).

### Correctness hazards

- Ground Arg1 queries must still see clauses in `fa_var`.
- Map keys must be non-var clause Arg1s only — do not let `index_cmpkey` wildcards collapse distinct ground keys in the map.
- Retract while choice points hold `dbe`: keep existing `dbgen` / `can_view` rules.
- Avoid skiplist duplicate-key `sl_rem` for chain membership; unlink `fa_next` explicitly.

## Implementation order

1. Add `fa_bucket` + `rule->fa_next`; maintain on assert / retract / abolish.
2. `find_key`: ground Arg1 → bucket walk; leave `q->st.key` alone; wire choice/retry via `fa_next`.
3. Wire `fa_var` for clauses with var Arg1.
4. Green `/tmp/idx_bench/run.sh` vs SWI (and stock Trealla baseline).
5. giso_07 before/after (compile phase is the real gate).
6. Optional phase B: drop idx1 build for dynamic preds.

## Success metrics

| Check | Target |
|-------|--------|
| `matches_e1` | 90 |
| `e(1,_,_),!` | ~SWI/Scryer flat (≪ stock ~6 µs) |
| `e(1,1,2),!` | no bad regression |
| `findall e(1,_,_)` | clearly under stock ~33 µs |
| **giso_07 compile** | large cut vs ~2.9 s stock if `e(G,_,_)` dominates |

Parse may remain assert-bound; that is out of scope for this plan.

## Files likely touched

- `src/internal.h` — `fa_bucket`, `rule->fa_next`, `predicate.idx_fa` / `fa_var`
- `src/module.c` — build/maintain on `assert_commit` and destroy paths
- `src/query.c` — `find_key` / `next_key` / `has_next_key` / retract unlink
- `src/bif_database.c` — abolish cleanup
- Tests: lean on `/tmp/idx_bench` + existing dynamic DB tests; add a small issues test if useful

## Open questions for review

1. Skiplist vs hash for `idx_fa` map? (Skiplist reuses `index_cmpkey`; hash may be faster for ints/atoms.)
2. Keep idx2 as-is, or eventually fold into a second-arg story?
3. Lower index threshold below 500 once `idx_fa` is trusted?
4. Should phase B (drop idx1) be in the first PR or a follow-up?
