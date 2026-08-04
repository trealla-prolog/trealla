# Indexing review — `redesign-indexing` @ `4858906`

Fresh clone, every create / destroy / insert / remove / lookup site for the four
indexes (`idx1`, `idx1a`, `idx2`, `wild2`) walked against the docs.

One crashing bug, two coverage gaps, one quantified waste, and a set of invariants
the code relies on but does not state.

---

## 1. BUG — dangling `idx1a` / `wild2` on file reload (crashes)

`check_not_multifile()`, `src/module.c:1751`:

```c
sl_destroy(pr->wild2);
sl_destroy(pr->idx2);
sl_destroy(pr->idx1a);
sl_destroy(pr->idx1);
pr->idx2 = pr->idx1 = NULL;      // idx1a and wild2 left dangling
```

All four are destroyed, only two are nulled. Every *other* teardown site nulls all
four — `query.c:631`, `module.c:376`, `module.c:917`, `bif_database.c:330`. This one
was missed when `idx1a` and `wild2` were added.

Two ways it bites:

- `destroy_predicate()` later runs `sl_destroy(pr->wild2)` on the freed list — double free.
- `idx2_add()` only allocates `if (!pr->wild2)`, so the next var-arg2 assert appends
  into freed memory.

**Reachable on the ordinary Logtalk/Prolog reload path**: a static, non-multifile
predicate not named `$…`, redefined from a different file (or on `is_reload`), with
≥ 500 clauses. Same family as the `purge_properties()` bug from earlier in this work.

Reproduced (`a.pl`/`b.pl`/`drv.pl` — 600 × `foo(N,_)` then 600 × `foo(N,b)`):

```
Warning: overwriting 'foo'/2
got(b)
[b]
timeout: the monitored command dumped core
```

ASan:

```
ERROR: AddressSanitizer: heap-use-after-free
READ of size 8
    #0 sl_destroy            src/skiplist.c:134
    #1 destroy_predicate     src/module.c:396
freed by thread T0 here:
    #1 sl_destroy            src/skiplist.c:158
    #2 check_not_multifile   src/module.c:1747
    #3 assertz_to_db         src/module.c:2453
```

Fixed, ASan clean after.

## 2. Flags not cleared at two teardown sites

`is_key_var` / `is_key_var1` / `is_key_var2` are cleared at `bif_database.c:333`,
`query.c:633`, `module.c:2198`, `module.c:2296` — but **not** at
`abolish_predicate()` (`module.c:376`) or `clear_property()` (`module.c:917`), which
clear only the two older root-only flags.

Not a crash: the flags are conservative barriers, so a stale `true` costs an index,
never soundness, and `assert_commit()` recomputes them on the next build. Included in
the fix so all five teardown sites read the same.

## 3. GAP — `--index-check` never verified the idx2 fallback path

`find_key()`, `src/query.c:1908`:

```c
return collect_hits(q, pr, key, iter, (idx == pr->idx1) ? 1 : 0);
```

`0` means *do not check*. So the classic "goal arg1 unbound, key on arg2 instead"
lookup — the whole `is_var(arg1) || is_var_in_first_arg` branch above it — was the one
indexed path `--index-check` could not see. Changed to `2`; verified clean on a
3000-clause ground predicate (`gg.pl`), which now reports 2 verified lookups where it
reported 1.

The other unverified path is the capped `idx1a` bail (`rc == -1`) — that one is correct
to skip, the result set is deliberately incomplete there.

Worth knowing regardless: the trealla suite only produces **5 verified lookups total**
at threshold 500. The checker is only meaningful against eyelet / Logtalk / sarif.

## 4. WASTE (quantified) — `idx1a` is built for every predicate, read for almost none

`idx1a` is created unconditionally at build time (`module.c:2280`, `if (pr->key.arity)`),
but the only read is `query.c:1836`, which sits **inside** `if (pr->is_key_var) { … }`.
So for any predicate whose clause heads are all ground — the common case — `idx1a` is
built, inserted into on every assert, removed from on every retract, and never once
consulted.

Measured, 500k ground `g/2` facts, this branch vs. the same branch with `idx1a`
creation `#if 0`'d:

| | MAXRSS | assert loop |
|---|---|---|
| as-is | 208.9 MB | 0.73s |
| no `idx1a` | 182.8 MB | 0.62s |
| **cost** | **+26.1 MB (+14%)** | **+0.11s (+18%)** |

Concrete fix: the bulk loop already computes `is_key_var` as it goes, so after the loop
`if (!pr->is_key_var) { sl_destroy(pr->idx1a); pr->idx1a = NULL; }`, plus a rebuild on
the incremental path the first time `is_key_var` flips false → true. That is the only
transition that can make it wanted again.

## 5. Unstated invariants the code depends on

Each of these holds today. Each is one edit away from being unsound, and none is
written down.

**(a) `!is_key_var` ⟹ `wild2` empty.** The idx2 fallback at `query.c:1897` sets
`idx = pr->idx2` and **never merges `wild2`**. That is only safe because `idx2_add()`
diverts every var-arg2 clause into `wild2`, and no head holds a var on this path, so
`wild2` is empty. Give `idx2` a var-bearing clause on this path and it silently drops
solutions. One comment.

**(b) `!is_key_var1` ⟹ `idx1a` is complete.** `idx1a_add()` *returns without adding*
when the clause's arg1 holds a var. So `collect_hits_cap` returning 0 → `find_key`
returning `false` is only correct because the `!pr->is_key_var1` guard means no such
clause exists. This is the load-bearing one: get it wrong and lookups return "no
matching clause".

**(c) Nested vars in the goal's arg1 are sound.** `query.c:1836` tests `!is_var(a1)`
— bare-var only — so `p(f(X), …)` descends `idx1a` with a partial key. Sound, because
all clause arg1s are ground, the comparator truncates at the goal's first var, and
lexicographic prefix comparison is monotone in the full order, so the matching run is
contiguous. The idx2 branch carries exactly this argument in a comment; the idx1a
branch does not.

**(d) `recheck_var_in_indexed_args()` has two early `return`s** (`module.c:2234`,
`2246`) that predate the three new flags and now sit *after* they are computed. Both
happen to be safe — `is_var_in_first_arg && is_var_in_second_arg` implies all three
key-var flags are already true, and arity 0 means the head can never hold a var. But
the function's whole job is to avoid under-reporting, and these two lines under-report
by construction if either premise ever shifts. Hoist the key-var scan above them.

**(e) `do_abolish()` frees before it destroys.** `predicate_purge_dirty_list()`
(`bif_database.c:191`) frees clauses with no `index_remove_clause()`, and the
`sl_destroy` block runs *after* it (`bif_database.c:328`). Safe only because
`sl_destroy()` never compares keys and nothing in between does either. Moving the
`sl_destroy` block above the purge costs nothing and removes the ordering dependency.

## 6. Checked and clean

- **Solution order through the `wild2` merge.** `tmp_idx` and `wild2` both key on
  `db_id` with the default (integer) comparator, and asserta's negative ids sort ahead
  of assertz's positives, so the merge really is in database order. Verified on a
  3000-clause predicate with 428 side-list clauses — output byte-identical to a
  no-index oracle, full list not just counts (`ord.pl`). Also verified with `wild2`
  *under* the 1/10 cap so the merge actually runs (`w2.pl`, 675 = 600 + 75). The
  `append` flag threaded through `idx1a_add`/`idx2_add` is therefore cosmetic —
  `collect_hits_cap` re-sorts by `db_id` regardless.
- **Iterator ownership.** `iter_exhausted` / `iter_release` / the `rc == -1` bail all
  release correctly; `sl_done` on a tmp list destroys it. The one leak
  (`!sl_next` after a non-empty `tmp_idx`) is unreachable.
- **`merge_wild2` across choicepoints.** Set immediately before `collect_hits`, cleared
  at its entry, so it is always false in any `run_state` that gets snapshotted.
- **Every clause-free path removes its entries first.** Six sites; the two `UNDO_RULE`
  frees (`query.c:999`, `query.c:2509`) are covered upstream by `leave_predicate`'s
  `index_remove_clause`.
- **db-stress memory.** No per-round index leak: the gap between this branch and the
  no-index oracle is a *constant* ~18 MB at 2, 8 and 24 rounds. The linear growth
  itself (~16 MB/round) is present in the no-index build too — pre-existing, not this
  work.

## Verification of the fix

| check | result |
|---|---|
| trealla suite | 335 pass / 1 pre-existing failure |
| chess.pl | `7d198dd5` — unchanged |
| reload repro under ASan | clean (crashed before) |
| `idx.pl` / `ord.pl` / `w2.pl` / `gg.pl` vs no-index oracle | identical, order included |
| `--index-check` sweep over suite + new corpora | 0 mismatches |
| db-stress rounds 2 / 8 / 24 | constant delta vs oracle |

## Suggested order

1. `check_not_multifile` null-out — **crashing, ship it.**
2. Flag clears at the two teardown sites — same patch, trivial.
3. `--index-check` on the idx2 fallback — free coverage.
4. Lazy `idx1a` — 26 MB and 18% of assert cost on ground predicates.
5. The five comments in §5, especially (b).
6. Still open from before: prefix keys for `$predicate_property/3` (~1.1s of sarif),
   and a `wild1a` side list to do for `idx1a` what `wild2` did for `idx2` — one
   var-arg1 clause in 30,000 currently disables arg1 indexing outright, which is the
   same cliff `wild2` was built to remove.
