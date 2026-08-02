Introduction
============

In Prolog systems there are two types of indexing used: 1) program-like
and 2) data-like. They are of a radically different nature. Trealla
makes this distinction.


Program-like indexing
=====================

This is the familiar lookup of rules that constitute a predicate. It
has to handle backtracking and allow efficient and effective early
choice-point elimination. Things that affect this capability are
dynamic and multifile predicates. Program-like predicates are usually
of limited size, typically at most a few hundred to a few thousand
clauses.

Relevant today in Trealla:

- Uniqueness over the full term (`cl->is_unique`) and early det via
  ground arg checks in `has_next_key`
- First-/second-arg indexes (`pr->idx1`, `pr->idx2`) built once the
  predicate crosses a clause-count threshold
- Ground queries (in full or part) vs var in arg1/arg2 (falls back to
  the linear `pr->head` chain when the index is unusable)


Data-like indexing
==================

This is more akin to a traditional database based on a key-value type
search, possibly with duplicates. Backtracking is usually not an
issue, and predicates may be dynamic. The number of such rules may be
in the tens of thousands plus.

Points still to go over:

- Logical-update semantics (transactional view / `dbgen`)
- When deleted clauses (garbage) are collected


Current implementation (program-like)
=====================================

Clauses for a predicate are linked in assert order via `pr->head` /
`rule->next`. Each rule has a `db_id` (assertz positive and growing,
asserta negative) that defines *database order* — the order clause
selection must obey.

When the clause count crosses a threshold (~500), `assert_commit`
builds:

- `pr->idx1` — skiplist keyed by the full head cell (`index_cmpkey`)
- `pr->idx2` — skiplist keyed by arg2, if arity > 1

`sl_app` / `sl_set` insert into those skiplists; a threshold rebuild
walks `pr->head` and `sl_app`s every live clause. Var-headed arg1/arg2
set `is_var_in_first_arg` / `is_var_in_second_arg` and force `find_key`
off the index (a var compares equal to everything, so the skiplist is
not a total order).

`find_key` (`src/query.c`) does the lookup. On a hit it does *not*
hand the skiplist iterator straight to matching. Instead:

```text
sl_find_key(idx, key)
  → while sl_next_key: sl_app(tmp_idx, db_id, rule)
  → iterate tmp_idx in db_id order
```

That temporary skiplist is the subject of the redesign below.


Why the temporary skiplist exists
=================================

**Key order ≠ clause order.**

`idx1` / `idx2` are ordered by `index_cmpkey`. Matching clauses are
yielded in *key-sort* order. Prolog requires trying them in *database*
order (`db_id` / assert order).

That mismatch is unavoidable with the current shape whenever more than
one key (or one key with several colliding entries) matches the probe:

- Partial instantiation — `index_cmpkey` treats a var as equal to any
  value, so one probe can span many key regions
- Distinct heads that share an indexed prefix (e.g. arg1-only use of
  the index) but differ later
- Mixed `asserta` / `assertz` after a rebuild — equal-key chain order
  in the skiplist is not a reliable stand-in for `db_id` order

So today every multi-hit lookup **prefetch + re-sort by `db_id`**.
Single-hit could skip the tmp list (the comment in `find_key` already
says so) but the code path still builds it.


Redesign: eliminate the temporary skiplist
==========================================

Goal: matching iterates candidates **already in database order**, with
no query-time skiplist alloc for the common cases.

Option 1 — Key → db-ordered bucket (best fit for today’s shape)
---------------------------------------------------------------

Index value is not a single `rule*`, but a list or vector of clauses
**already in `db_id` order** (assertz append, asserta prepend; rebuild
from `pr->head`).

- Ground key, one bucket: walk the bucket — done, no tmp list
- Several buckets match (var / weak compare): **k-way merge by
  `db_id`** (heap of size *k*), not “insert everything into a
  skiplist”

Same asymptotics when many buckets hit; no alloc when one bucket or
one hit. Keeps the skiplist-of-keys idea; fixes the *value* side.

Option 2 — Composite key `(index_key, db_id)`
---------------------------------------------

Order the skiplist by index key, then `db_id`. A ground lookup is a
contiguous range already in clause order. Multi-key matches are still
several ranges → merge as in option 1. Cleaner than a side structure;
heavier compare and insert.

Option 3 — Index as a skip into the clause chain
------------------------------------------------

Keep `pr->head` as the source of truth (always db order). Index maps
key → first matching `rule*`, plus `next_same_key` links maintained on
assert/retract.

- Ground first-arg (or whatever the index covers): walk that subchain
  only — already db order
- Var in the indexed arg: full chain, or another index

Classic try/retry-chain + arg-index model. **No reorder step** for the
common case. More pointer maintenance on assert/retract; simplest
query-time story.

Option 4 — Dense `db_id` + posting lists
----------------------------------------

Clauses in a vector; index key → sorted `db_id[]` (or bitmap). Lookup
yields sorted ids; multi-key = merge/intersect. Attractive for large
dynamic/data-like predicates; more machinery than Trealla has now.


Recommendation
==============

- Prefer **option 3** if the aim is to delete the tmp skiplist with the
  least query-time policy change: matching stays “follow pointers in
  db order.”
- Prefer **option 1** if keeping a skiplist-of-keys is desirable and
  only the value side needs to become db-ordered buckets (with k-way
  merge for multi-bucket probes).

Avoid relying on `sl_app` / `sl_set` equal-key chain order as a proxy
for `db_id` — asserta/assertz plus the threshold rebuild already make
that fragile, which is why the temporary list exists.

Code anchors: `find_key` in `src/query.c`, `assert_commit` /
`index_cmpkey` in `src/module.c`, skiplist in `src/skiplist.c`.
