# Redesigning the dynamic database index

Grounded in four defects traced this session. Unification hashes were considered and
rejected — see the appendix for why, and for the two things worth taking from them.

## What the current design gets wrong

1. **Keys are borrowed `cell *` pointers into clause cells.** An entry outliving its
   clause makes the next descent a use-after-free. Every free path has to withdraw
   entries first; `query_purge_dirty_list()` and `purge_properties()` did not.
2. **Ordering is a deep term comparison returning 0 for vars.** `[_]` equals both `[a]`
   and `[b]`, which differ from each other. Not a total order, so no position in a
   sorted structure satisfies every query.
3. **Removal needs a comparison-driven descent.** Under duplicate keys — the normal
   case, since every clause sharing a functor collides — `sl_rem()` failed on 394 of
   400 removals.
4. **Lookups allocate**, one `sl_create` per call to restore database order.
5. **The iterator is an owning pointer in `run_state`**, snapshotted into every
   choicepoint. A cut frees it; a later retry walks freed memory.

And one property that is *right* and must survive: the deep key discriminates. `f(k1,_)`
and `f(k2,_)` separate on the ground sub-argument, worth 12x over a linear walk. Every
attempt to fix correctness by excluding var-bearing clauses failed on exactly this.

**The diagnosis is narrow: the index is sound in concept and broken in its treatment of
variables.** The redesign should be correspondingly narrow.

## The four changes

### 1. Key on a flattened ground prefix — variables never enter a comparison

Walk `arg1` left to right, emitting tokens (functor/arity, atom, small int), **stopping
at the first variable** or at a depth cap. That token sequence is the key. A clause is
filed under the longest prefix its own groundness supports:

| clause `arg1` | key |
|---|---|
| `f(k1,z)` | `[f/2, k1, z]` |
| `f(k1,_)` | `[f/2, k1]` |
| `[a,b]` | `['.'/2, a, '.'/2, b]` |
| `[a\|_]` | `['.'/2, a]` |
| `[_]`, `[_\|_]` | `['.'/2]` |
| bare `X` | `[]` |

A goal computes its own key to depth *d* and probes **every prefix length 0..d**. That
is what makes `f(k1,_)` reachable from a fully ground goal while `[_]` still matches
everything: each clause sits at the depth its groundness earns, and the goal looks at
every depth it could have been filed under.

No variable is ever compared, so defect 2 becomes unrepresentable rather than fixed.
Probes are `d+1`, not `2^d`, because a clause stops at its *first* variable — that is
the deliberate trade, and it costs precision only for shapes like `f(_, k9)`, where the
ground tail after a variable goes unused. Bounded, and a precision loss rather than a
correctness one.

Depth cap 3 to start; tune against Logtalk and `chess.pl`.

### 2. Intrusive doubly-linked membership — removal without comparison

```c
rule *idx_prev, *idx_next;   /* bucket chain */
uint32_t idx_bucket;         /* where it lives */
```

`sizeof(rule)` goes 144 → 168, so 24 bytes per clause, 24MB per million.

Removal becomes pointer surgery. **This is what makes borrowed keys safe again**: keys
are only dereferenced during lookup, when every clause in the structure is live. The
dangerous case was always removal, which had to compare its way to a node while other
clauses were being freed around it. Take comparison out of removal and defects 1 and 3
go together, along with the withdraw-before-free discipline, the two-pass purge, and the
reason `clear_property()` destroys the whole index.

Buckets hold clauses in `db_id` order: `assertz` appends, `asserta` prepends, both O(1).

Hash table over exact prefix keys, not a skiplist — probes become O(1) instead of
O(log n), and there is no ordered structure left to get wrong. Power-of-two sizing,
double when load exceeds ~2.

### 3. Lazy k-way merge — no allocation per lookup

Up to `d+2` buckets must come back in database order. Hold one cursor per bucket and
emit the minimum `db_id`:

```c
typedef struct { rule *cur[MAX_PROBES]; uint8_t k; } merge_iter;
```

`k ≤ 5`. Each step is a five-way min — no allocation, no sort. A goal that wants one
solution pays for one, which the current prefetch-everything design cannot do.

### 4. One iterator, embedded by value — the integration point

The linear walk is not going away: sub-threshold predicates, unbound-`arg1` goals and
consulted predicates all use it. Both paths must present one interface.

```c
typedef struct {
    enum { ITER_CHAIN, ITER_MERGE } kind;
    union { rule *chain; merge_iter merge; };
} clause_iter;
```

Held **in `run_state` by value, not as a pointer**. Snapshotting into a choicepoint then
copies it: nothing to own, nothing to free, nothing to dangle. That is the structural fix
for the `initialization_1` crash — today's `sliter *iter` is an owning pointer duplicated
into every choicepoint with no ownership story, and the patch we shipped closes only one
of the two frees.

Both kinds apply the same logical-update-view test (`dbgen_created` /
`dbgen_retracted`), so indexed and linear stay identical by construction.

## Staging

1. **`clause_iter` by value, linear path only.** No index involvement, no behaviour
   change. Gets the owning pointer out of `run_state` and fixes the crash properly.
2. **`--index-check`.** Every indexed lookup also runs the linear walk and compares
   candidate sets, aborting on mismatch. Cheap once both paths share an interface, and
   it would have caught all four defects on their first run. Assert the indexed set is a
   *superset* of the linear set's matches — approximations must widen, never narrow.
3. **Prefix keys + intrusive buckets behind a flag**, default off. Run the suite,
   Logtalk, `nested_var_bug.pl` and the 700-clause repro under `--index-check`.
4. **Flip the default**, keep the skiplist path one release.
5. **Delete the skiplist index and `index_cmpkey_`** — ~200 lines of comparator
   including every wildcard special case.

## Risks

- **Representation agreement.** A string and a code/char list that unify must produce
  the same tokens. Same trap `index_cmpkey_` fell into with `is_string` vs
  `is_iso_list`. Most likely source of a silent miss; needs tests across
  `double_quotes` and partial strings.
- **Numeric agreement.** Small int, bigint, rational, float that unify must key alike.
  Safest start: tokenise small ints and atoms precisely, and treat anything else as
  "stop the prefix here" — always sound, just less selective.
- **Attributed variables** must tokenise as plain variables, or `freeze/2` and `dif/2`
  goals get filtered out.
- **Precision loss after a variable.** `f(_, k9)` files at depth 0. If that shape turns
  out to be common in Logtalk's tables, revisit — see the appendix.
- **Threads.** Bucket mutation stays under `module_lock`; the existing refcnt and
  dirty-list discipline is unchanged by intrusive links.

## Appendix: unification hashes, and why not

Hendricks' scheme (Golog, 2013) gives each term an N-bit hash, splitting the bit space
between functor and arguments recursively. Clause-head variables hash to all 1s ("I
supply anything"), goal variables to all 0s ("I demand nothing"), and the test is
`(query & clause) == query` — Bloom semantics, false positives only.

It is elegant and it handles `f(_, k9)`, which the prefix scheme above does not. It was
rejected on five grounds, the first of which is decisive and comes from having actually
built it:

- **Updates.** This was tried in early trealla. Lookup was fast; maintenance was not.
  The reason is structural, not incidental: subset matching does not partition. A clause
  whose variable slices are all 1s satisfies a large family of query patterns, so it
  cannot be filed in one bucket — any structure that avoids the linear scan has to
  enumerate supersets, and keeping that coherent under assert and retract is
  combinatorial. The scheme is only cheap to maintain in the form where it is a flat
  filter over every clause, which is precisely the form that gives up sub-linear lookup.

And on analysis:

- **It is a filter, not an index.** Every lookup touches every clause. For trealla that
  is a complexity-class regression from O(log n). Measured on this machine with the
  filter inline in a 168-byte `rule` walked as a linked list: 0.81 µs at 1,000 clauses,
  75 µs at 10,000, 2.56 ms at 100,000. The constant is tiny right up until it isn't.
- **Bit dilution undercuts its own motivating example.** 64 bits split recursively by
  arity. In `married([tim|_], [_,sue|_])`, `sue` ends up with roughly 6 bits — 1-in-64
  selectivity on the exact term the example exists to demonstrate. Precision collapses
  with depth, which is the opposite of the claim.
- **The cost is memory traffic, not the AND.** "Hundreds of clause hashes per
  unification" holds only while hashes are cache-resident. The jump from 0.8 to 7.5
  ns/clause in the table above is entirely cache misses. Recovering it needs a packed
  array parallel to the chain — another structure to keep coherent under assert and
  retract, which is the exact class of bookkeeping that produced this session's bugs.
- **No stateable worst case.** False positives are data-dependent and unbounded.

Two things worth taking:

1. **The preparation/query asymmetry.** A clause-head variable *supplies*; a goal
   variable *demands*. That framing is why the probe set above is "every prefix length
   up to my own depth" rather than something symmetric, and it is a clearer way to
   reason about the direction of the relation than the current comparator's
   var-equals-anything.
2. **Cheap tokens.** Use a small integer's low bits directly rather than hashing it, and
   do not spend key space on the `'.'/2` functor — pairs nest too deeply to pay for it.

Held in reserve: a 64-bit unification hash makes a decent *secondary* filter **within**
an oversized bucket, if profiling ever shows buckets large enough to matter. That keeps
it additive rather than load-bearing.

Source: http://blog.ndrix.com/2013/03/prolog-unification-hashes.html
