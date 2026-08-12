# Review: Phase 1 tabling implementation (`bif_tabling.c`)

Static review of the shipped file, ~1343 lines. **Nothing here was
executed** — the working tree was recycled, so every finding below is
from reading. The ones marked *unverified* need a repro before being
believed; I have written the repro I would use for each.

---

## 1. Table handles are unvalidated raw pointers — memory corruption from pure Prolog

```c
static table *tbl_handle(cell *c)
{
	return (table*)(size_t)c->val_uint;
}
```

Every `'$tbl_*'` builtin that takes a handle does this and then
immediately dereferences. The header comment says handles "only ever
flow between `'$tbl_*'` builtins", which is true of the *intended*
callers and false of what the code permits.

`'$tbl_add_answer'/2` and friends are ordinary entries in the builtins
table. `struct builtins_` has `iso`, `evaluable`, `ffi` and
`via_directive` flags — **there is no hidden/internal flag**. The `$`
prefix is a naming convention, nothing more, and Trealla's own README
documents users calling `'$dlopen'/3`, `'$register_function'/4` and
`'$parse_url'/2` directly. So the convention is not one users have been
taught to respect.

Which means this is reachable:

```prolog
?- '$tbl_add_answer'(12345, foo).       % arbitrary pointer, dereferenced
?- '$tbl_set_status'(0, complete).      % NULL deref
```

Compare how streams handle exactly this problem: an integer cell
carries `FLAG_INT_STREAM` and `get_stream()` validates before use.
Tabling has no equivalent — no tag, no range check, no registry
membership test.

Severity is high not because a user is likely to do it deliberately,
but because the failure is memory corruption rather than an error term,
and because a *stale* handle produces the same effect by accident (see
§5).

Fix directions, cheapest first: flag the handle cell the way streams
do and reject unflagged integers; or keep handles as indices into a
per-thread array and bounds-check; or validate membership in
`s->all_tables` (O(n), but these are not hot builtins).

**Repro:** `tpl -g "'\$tbl_set_status'(0,complete),halt"` — expect a
segfault, want a `type_error`/`existence_error`.

---

## 2. Recursion depth is bounded by *answer* size, not call size

`trie_walk()` recurses on term depth, with the comment:

> Recursive on term depth; tabled calls are shallow.

That justification holds for *calls*. But the same function is reached
from `'$tbl_add_answer'` for every **answer**, and answers are not
shallow — a list is right-nested `'.'/2`, so an answer that is a
10,000-element list recurses 10,000 frames on insert.

`trie_free()` has the same shape and is worse:

```c
static void trie_free(tnode *n)
{
	while (n) {
		tnode *sib = n->sibling;
		trie_free(n->child);        // recurses on trie DEPTH
		...
```

Siblings iterate, children recurse. A trie holding one long-list answer
is a *chain*, so depth ≈ term size, and the whole chain is freed by
recursion at table teardown.

A tabled predicate returning long lists is not exotic — it is the first
thing anyone tries with a tabled DCG. This is a plain stack overflow
(SIGSEGV, no error term) at some list length I cannot determine without
running it.

**Repro:** table a predicate that yields `numlist(1, N, L)` for
increasing N and find where it dies. Worth doing before Phase 2, since
item 6 (trie-path reconstruction) would add a *third* recursive walk
over the same structure.

*Unverified — this is the finding I would most want to confirm by
execution.*

---

## 3. OOM in `thash_grow` silently desynchronises the index from the sibling list

In `trie_step`, after a node is created and linked:

```c
if (h) {
	if (h->count >= h->nbuckets - h->nbuckets/4) {
		if (!thash_grow(w->q, h)) { w->oom = true; return false; }
	}

	thash_insert(h, n, key_hash(w->q, key));
}
```

If `thash_grow` fails, the function returns **before** `thash_insert`.
The node is already in `*slot` (the sibling chain) but is not in the
hash index. Every later lookup goes through the index (because `h` is
non-NULL), misses it, and creates a **second node with the same key**.

At that point the trie has duplicate keys, and duplicate keys break the
one thing the answer trie exists for: `*existed` in `trie_insert_` no
longer detects a repeat, so a tabled predicate can return the same
answer twice.

Contrast the sibling case: if `trie_index_children()` fails,
`parent->index` stays NULL and lookups continue via the sibling scan —
correct, just slower. Only the `thash_grow` path corrupts.

OOM-only, so consistent with other OOM paths deliberately left alone
elsewhere in the codebase. Worth distinguishing though: this one does
not fail, it silently returns wrong answers afterwards.

Fix is one line — insert into the old table on grow failure rather than
returning:

```c
if (!thash_grow(w->q, h)) { thash_insert(h, n, hv); w->oom = true; return false; }
```

or simply don't treat a failed *grow* as fatal, since the index is only
an optimisation.

---

## 4. A failed insert leaves partial paths in the trie

`trie_insert_` bails out cleanly:

```c
if (!ok || !leaf)
	return NULL;
```

but `trie_walk` may already have created nodes for the arguments it
processed before the one that failed. Those nodes stay. They are not
marked `is_leaf`, so they never surface as answers, and I do not
believe they cause a wrong result — a later successful insert down the
same prefix still creates its own leaf and reports `existed` correctly.

But they are never reclaimed until the table is destroyed, so a program
that repeatedly calls a tabled predicate with an untabelable answer
(an attvar, a blob, a stream) grows the trie without bound while
throwing on every call.

Low severity, easy to miss, and it is exactly the kind of thing a
restraint (Phase 2 item 1) will make more visible rather than less.

---

## 5. `'$tbl_get_answer'` validates the generation only on retry

```c
if (q->retry && (q->st.v2 != s->generation))
	return false;

tbl_ans *a = q->retry ? (tbl_ans*)(size_t)q->st.v1 : t->first_ans;
```

The guard protects an enumeration in progress across an abolish — good,
and clearly deliberate. But on the **first** call `t` is dereferenced
with no check at all, so a handle obtained before an
`abolish_all_tables/0` and used after it is a use-after-free:

```prolog
?- '$tbl_variant_table'(foo, H, _), abolish_all_tables, '$tbl_get_answer'(H, A).
```

This is the same root cause as §1 — handles carry no validity
information — but it is reachable without inventing a bogus integer,
which makes it more likely to happen by accident.

---

## 6. An undocumented invariant that Phase 2 will break

`'$tbl_get_answer'` pushes a choice point only when there is a next
answer:

```c
if (a->next) { ...; CHECKED(push_choice(q)); }
```

That is a nice touch — no trailing choice point on the last answer.
It is only sound because answers are **immutable once complete**, and
`get_answer` is only used on completed tables. If an answer is appended
after an enumeration has passed what was then the tail, it is never
seen.

Nothing states this invariant. Phase 2 item 3 (answer subsumption,
which *updates* existing answers) and batched scheduling (posting
answers before completion, discussed under item 1) both violate it
directly. Worth a comment now, while the reason is still fresh.

---

## 7. Shadowed `s` in `bif_tbl_pop_worklist_1`

```c
tbl_state *s = tbl(q);
...
for (tbl_ans *a = t->unproc_ans; a; a = a->next) {
	for (tbl_susp *s = t->first_susp; s; s = s->next) {   // shadows tbl_state *s
```

Compiles and behaves correctly today. It is a trap for the next person,
and `pop_worklist` is precisely the function Phase 2 items 3 and 7 will
be editing. Rename the inner one.

---

## 8. Smaller notes

- `t->functor = is_interned(p1) ? p1->val_off : 0;` — a non-interned
  callable variant records functor 0, which `abolish_table/1` then
  cannot match. Probably unreachable if all callables are interned;
  worth an assertion rather than a silent 0.
- In `bif_tbl_variant_table_3`, the table is allocated and linked into
  `s->all_tables` *before* the two `unify` calls. If the first unify
  fails the table survives with no owner. Harmless (it is found again
  next call) but it is a side effect on a failure path, and the pattern
  we spent this week removing elsewhere was exactly that.
- `key_hash` for a non-bigint `TAG_INT` uses `c->val_int >> 32` on a
  signed 64-bit value. Arithmetic shift on gcc/clang, so fine in
  practice; implementation-defined in principle.

---

## What is well done

Worth saying, because the parts that are right are the parts that are
usually wrong in a trie implementation:

- **The `key_eq`/`key_hash` contract is carefully maintained.** Atoms
  arriving interned or as cstrings hash their *text* so both
  representations collide correctly; strings xor a constant so they
  cannot collide with atomish keys of the same text. I checked the
  cross-cases (interned-vs-cstring, string-vs-atomish, compound
  functors) and could not find a pair where equality and hashing
  disagree.
- **The float comparison is bitwise, not `==`, and the comment explains
  why**: `key_hash` hashes the bit pattern, so `==` would merge
  `0.0`/`-0.0` and never match NaN, and indexed lookups would miss.
  That is a subtle bug avoided deliberately.
- **`tbl_pin_answer_frame`** and its comment are the best thing in the
  file — the interaction between imported answer variables and frame
  trimming is genuinely hard, the reasoning about why `no_recov` does
  not propagate from inside a builtin is correct, and it is documented
  where the next person will need it.
- **Attributed variables are rejected rather than silently mistabled.**
  Right call, clearly reasoned in the comment.
- The **worklist cross-product** (new answers × all suspensions, then
  old answers × new suspensions) is the correct way to avoid
  re-pairing, and reads clearly.

---

## Recommended order

1. §1 handle validation — it is the only finding that is memory-unsafe
   from ordinary Prolog, and the fix is small.
2. §2 recursion depth — verify first; if it bites at plausible list
   lengths it outranks everything.
3. §3 `thash_grow` — one line, removes a silent-wrong-answer path.
4. §6 and §7 — comments and a rename, minutes, and both are in code
   Phase 2 will touch.
5. §4, §5, §8 — as convenient.
