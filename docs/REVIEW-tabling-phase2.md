# Review of DESIGN-tabling-phase2.md

Self-review, checked against the shipped `bif_tabling.c` rather than
from memory. Findings ordered by severity. Four are substantive enough
that I would not implement from the design as written.

---

## Severe

### 1. The recommended order is probably wrong

The design puts **shared completed tables second**, justified as "the
single biggest win available". Reviewing it honestly, it is the worst
choice for second place:

- it has the most dangerous failure mode on the list — a cross-thread
  use-after-free, which is silent, intermittent and invisible to ASAN
  when the `pl_destroy()` sweep hides it;
- it is worth **nothing** to a single-threaded user, which is most
  users and every WASM embedding;
- it is the one item whose central design question the document
  explicitly fails to answer (see §3 below).

Items 3 (subsumption) and 4 (incremental) give more users more value at
materially lower risk. Revised order:

    1 restraints → 3 subsumption → 4 incremental → 2 sharing → 6 → 7

That also groups the two items that touch `bif_tbl_add_answer_2`
(1 and 3) adjacently, which the design notes is desirable and then
separates anyway.

### 2. The locking claim is imprecise, and as written describes a race

The design says a reader "checks the registry first and, on a hit,
reads it without a lock". Taken literally that is a data race: the
registry itself is mutable shared state, and reading a pointer out of
it unsynchronised gives no happens-before edge to the table *contents*
written by the publishing thread.

What actually makes it safe is narrower and needs saying precisely:

- the table is fully built and never written again **before**
  publication;
- publication happens under a mutex;
- **lookup also happens under that mutex** — it is the acquire against
  the publisher's release that makes the contents visible;
- only after lookup returns does the reader touch the table, and by
  then it is immutable.

So it is not "read without a lock", it is "one short lock to find it,
then no lock to use it". The distinction is the whole correctness
argument and the current wording invites someone to implement the racy
version.

### 3. Refcount vs epoch is not decided, and the obvious choice fails

The design offers "a refcount, or an epoch scheme" and moves on. That
is the single most dangerous part of the item and it is left open.

Worse, refcounting has a specific problem the document does not
identify: **a reader can hold a table across `completion/0`**, which
runs arbitrary user code and may block on I/O, suspend, or throw. So a
refcount can be held for an unbounded time, and
`abolish_all_tables/0` cannot wait on it without risking a deadlock
against user code. That pushes hard toward deferred reclamation —
publish a new generation, let readers finish on the old one, free when
the last pre-generation reader retires — reusing the `generation`
counter already on `tbl_state`.

The design should pick one and justify it, not list both.

### 4. Restraints interact with SCC merging, and this is unaddressed

The design says a restraint breach raises, and that
`'$tbl_reset_incomplete'` already exists for the exception path. That
covers the simple case. It does not cover the case Phase 1 found
hardest: a fresh variant completed in a **nested SCC** whose tables
are merged into the parent on pop when a suspension targets an outer
SCC.

If a restraint fires inside a nested SCC that has already merged, which
tables are incomplete? The inner ones? The merged set? The design
needs to answer that explicitly, because it is precisely the region
where Phase 1's bugs lived, and "reset_incomplete already exists" is
an assumption rather than a verified claim.

---

## Moderate

### 5. Incremental tabling hand-waves the hard part

"A hook in the clause-retrieval path" is the easy half. The hard half
is knowing **which table you are currently computing**, and the design
assumes that is available.

It is not simply "the current table on a stack". A suspended consumer
resumes later via `delim/3`, running on behalf of a table that is not
lexically current at that moment. So the dependency must be attributed
to the table the *continuation* belongs to, not to whatever is on top
of a stack when the dynamic predicate is called. That is a real design
problem and the document does not acknowledge it exists.

### 6. Answer subsumption understates the trie change

The design frames subsumption as a change to the insert logic in
`bif_tbl_add_answer_2`. But the answer trie is currently keyed on the
**whole answer term** — that is what gives free duplicate detection.
Subsumption needs it keyed on the *non-aggregated* arguments only, so
that `path(a,b,3)` and `path(a,b,5)` collide and can be combined.

That is a structural change to how the answer trie is used, not just
to what happens after a lookup. It may mean a second index, or a
different key construction per tabled predicate. The design should
say so; as written, someone would start in the wrong place.

### 7. Item 6 muddles the memory win with the time cost

Verified in the source: `tbl_image` does `copy_term_to_tmp` then
`TPL_malloc` then `dup_cells` — a genuine full second copy of every
answer, in addition to its trie path.

So the **memory win from dropping images is unconditional** — the trie
exists either way. Only the *retrieval cost* is a trade. The design
says "which way that goes depends entirely on the ratio of answers
stored to answers consumed", which is true of the time and false of the
memory, and reads as though the whole item might be a wash. It is not;
the question is only whether the reconstruction cost is acceptable.

The recommendation to measure first is still right, but it should
measure the right thing: reconstruction time per answer, not whether
the memory saving exists.

### 8. The threadless build is never mentioned

`bif_tabling.c` already has `#if USE_THREADS` around the per-thread
state. Item 2 is meaningless under `NOTHREADS` and must compile out
cleanly — and `NOTHREADS` is the WASI configuration, which is what
diagramide ships. Given that config could not even link at `-O0` until
this week, it deserves an explicit line in any design that touches
thread state.

### 9. No feature-flag or rollback story

The design flags item 7 as the one that "changes existing behaviour",
then offers no way to turn it off. Phase 1 gated native tabling behind
a flag checked in `start_tabling`. Each behaviour-affecting Phase 2
item should say whether it is flag-gated and what the default is —
particularly WFS, where "correct" and "what your program did last
release" may differ.

### 10. "Nearly free" is an unmeasured claim on a hot path

The restraint counters thread through `twalk`, which runs on every
answer insert. The struct has room (it already carries `oom` and
`attvar` flags), so the change is easy — but "nearly free" is asserted,
not measured, in a document whose own cross-cutting notes say to
measure before optimising. State it as an expectation with a
measurement attached.

---

## Minor

### 11. No test baseline

"Add to `tests/misc/tabling.pl`" appears throughout with no statement
of what is there now (17 checks, including the four-thread concurrency
test with its marker-count negative control). Without the denominator,
"add a test" has no target.

### 12. Effort estimates are one word each

S/M/L/XL with one sentence. For items 1 and 6 that is probably fine.
For items 4 and 7 the range between a good week and a bad month is
what actually determines whether they get started, and the design
should decompose them further before either is scheduled.

---

## What holds up

- The **case for restraints first** is right, and the argument — that a
  diverging table is currently indistinguishable from a bug — is the
  strongest justification in the document.
- **Deferring WFS to last** is right, and for the right reason: it is
  the only item that rewrites completion.
- The **"don't do" list** (GMP, subgoal abstraction, call subsumption)
  is appropriately short and appropriately justified.
- The **`throw_error()` returns TRUE** warning is worth its space:
  items 1 and 3 both add error paths to the same function, so it will
  come up again.
- Recommending **measurement before building item 6** is right even
  though the framing of what to measure is wrong.

---

## Verdict

The shape is sound and the priorities are defensible, but three things
need fixing before anyone implements from it: the ordering (§1), the
locking argument (§2), and the reclamation decision (§3). §4 is a
genuine hole rather than a wording problem — it needs an answer, and
that answer may require reading the SCC merge path again rather than
reasoning about it.
