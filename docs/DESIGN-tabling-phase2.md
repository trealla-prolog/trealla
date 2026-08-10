# Tabling, Phase 2 — design

Design only. No Trealla source is modified by this document.

Phase 1 shipped: native tries, variant tabling, least-model semantics,
nested-SCC completion, thread-private tables, `abolish_table/1`. The
as-built architecture and its deliberate limits are in
`HANDOFF-native-tabling.md` §3–4; this picks up at §5.

Everything below is grounded in the shipped `bif_tabling.c` (~1343
lines, 16 `$tbl_*` builtins) and `library/tabling.pl`. Line references
are to the reference copy in this folder.

---

## Summary and recommended order

| # | Item | Gives | Size | Risk |
|---|------|-------|------|------|
| 1 | Restraints | runaway table → `resource_error`, not OOM-kill | S | low |
| 2 | Shared completed tables | threads stop recomputing | M | medium |
| 3 | Answer subsumption | aggregate at insert; bounded tables | M | medium |
| 4 | Incremental tabling | tables survive assert/retract | L | medium |
| 5 | General continuations | lifts the if-then-else-condition limit | M | high |
| 6 | Trie-path reconstruction | drops per-answer images (memory) | M | medium |
| 7 | `tnot` / well-founded semantics | correct negation through recursion | XL | high |

Do them in that order. The ordering is not by importance — WFS is the
most *valuable* item on the list — it is by how much each one
destabilises what already works. Restraints and sharing are additive.
WFS rewrites completion. Putting it last means the first six ship and
stay shipped.

Two of these (1, 3) touch the same twenty lines of
`bif_tbl_add_answer_2`. Doing 1 first makes 3 easier, not harder.

---

## 1. Restraints

**What it gives.** Today a tabled predicate whose answer set is
infinite does not hang — it stores answers until the OOM killer takes
the process (rc 137). The canonical case, verified to behave as Scryer
#573 describes:

    :- table as//0.
    as --> [].
    as --> [a], as.

    ?- phrase(as, Ls).      % unbound: diverges, killed on memory

The value is not the limit itself. It is that **a diverging table
currently looks exactly like a bug in the user's program**. There is
no message, no partial output, and the exit code is the same one you
get from any other OOM. Turning that into

    error(resource_error(tabled_answer_count), phrase/2)

converts an unbounded debugging session into a one-line diagnosis.

**Where it goes.** `bif_tbl_add_answer_2` (line 763) already walks the
whole answer term through `trie_insert_`, and already returns `false`
on a duplicate. Three counters, all cheap:

- *answer count per table* — a `unsigned n_answers` on `struct table_`,
  incremented next to the existing `first_ans/last_ans` append.
- *answer size* — `trie_insert_` visits every cell; thread a depth and
  a node count through `twalk` (it already carries state, line 217) and
  fail the insert past the limit.
- *subgoal size* — same walk, in `$tbl_variant_table` instead.

**Flags.** Follow SWI's names so existing code and documentation
transfer: `max_table_answer_size`, `max_table_subgoal_size`, and
`max_answers_for_subgoal`. Default `infinite` (current behaviour), so
this is opt-in and nothing changes for existing programs.

**Behaviour on breach.** SWI offers error / suspend / abstract. Ship
`error` only. `suspend` needs the answer-completion machinery from
item 7, and `abstract` needs subgoal abstraction, which changes what a
variant *is* — both are their own projects. An honest
`resource_error` is the whole win here; the rest is polish.

**Risk.** Low. Nothing in the completion algorithm changes. The one
subtlety: raising from inside `'$tbl_add_answer'` unwinds through the
`delim/3` in `library/tabling.pl`, so the table is left incomplete and
must be cleaned up. `'$tbl_reset_incomplete'` (line 1302) already
exists for exactly this and is already wired to the exception path —
confirm it covers the new throw rather than assuming it does.

**Test.** The `as//0` case must raise instead of being killed; a
bounded table must be unaffected; `phrase(as,[a,a,a])` must still be
`true`. Add to `tests/misc/tabling.pl`. Worth asserting the *exit
code* too — the current failure mode is 137, and a test that only
checks output would pass on a killed process.

**Size.** Small. A day, most of it testing.

---

## 2. Shared completed tables

**What it gives.** Tables are thread-private today, so N threads
tabling the same predicate do N times the work. For a server answering
similar queries on many threads this is the single biggest win
available.

**The invariant that must not break.** Phase 1 rejected locking shared
tables, and that reasoning still holds: the leader's critical section
spans `completion/0`, a *Prolog* loop that runs arbitrary user code
between `'$tbl_*'` calls. No lock survives that. Per-builtin locks
would make the tries memory-safe while still letting thread B read
thread A's half-built table as complete — quietly wrong instead of
loudly refused.

The way through is that **completed tables are immutable**. Nothing
writes to a table once `'$tbl_mark_all_complete'` has run. So:

- threads build tables privately, exactly as now, with no locking;
- on completion, the leader *publishes* the table to a registry hanging
  off `prolog` (not `thread`);
- another thread looking up a variant checks the registry first and, on
  a hit, reads it without a lock.

Only publication and lookup touch shared state, and both are short,
bounded operations with no user code inside them — so a mutex there is
sound in a way it is not around `completion/0`.

**What needs designing carefully.**

*Lifetime.* Once a table is shared, "free it when the thread retires"
is wrong. `tabling_destroy_thread()` currently frees everything the
thread built. Published tables need a refcount, or an epoch scheme, so
the last reader frees. This is the part most likely to produce a
use-after-free; budget for it.

*Abolish.* `abolish_all_tables/0` and `abolish_table/1` must
invalidate published tables while other threads may be enumerating
them. There is already a `generation` counter on `tbl_state` (int64_t,
matched to `q->st.v2`) for exactly this hazard within a thread —
extend that idea rather than inventing a second mechanism.

*What to publish.* Only tables that completed without an exception,
and (if item 1 lands first) only those that completed without hitting
a restraint. A table truncated by a restraint is not a complete table
and must never be shared.

*Opt-in.* SWI distinguishes shared from thread-local tables
(`:- table p/1 as shared`). Making sharing the default changes
observable behaviour for anyone relying on per-thread tables. Ship it
as opt-in, default private, matching today.

**Test.** Extend `test_threads` in `tests/misc/tabling.pl`. It
currently asserts four threads agree *and* that each did its own work —
a marker count proves the answers didn't just come from a shared table.
For this item that test inverts: with sharing on, the marker count
should show exactly *one* thread did the work. Keep both tests; they
are each other's negative control.

**Measure.** Wall time for N threads tabling the same predicate,
shared vs private, plus peak RSS. Phase 1's per-thread free took 200
sequential tabling threads from 59 MB to 9 MB; the sharing number
should be reported the same way.

**Size.** Medium. The mechanism is small; the lifetime rules are not.

---

## 3. Answer subsumption

**What it gives.** Aggregation at insert time instead of storing every
answer, e.g. shortest-path where only the minimum matters:

    :- table path(_,_,min).

Without it, a path table stores every route and the program filters
afterwards. With it, the table stores one answer per key. This also
*bounds* tables that would otherwise be unbounded, which makes it a
partial answer to the same problem restraints address — from the other
direction.

**Where it goes.** `bif_tbl_add_answer_2` again. Instead of "insert
into dedup trie, fail if it existed", the shape becomes: look up the
key part of the answer; if absent, insert; if present, apply the
lattice operation to the old and new answer, and if the result differs
from the old one, *replace* it and re-post the answer to consumers.

**The hard part is that last clause.** Today an answer is added once
and never changes, so a consumer that has seen it is done. With
subsumption an existing answer can be *updated*, and every consumer
that already consumed the old value must run again. That is a real
change to the worklist protocol in `'$tbl_wkl_work'` (line 1297) — the
existing `unproc_ans` / `unproc_susp` pairing assumes answers are
append-only.

**Recommendation.** Ship `min`/`max` over standard order first. They
are monotone, which means an update always moves in one direction and
the fixpoint argument stays simple. General user-supplied lattices
open the question of whether the operation is monotone at all — and a
non-monotone one will not terminate. If general lattices are wanted
later, they need a documented obligation on the user.

**Size.** Medium. The insert logic is small; the re-posting is not.

---

## 4. Incremental tabling

**What it gives.** Tables survive changes to the dynamic predicates
they depend on. Today any assert or retract means the answers are
stale and there is no mechanism to notice — the user must call
`abolish_all_tables/0` and pay for full recomputation. For a tabled
query over a slowly-changing fact base this is the difference between
usable and not.

**What it needs.**

*A dependency graph.* Record, per table, which dynamic predicates were
consulted while it was being computed. The natural capture point is
wherever a tabled evaluation calls a dynamic predicate — which means a
hook in the clause-retrieval path, not in `bif_tabling.c`.

*Invalidation.* `assertz`/`retract` on a predicate marked incremental
walks the IDG and marks dependent tables invalid. `bif_database.c` is
where this lands, and it is a hot path — the check must be near-free
when nothing is incremental (a flag on `struct predicate_` tested
before any graph walk).

*Re-evaluation.* Lazy is right: an invalid table is recomputed on next
call, not at invalidation time. Eager re-evaluation would make an
innocuous `assertz` arbitrarily expensive.

*Declaration.* `:- table p/1 as incremental` and
`:- dynamic q/1 as incremental`, per SWI.

**Risk.** The graph itself is straightforward. The risk is the hook in
the dynamic-predicate path: it touches code far outside tabling, on a
path everything uses. Measure `make test` wall time before and after
with no incremental predicates declared — the null case must be free.

**Size.** Large.

---

## 5. General nondeterministic continuations

**What it gives.** Lifts the documented Phase 1 limit that a worker
cannot suspend inside an if-then-else *condition* (the then/else
branches are fine). Also the groundwork for anything else that needs a
real captured continuation rather than a goal list.

**Why it is listed here.** The handoff notes this as "chunk-list
`call_continuation` for general nondeterministic continuations", and
records that Phase 1 *deliberately retreated* from continuation
capture. Reopening it reopens the hardest part of the original work.

**Recommendation.** Do not schedule this on its own merits — the
current limit is narrow and documented. Do it only if item 7 (WFS)
turns out to require it, which is likely, in which case it becomes the
first step of that project rather than a separate one.

---

## 6. Trie-path answer reconstruction

**What it gives.** Memory. Every answer currently stores a full
`cell *image` (`struct tbl_ans_`, line 440) *in addition to* its path
in the answer trie. The trie already contains the answer; the image is
a second copy kept because reconstructing a term from a trie path is
work and copying is not.

Dropping the image means `'$tbl_get_answer'` rebuilds each answer by
walking the trie path on demand.

**Trade.** Less memory per stored answer, more work per answer
*retrieved*. Which way that goes depends entirely on the ratio of
answers stored to answers consumed, and on how often a table is
re-consumed after completion. A table consumed once favours
reconstruction; a table consumed repeatedly favours images.

**Recommendation.** Measure before building. Instrument a build to
report total image bytes vs total answers for the existing benchmark
set, and get the retrieval count. If images are a small fraction of
tabling's footprint this item is not worth its risk — it touches the
hot retrieval path for a memory win that may not be there. This is the
one item on the list I would be prepared to drop after measuring.

---

## 7. `tnot` and well-founded semantics

**What it gives.** Correct semantics for negation through recursion.
Today tabling is least-model only: a program with a negative loop has
no answer under Phase 1, where WFS gives `undefined`. This is the item
that takes Trealla's tabling from "useful for transitive closure" to
"complete", and it is what most of SWI's tabling complexity exists to
support.

**What it needs.** Roughly, in order:

- *Delay lists* — an answer may be conditional on the truth of another
  literal not yet known. `struct tbl_ans_` grows a delay list, and the
  answer trie must distinguish conditional from unconditional answers.
- *Answer completion* — after an SCC fixpoint, decide which conditional
  answers become true, false, or stay undefined. This is a second
  fixpoint over the delay structure.
- *Negation-aware scheduling* — the SCC machinery (`tscc`, line 494)
  must understand negative dependencies, which are what make an SCC
  non-stratified.
- *`undefined`* as a third truth value, visible to the user, with
  decisions about how it prints and how it interacts with `call/1`.

**Risk.** High, and specifically: this is the one item that **changes
existing behaviour**. Everything else on this list is additive. WFS
rewrites completion, which is the part of Phase 1 that took the longest
to get right and has the most subtle tests behind it.

**Recommendation.** Last, on its own branch, with the existing tabling
suite green throughout. Do not start it until items 1–4 have shipped
and settled, because debugging a WFS regression on top of an unsettled
incremental-tabling change would be miserable.

**Size.** Extra large. This is the multi-week item; the other six
together are probably smaller.

---

## Explicitly not doing

- **GMP.** Declined in Phase 1, still declined.
- **Subgoal abstraction.** Changes what a variant is; would need to be
  designed against the trie's canonical-cell representation. Only
  worth revisiting if restraints prove insufficient in practice.
- **Call subsumption (as opposed to answer subsumption).** Variant
  tabling only, as documented. A different indexing strategy on the
  variant trie, and a large project for a narrow gain.

---

## Cross-cutting notes

**The `throw_error()` trap.** It returns TRUE — it raises by setting
`q->did_throw` and the builtin returns that value. The natural helper
idiom `if (!check(q)) return false;` reads backwards: the check
"succeeds", control falls into the body, and the pending ball is lost.
Symptom is a silent *failure* where a throw was expected. This cost an
hour in Phase 1 and items 1 and 3 both add new error paths to
`bif_tbl_add_answer_2`, so it will come up again.

**ASAN cannot see peak memory.** The `pl_destroy()` sweep frees
everything at exit, so a per-thread or per-table lifetime bug shows up
as RSS, not as a leak report. Items 2 and 6 are both memory work —
measure RSS, do not trust a clean ASAN run.

**Leaks that are not ours.** The tabling suite reports ~53 KB under
ASAN from a string-slice-on-backtrack issue that reproduces on a
pristine checkout with no tabling module at all
(`LEAK-string-slice-on-backtrack.md`). Do not chase it while working
on these items, and do not accept "clean with tabling off" as evidence
of anything — the flag-off run of that test is exponential and dies on
the timeout before ASAN reports.

**Test-first is cheap here.** Every item above has a failing test that
can be written before the implementation: the `as//0` divergence, the
thread marker count, a min-aggregated path table, a table that should
survive an `assertz`. Writing those first also settles the surface
syntax, which is the part hardest to change after release.
