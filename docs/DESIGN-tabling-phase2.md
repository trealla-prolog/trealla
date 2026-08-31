# Tabling, Phase 2 — design (v3)

Design only for items 3–6. Items 1 and 2 have since been implemented;
their sections are kept as written for the record, with the outcome
noted below.

**v2 folded in two things:** the self-review in
`REVIEW-tabling-phase2.md`, and the effect of the Phase 1 review fixes
now landed on v3.2.0 (`tabling-review-fixes-all.patch` — handle
validation, iterative trie walks, best-effort indexing, failed-insert
unwind). Sections marked **[changed in v2]** differ materially from v1.

**v3 revises item 3 only**, from reading the code rather than
reasoning about it. Two of v2's framings there were wrong: attribution
is an SCC-level problem, not a continuation-level one, and invalidation
must be validate-on-read rather than invalidate-on-write given that
tables are per-thread. See **[changed in v3]** markers in that section.
The one assumption SCC attribution rests on has since been measured
(1157 checks, 0 mismatches) and is marked **[settled]** there. v3 also
records what items 1 and 2 turned up:

- **Item 1 (restraints) — done.** Three SWI-named flags, default
  `infinite`, `resource_error` on breach.
- **Item 2 (answer subsumption) — done.** `:- table p(_,_,min)`, keyed
  on the non-aggregated arguments, with the worklist re-pairing pass
  the section predicted; that pass is load-bearing and has a negative
  control.
- **A Phase 1 soundness bug, found while starting item 1 and fixed
  first.** `completion/0` marked an *escaping* nested SCC's tables
  COMPLETE before `'$tbl_pop_scc'` discovered the escape, caching a
  partial (often empty) table permanently. The existing cycle test
  could not catch it: its cycle has no answers, so a wrongly-completed
  empty table is indistinguishable from a correct one. This is the
  concrete instance of v2's own warning about unexercised tests.

Baseline: v3.2.0 plus that patch. Suite now 389 passed / 0 failed.

---

## Order **[changed in v2]**

| # | Item | Gives | Size | Risk | Status |
|---|------|-------|------|------|--------|
| 1 | Restraints | runaway answer *set* → `resource_error` | S | low | **done** |
| 2 | Answer subsumption | aggregate at insert; bounded tables | M | medium | **done** |
| 3 | Incremental tabling | tables survive assert/retract | L | medium | next |
| 4 | Shared completed tables | threads stop recomputing | M | **high** | |
| 5 | Trie-path reconstruction | drops per-answer images | M | medium | |
| 6 | `tnot` / well-founded semantics | correct negation through recursion | XL | high | |

v1 had sharing second, on the grounds that it was "the single biggest
win available". That was wrong on three counts: it has the most
dangerous failure mode (a cross-thread use-after-free, silent and
intermittent, and invisible to ASAN because the `pl_destroy()` sweep
hides it), it is worth nothing to single-threaded users or any WASM
embedding, and it was the one item whose central design question v1
failed to answer. It is now fourth and its risk is marked high.

Items 1 and 2 both touch `bif_tbl_add_answer_2`, and are now adjacent
so that work is done once.

General nondeterministic continuations were a separate item in v1.
They are not scheduled on their own merits — the limit they lift (a
worker suspending inside an if-then-else *condition*) is narrow and
documented. They are now the first step of item 6, which is the only
thing likely to require them.

---

## 1. Restraints

**What it gives.** A tabled predicate whose answer set is infinite
stores answers until the process is OOM-killed:

    :- table as//0.
    as --> [].
    as --> [a], as.

    ?- phrase(as, Ls).      % unbound: diverges, killed on memory

The value is diagnostic. A diverging table is currently
indistinguishable from a bug in the user's program: no message, no
partial output, and exit code 137 like any other OOM.

**[changed in v2] The justification is narrower than v1 claimed.** v1
also leaned on stack overflow from deep answers. That is fixed —
`trie_walk` iterates on the last argument and `trie_free` uses an
explicit worklist, so a 1,000,000-element answer now inserts and tears
down cleanly where 100,000 used to segfault. Restraints are still
needed for unbounded answer *sets*; they are no longer needed to
prevent that particular crash.

**Where it goes.** `bif_tbl_add_answer_2` → `trie_insert_` → `twalk`.
Three counters:

- *answer count per table* — `unsigned n_answers` on `struct table_`.
- *answer size* — `twalk` already carries `oom`, `attvar` and the
  unwind bookkeeping; add depth and node counters alongside.
- *subgoal size* — the same walk, from `'$tbl_variant_table'`.

**[changed in v2] The failure path is now free.** `trie_insert_`
unwinds whatever a failed walk created (Phase 1 review #4), so a
restraint breach can simply fail the walk the way `attvar` does and
cleanup happens automatically. In v1 this needed its own handling.

**Flags.** SWI's names, so documentation transfers:
`max_table_answer_size`, `max_table_subgoal_size`,
`max_answers_for_subgoal`. Default `infinite`; opt-in, nothing changes
for existing programs.

**Behaviour on breach.** `error` only. `suspend` needs answer
completion from item 6; `abstract` needs subgoal abstraction, which
changes what a variant *is*. An honest `resource_error` is the win.

**[changed in v2] Open question — restraints inside a merged SCC.** A
breach raises, and `'$tbl_reset_incomplete'` is wired to the exception
path. That covers the simple case. It does *not* obviously cover a
fresh variant completed in a nested SCC whose tables were merged into
the parent on pop. Which tables are incomplete then — the inner set,
or the merged set? This needs answering by reading the merge path, not
by reasoning about it. It is the one part of this item that is not
routine, and it is where Phase 1's bugs lived.

**Test.** `as//0` must raise instead of being killed — assert the
*exit code*, since the current failure is 137 and an output-only check
passes on a killed process. A bounded table must be unaffected;
`phrase(as,[a,a,a])` must still be true.

**Size.** Small, except for the SCC question above.

---

## 2. Answer subsumption

**What it gives.** Aggregation at insert instead of storing every
answer:

    :- table path(_,_,min).

Also *bounds* tables that would otherwise be unbounded, approaching the
same problem as item 1 from the other side.

**[changed in v2] It is not just insert logic.** The answer trie is
keyed on the **whole answer term** — that is what makes duplicate
detection free. Subsumption needs it keyed on the *non-aggregated*
arguments, so `path(a,b,3)` and `path(a,b,5)` collide and can be
combined. That is a structural change to how the trie is used, possibly
a second index or a per-predicate key construction. v1 framed this as a
change to what happens after a lookup, which would send an implementer
to the wrong place.

**The other hard part.** Today answers are append-only: a consumer that
has seen one is done. Subsumption lets an existing answer be
*updated*, so every consumer that read the old value must run again.
That changes the worklist protocol in `'$tbl_pop_worklist'` /
`'$tbl_wkl_work'`, whose `unproc_ans`/`unproc_susp` pairing assumes
append-only.

**[changed in v2]** The invariant this breaks is now documented in the
code, at the choice-point elision in `'$tbl_get_answer'` — that
elision is only sound because completed tables are immutable. The
implementer will meet the comment where they need it.

**Recommendation.** `min`/`max` over standard order first: monotone, so
updates move one way and the fixpoint argument stays simple. General
user-supplied lattices raise the question of whether the operation is
monotone at all, and a non-monotone one will not terminate — if wanted
later, it needs a documented obligation on the user.

**Size.** Medium.

---

## 3. Incremental tabling **[changed in v3 — substantially]**

**What it gives.** Tables survive changes to the dynamic predicates
they depend on, instead of the user calling `abolish_all_tables/0` and
paying for full recomputation.

**What it needs.** A dependency graph (which dynamic predicates a table
consulted), invalidation on assert/retract, lazy re-evaluation on next
call, and `:- table p/1 as incremental` / `:- dynamic q/1 as
incremental`.

**[v2 said] The hard part is attribution, not the graph.** The
difficulty is knowing *which table you are currently computing*, and it
is not "the top of a stack": a suspended consumer resumes later via
`delim/3`, running on behalf of a table that is not lexically current
at that moment. The dependency must be attributed to the table the
*continuation* belongs to.

**[changed in v3] That framing is right about the problem and wrong
about the granularity.** It leads an implementer to bracket `delim/3`
— the one place that sees both the first run of a worker and every
later resumption — with a push/pop "current table". **That bracket is
unsound.** `activate/3` drives `delim/3` with a failure-driven loop
(`delim(...), fail ; true`) and `reset/3` is nondeterministic; that is
precisely how multiple answers are collected. A push/pop pair pops on
the first solution and never re-pushes on redo, so every answer after
the first is attributed to whatever happens to be on the stack. The
symptom is a silently under-recorded dependency, i.e. a stale table —
the worst available failure mode for this feature, and invisible to any
test that only checks the first answer.

**[changed in v3] Attribute per SCC, not per table.** Push/pop against
the *existing* `'$tbl_push_scc'` / `'$tbl_pop_scc'` bracket. Everything
between those two is deterministic on exit — `activate/3`'s loop drains
and `completion/0` recurses to a fixpoint — which is why Phase 1 works
at all, and it makes the bracket safe for exactly the reason the
`delim/3` one is not. Three further points in its favour:

- an SCC is already the unit of *completion*, so it is the natural unit
  of *invalidation*;
- over-invalidating within an SCC costs nothing real: mutually
  recursive tables have to be recomputed together anyway;
- `top->fresh_head` already *is* the list of tables to flush the
  collected dependencies onto at `'$tbl_mark_all_complete'`.

This dissolves the v2 problem rather than solving it: at SCC
granularity you never need to know which continuation you are inside.

**[settled] Is the `TT` in `dep(Ball, C, Wrapper, TT)` always a table
of the SCC currently being completed?** Yes, as far as measurement
goes. A temporary `'$tbl_dep_scc_check'/1` comparing `t->scc` against
`tbl_scc_id(s)` where `completion/0` pulls each dep off the worklist:
**1157 checks, 0 mismatches** — 1126 across `tabling.pl`,
`tabling_subsumption.pl`, `tabling_restraints.pl` and
`dcg_tabling.pl`, plus 31 from cases written to attack the merge path
specifically (mutual recursion entered from the non-recursive side —
the shape that exposed the Phase 1 premature-completion bug; a
three-way cycle entered mid-chain; a cycle reached through `findall/3`,
forcing nested SCC completion instead of suspension; left recursion
over a cyclic graph entangled with a second table; and a re-run after
`abolish_all_tables/0`). A full `make test` sweep also reported none.

The check *counted* as well as compared, deliberately: the first run
printed nothing at all, which reads as a clean result but was actually
`tpl` honouring only the last `-g` and dropping the report goal. A
diagnostic that cannot tell "0 mismatches" from "never ran" is not a
diagnostic.

This is evidence, not proof — it does not cover every possible program
shape. If that margin matters, keep the comparison as a debug-build
assertion rather than deleting it with the scaffolding.

**[changed in v3] Table→table edges are also needed.** v2 lists only
"which dynamic predicates a table consulted". If table A calls table B
and B depends on `q`, then a change to `q` must invalidate A as well.
Record the edge at `'$tbl_variant_table'` when a table is looked up
while an SCC is active, and let a transitive walk handle the rest.

**[changed in v3] Invalidate on read, not on write.** This is the
constraint v2 missed entirely. `tbl_state` hangs off
`thread->tabling_state` — tables are **per-thread** — while the
database and `pl->dbgen` are shared. Eager invalidation from the
asserting thread therefore means writing to another thread's tables:
the same cross-thread hazard that makes item 4 "high risk", silently
inherited by an item nobody would think to look at for it. Invert it:

- `uint64_t last_modified` on `struct predicate_`, stamped
  `++pl->dbgen` on assert/retract (the counter already exists and is
  already bumped there);
- each table stores its dependencies plus the generation it completed
  at;
- `'$tbl_variant_table'` re-validates on lookup, **in the owning
  thread**. No cross-thread mutation anywhere.

A single `pl->dbgen` equality check short-circuits the whole walk when
nothing has changed since the table completed, which is the common
case.

**[changed in v3] Invalidation is abolish semantics, not
`'$tbl_reset_incomplete'`.** That builtin deliberately *keeps* answers
(`t->unproc_ans = t->first_ans`); invalidation needs a full drop. Now
that item 2 has landed this is sharper than it looks: `leaf->value` in
the answer trie points at live `tbl_ans` structs, so freeing answers
without also clearing the trie leaves dangling pointers straight in the
dedup path.

**Risk.** The graph is straightforward; the hook is in a hot path
everything uses. It belongs in `enter_predicate()` — once per *call*,
not once per clause tried, and it already touches `pr` and writes
`f->dbgen` — guarded by a bitfield test that is false for everything
unless opted in. Measure `make test` wall time with no incremental
predicates declared anyway; the null case must be free, and "should be"
is not a measurement.

**[changed in v3] Syntax.** `:- table p/1 as incremental` parses as
`table(as(p/1, incremental))`. Item 2's mode-spec clause matches any
compound that is not `/` or `//`, so it *swallowed* this: it tabled a
predicate literally named `as`/2 and left `p/1` untabled with no
diagnostic. Fixed ahead of this item (an `as`/2 clause ordered before
the mode-spec clause, which now rejects unimplemented options loudly)
— but it is worth noting as the shape of bug this directive syntax
invites.

The `:- dynamic q/1 as incremental` half is disproportionately
expensive: `dynamic` is handled at *parse* time in C, walking a list
that expects `/` or `//` shapes, and `as` lands in its
predicate-indicator error path. Prefer a separate `:- incremental q/1.`
directive handled entirely in `library(tabling)` by term expansion onto
a builtin. Same expressiveness, no parser surgery.

**[changed in v3] Do not store raw `predicate *`.** `destroy_predicate`
runs only at module teardown and abolish merely sets `is_abolished`, so
raw pointers *mostly* survive — but module unload is a live hazard, and
Phase 1's review already paid for this exact lesson once when raw table
pointers became `(serial, index)` slots. Key dependencies by (module,
functor, arity), or reuse the slot pattern.

**Size.** Large, but smaller than v2 implies: validate-on-read removes
the cross-thread machinery, and SCC attribution removes the
continuation-tracking machinery. The graph plumbing and the abolish
semantics are still real work.

---

## 4. Shared completed tables **[changed in v2 — substantially]**

**What it gives.** N threads tabling the same predicate currently do N
times the work.

**The invariant.** Phase 1 rejected locking shared tables and that
reasoning holds: the leader's critical section spans `completion/0`, a
*Prolog* loop running arbitrary user code between `'$tbl_*'` calls, and
no lock survives that. Per-builtin locks would make the tries
memory-safe while still letting thread B read thread A's half-built
table as complete — quietly wrong instead of loudly refused.

The way through is that completed tables are immutable: build
privately with no locking, publish on completion, read after lookup.

**[changed in v2] Say the locking precisely.** v1 said a reader
"checks the registry first and, on a hit, reads it without a lock".
Taken literally that is a data race — the registry is mutable shared
state, and reading a pointer out of it unsynchronised gives no
happens-before edge to the table contents. The correct statement:

- the table is fully built and never written again **before**
  publication;
- publication happens under a mutex;
- **lookup also happens under that mutex** — it is the acquire against
  the publisher's release that makes the contents visible;
- only after lookup returns does the reader touch the table, and by
  then it is immutable.

Not "read without a lock" — "one short lock to find it, then no lock to
use it". Publication and lookup are short and contain no user code,
which is what makes a mutex sound there and unsound around
`completion/0`.

**[changed in v2] Reclamation: decided, not enumerated.** v1 offered
"a refcount, or an epoch scheme". Refcounting fails for a specific
reason: a reader can hold a table across `completion/0`, which runs
arbitrary user code and may block on I/O, suspend or throw. A refcount
can therefore be held for unbounded time, and `abolish_all_tables/0`
cannot wait on it without risking deadlock against user code. **Use
deferred reclamation keyed on the existing `generation` counter.**

**[changed in v2] Half of that mechanism now exists.** The Phase 1
review fix replaced raw-pointer handles with `(serial, index)` pairs
into a slot array: releasing a slot bumps its serial, so a stale handle
validates as `type_error(table_handle, _)` instead of dereferencing
freed memory. That is exactly the stale-handle detection deferred
reclamation needs — a published table can be retired without waiting
for readers, because readers that come back late are detected.

**[changed in v2] But the slot array is per-thread, and that is now the
main obstacle.** It hangs off `tbl_state`, which hangs off `thread`.
A handle minted by thread A is meaningless in thread B by construction.
Sharing therefore needs a handle redesign, one of:

- a **shared slot table** for published tables, with thread-private
  slots for unpublished ones, and a bit in the handle saying which;
- a **two-level handle** — owner thread id plus slot — validated
  against the owner's array.

Decide this before writing any of the registry. It was invisible when
v1 was written because handles were raw pointers, which are
thread-agnostic and unsafe for a different reason.

**Other requirements.** Publish only tables that completed without an
exception, and — once item 1 lands — without hitting a restraint; a
truncated table is not a complete table. Opt-in
(`:- table p/1 as shared`), default private, matching today.

**Test.** `test_threads` currently asserts four threads agree *and*
that each did its own work via a marker count. With sharing on, that
count should show exactly one thread did the work. Keep both; they are
each other's negative control.

**[changed in v2] Threadless build.** `bif_tabling.c` already has
`#if USE_THREADS` around the per-thread state. This item is meaningless
under `NOTHREADS` and must compile out cleanly — and `NOTHREADS` is the
WASI configuration, which is what embedders ship. v1 never mentioned
it.

**Measure.** Wall time for N threads on the same predicate, shared vs
private, plus peak RSS — reported the way Phase 1 reported 200
sequential tabling threads at 9 MB vs 59 MB.

**Size.** Medium mechanism, high risk, and the handle question is a
prerequisite.

---

## 5. Trie-path answer reconstruction

**What it gives.** Every answer stores a full `cell *image` — a real
second copy, `copy_term_to_tmp` then `TPL_malloc` then `dup_cells` — in
addition to its path in the answer trie. Dropping the image means
`'$tbl_get_answer'` rebuilds each answer by walking the trie path.

**[changed in v2] The memory win is unconditional; only the time cost
is a trade.** v1 said "which way that goes depends entirely on the
ratio of answers stored to answers consumed", which is true of the time
and false of the memory — the trie exists either way. The question is
only whether reconstruction cost is acceptable, not whether the saving
is real.

**[changed in v2] There is no parent pointer.** `struct tnode_` has
`child`, `sibling`, `hnext`, `index`, `nchildren`, `value`, `is_leaf` —
nothing pointing up. Reconstruction walks leaf-to-root, so it needs
either a parent pointer on every node (8 bytes each, partly cancelling
the saving) or the path recorded another way. This was not in v1 and it
is the thing to settle first.

**[changed in v2] The case is stronger than it was.** Answers of
1,000,000 cells are now reachable, since the recursion limit is gone.
Each one carries a full image.

**Recommendation.** Still measure first, but measure the right thing:
reconstruction time per answer, and the parent-pointer overhead against
the image bytes saved.

**Size.** Medium.

---

## 6. `tnot` and well-founded semantics

**What it gives.** Correct semantics for negation through recursion.
Today tabling is least-model only: a program with a negative loop has
no answer where WFS gives `undefined`. This is what most of SWI's
tabling complexity exists to support, and it is the most valuable item
on the list.

**What it needs**, roughly in order:

- *General nondeterministic continuations* (v1's separate item 5).
  Phase 1 deliberately retreated from continuation capture; this
  reopens it, and is the likely first step.
- *Delay lists* — an answer conditional on a literal not yet known.
  `struct tbl_ans_` grows a delay list and the answer trie must
  distinguish conditional from unconditional answers.
- *Answer completion* — a second fixpoint over the delay structure,
  deciding which conditional answers become true, false or undefined.
- *Negation-aware scheduling* — the `tscc` machinery must understand
  negative dependencies, which are what make an SCC non-stratified.
- *`undefined`* as a third truth value, with decisions about printing
  and interaction with `call/1`.

**Risk.** High, and specifically: **this is the only item that changes
existing behaviour.** Everything else is additive. It rewrites
completion, the part of Phase 1 that took longest to get right.

**[changed in v2] It needs a flag.** v1 flagged the behaviour change
and then offered no way back. Phase 1 gated native tabling behind a
flag checked in `start_tabling`; WFS should be gated the same way,
because "correct" and "what your program did last release" may differ.

**Recommendation.** Last, own branch, existing tabling suite green
throughout. Do not start until 1–3 have shipped and settled.

**Size.** Extra large — probably larger than the other five combined.

---

## Explicitly not doing

- **GMP.** Declined in Phase 1, still declined.
- **Subgoal abstraction.** Changes what a variant is, and would have to
  be designed against the trie's canonical-cell representation. Revisit
  only if restraints prove insufficient in practice.
- **Call subsumption** (as opposed to answer subsumption). Variant
  tabling only, as documented — a different indexing strategy on the
  variant trie, large project, narrow gain.

---

## Cross-cutting

**`throw_error()` returns TRUE.** It raises by setting `q->did_throw`
and the builtin returns that value, so the natural helper idiom
`if (!check(q)) return false;` reads backwards: the check "succeeds",
control falls into the body, and the pending ball is lost. Symptom is a
silent *failure* where a throw was expected. Items 1 and 2 both add
error paths to `bif_tbl_add_answer_2`.

**ASAN cannot see peak memory.** The `pl_destroy()` sweep frees
everything at exit, so a lifetime bug shows up as RSS, not as a leak
report. Items 4 and 5 are both memory work — measure RSS, do not trust
a clean ASAN run.

**[changed in v2] Do not trust an unexercised test either.** While
fixing Phase 1 review #4 I wrote a memory test whose failing insert
rejected on the *first* argument, so nothing was created before the
failure and the A/B came back byte-identical. The fix was fine; the
test proved nothing. For each item below, check the test actually
reaches the code before believing its result.

**Leaks that are not ours.** The tabling suite reports ~53 KB under
ASAN from a string-slice-on-backtrack issue that reproduces on a
pristine checkout with no tabling module
(`LEAK-string-slice-on-backtrack.md`). Do not chase it here, and do not
accept "clean with tabling off" as evidence — the flag-off run of that
test is exponential and dies on the timeout before ASAN reports.

**Test-first is cheap here.** Every item has a failing test writable
before implementation: the `as//0` divergence, a min-aggregated path
table, a table surviving an `assertz`, the thread marker count.
Writing them first also settles the surface syntax while it is still
cheap to change.
