# Handoff: Native Tabling for Trealla Prolog

*A letter to whichever model picks this up. Originally written before
Phase 1 existed; now updated after shipping it. Phase 1 is DONE and in
Andrew's tree. Read the war stories — both generations of them — they
are the map of where the landmines are buried.*

---

## 0. Where you are standing

Andrew (andrew.davison@gmail.com) works on **Trealla Prolog**
(github.com/trealla-prolog/trealla, C, WAM-ish VM). Across these
sessions we:

1. **Ported Scryer's `library(tabling)`** (Desouter et al.) to Trealla
   without modifying the tabling code. All enabling fixes upstream:
   goal_expansion routing, native bb attv preservation, attvar-aware
   var-var unification, multi-frame shift capture, copy_vars dead-frame
   safety.
2. **Built Phase 1 native tabling** (SWI's architecture: native tries/
   tables/worklists, delimited-continuation suspension, thin Prolog
   driver). Shipped as `native-tabling-full.patch` (~1200 lines,
   9 files, applies clean to upstream `a2c4c67`).

**Current state of the native engine:**
- `src/tabling.c` (~880 lines): canonical-cell trie with variant
  matching, hash-indexed children above fanout 16, tables with answer
  tries + insertion-order answer lists + suspension images, batched
  worklist scheduling, 12 `'$tbl_*'` builtins + 4 `'$trie_test_*'`
  test builtins.
- `library/tabling.pl` (embedded — REBUILD after edits): `(:- table)/1`
  via user:term_expansion, clause renaming to `'f tabled'`, Desouter
  driver shape, every shift in final-call position.
- Prolog flag `tabling` (default `true`): `false` = tabled predicates
  run as PLAIN calls (A/B switch; left recursion loops again).
- shift/1 hardening in `src/bif_control.c`: follows `'$jump'` cells,
  empty continuation is an executable `true` instruction.

**Benchmarks** (Andrew's Mac, vs SWI-Prolog's native tabling):
- fib(10000): 0.053s — identical to SWI.
- fib(100000): 0.672s vs SWI 0.594s elapsed — the residual is
  imath-vs-GMP on ~21K-digit additions, NOT tabling (small-answer
  tabled predicates scale exactly linearly: 10x work = 10x time).
  Andrew has declined GMP for now — don't reopen unprompted.

**Validated:** fib ladder to 100K tables, left-recursive path/2 on a
cyclic graph, mutual recursion (even/odd incl. correct failure),
tabled DCGs, non-ground answers with shared variables, regression suite
250/250 (incl. tests/misc/tabling.pl, 11 checks), ASAN silent, patch
re-verified from a fresh upstream clone.

**Scryer issues checked against us** (good conformance probes):
#1496 tabled DCG non-terminals (was broken here, fixed: Name//Arity in
the table directive), the setof-strictness one (fixed: delim catches
worker exceptions), #2621 redundant variant answers (we were already
correct - canonical var numbering dedups them), #1895 order-dependent
answers (fixed by nested-SCC completion; we reproduced Scryer's exact
bug before that change), #3365 lost variable sharing (see war story
2.10), #573 infinite answer sets (we diverge too - documented limit).

Build & test rhythm:

    make ISOCLINE=1 NOFFI=1 NOSSL=1 -j4          # sandbox build
    # ASAN: make clean && make ISOCLINE=1 NOFFI=1 NOSSL=1 \
    #   'OPT=-fsanitize=address -O0 -g -DDEBUG' -j4
    # NOTHREADS currently has an unrelated get_self link error - build WITH threads.
    printf "test, halt.\n" | ./tpl samples/fib_tabled.pl   # PASSED
    # suite: file-based diff, NOT $(...) capture (trailing-newline lies):
    for t in tests/tests/*.pl tests/issues/*.pl; do
      timeout 30 ./tpl -q -f -g halt "$t" > /tmp/t.out 2>/dev/null
      diff -a --strip-trailing-cr "${t%.*}.expected" /tmp/t.out >/dev/null || echo "FAIL $t"
    done

---

## 1. War stories, generation 1 (the library port — VM fixes, all upstream)

1.1 **goal_expansion is per-module**; `user:` hooks resolve via a
    redirect in parser.c using `find_goal_expansion_specific()` — which
    ignores the wildcard flag, because clpz registers a wildcard
    `user:goal_expansion` and redirecting on it hijacks clpz's own
    compilation (canary: tests/issues/test1061 → `[1-2,2-1]`).

1.2 **3-arg get_atts/put_atts are compile-time rewrites** (atts.pl,
    `user:goal_expansion`), never runtime predicates.

1.3 **`library/*.pl` is EMBEDDED in the binary.** Edits do nothing
    until `make`. If a library edit seems inert, it was.

1.4 **The blackboard copies** (`'$bb_put'` = detached image;
    `'$bb_b_put'` = by-ref clone, FLAG_LIVE, backtrack-undone).
    Attv preservation is native: `'$bb'('$bb_attv'(T,[V-RawAttrs|...]))`
    with a transitive closure THROUGH attribute values (attvar graphs
    cycle through attributes; pair-lists over shared vars linearize).

1.5 **`rebase_term` strips FLAG_VAR_REF** on import. Non-ref var cells
    resolve against whatever context the reader passes. Anything
    attached to a slot for later cross-frame reading must be re-flagged
    as refs with explicit `val_ctx`. Symptom: get_atts returns some
    OTHER variable's attributes.

1.6 **Var-var unification is attvar-aware** (unify.c): the plain var
    binds to the attributed one regardless of frame age, no hook fires
    (SICStus semantics), `no_recov` pins younger attvar frames.

1.7 **copy_vars must not deref dead frames**: `get_ordered_slot_num` is
    pure arithmetic (safe); the slot deref happens only when
    `copy_attrs`. ASAN heap-use-after-free under `import_term` = you
    reintroduced this.

1.8 **A null cell prints as `dummy`** — first atom in the atom table.
    `cont(dummy)` means null continuation, not a term named dummy.

1.9 Frames/slots REALLOC on growth (`create_vars`!) — never cache
    `frame*`/`slot*` across anything that can grow them.

---

## 2. War stories, generation 2 (building the native engine)

2.1 **The trie MUST hash-index children.** First-child/next-sibling
    with linear scan is quadratic for flat key spaces: tabled fib(N,_)
    puts every distinct N at ONE level; each new variant insert scans
    all existing siblings. Cost us 16s at fib(100000); the hash index
    (threshold 16, FNV keys consistent with key_eq including the
    atom-vs-cstring same-text case, grow at 3/4 load) made it 0.053s
    at fib(10000) — SWI parity. If you add a new key type, update
    key_eq AND key_hash together or hash lookups silently miss.

2.2 **shift's continuation capture must FOLLOW `'$jump'` cells.**
    Compiled if-then-else places jump-over-the-else after a then-branch
    call; a suspension inside a then-branch otherwise captures the
    else-branch as phantom goals (symptom: continuations containing
    `'$jump'(6), run_leader(...), true, ...` and calls to integers).
    The jump operand is the cell distance from the jump cell itself.
    Related discipline in the driver: every shift sits in final-call
    position of its own clause. Still unsupported: suspending inside an
    if-then-else CONDITION (the capture stops at the barrier-like
    `'$drop_barrier'` inside the condition's compiled form).

2.3 **Fabricated cells need their builtins resolved.**
    `make_atom(c, g_true_s)` produces an atom that resolves as a
    MISSING USER PREDICATE when executed (existence_error(true/0));
    use `make_instr(c, g_true_s, bif_iso_true_0, 0, 0)`.

2.4 **term_expansion result lists re-expand each element.** The
    `:- table` expansion emits `[Wrapper-rule, '$tabled'(Head)]` — the
    marker fact MUST come last or the rename rule rewrites the wrapper
    rule's own head as it is being loaded. (Scryer's wrapper.pl has the
    same ordering for the same unstated reason.)

2.5 **`clause/2` throws permission_error on static predicates** —
    "no_worker" diagnostics via clause/2 LIE. Predicates you thought
    were missing may exist. Probe by calling, or predicate_property.

2.6 **Embedded `g_libs` SHADOWS `--library` filesystem paths**
    (do_use_module: loaded-module → ignore-list → g_libs → filesystem).
    Embedding "tabling" means `--library scryerpath` can no longer
    provide `library(tabling)`. The ignore-list also silently no-ops
    `library(cont)` et al.

2.7 **`-g` goals run BEFORE file args load**; pipe queries via stdin
    and end with an inline `halt.` or the toplevel waits on
    choicepoints and your timeout lies. Also: `table foo/1` in a -g
    string won't parse once `table` is a prefix op — parenthesize or
    put probes in files.

2.8 **Two-pass materialized scheduling works.** `'$tbl_pop_worklist'`
    materializes (new answers x all susps) + (old answers x new susps)
    into a pending pair list and resets cursors; work arriving during a
    drain re-enqueues the table for the next round. Nondet enumeration
    via the between/3 pattern: stash cursor in `q->st.v1` BEFORE
    `push_choice`.

2.9 **Answers/dependencies are detached images** (`copy_term_to_tmp` +
    `dup_cells` malloc; free via `unshare_cells` then TPL_free — images
    hold refcounted bigints). dep/4 packs SourceCall+Cont+Wrapper in
    ONE image so they keep sharing variables; the answer image is
    SEPARATE on purpose — unification of `call_info(Answer,_)` against
    the dep copy is what instantiates the consumer.

---

2.10 **Answer variables live in the frame that ran the import, and that
     frame can be recycled.** This one produced *silently wrong answers*
     for months and was only caught via Scryer issue #3365. `import_term`
     creates the imported answer's variables in the CURRENT frame (the
     driver's, not the caller's). Once unified, any structure the caller
     retains - eg. an answer argument that is a list containing a shared
     variable - points at slots of that frame. On deterministic exit the
     frame is trimmed, the slots are recycled, and two occurrences of one
     answer variable quietly stop being the same variable: binding one no
     longer binds the other.

     Minimal repro (was: Q unbound; correct: Q == c):

         :- table s/2.
         s([a|X], X).
         ?- s([P,Q], R), R = [c].

     Depth matters. `s2([a|X],X)` called as `s2([P|T],R)` is FINE -
     unify merges the answer var straight into caller variables and
     nothing points into the doomed frame. It only breaks when the
     shared variable sits *inside a structure* in the answer.

     Diagnostic technique that cracked it: dump the var cells (var_num
     and val_ctx) at STORE, at IMG, after IMPORT, and after UNIFY. All
     four were correct - sharing survived every step - which ruled out
     the copier and the trie and localized the loss to *after* the
     builtin returned. Do this before theorising; two plausible
     hypotheses (copy_term_to_tmp reusing a live q->vars map, and
     get_ordered_slot_num's ctx*100+var_num colliding) were both wrong.

     Current fix: `tbl_pin_answer_frame()` sets `no_recov` on the frame
     when the imported answer contains variables (ground answers - the
     common case - skip it, so fib is unaffected). This mirrors what the
     VM already does in set_var(), which raises q->no_recov when a caller
     var is bound to a non-ground compound in a younger frame; that flag
     is only transferred to the NEXT frame created, so a binding made
     inside a builtin never protects the frame holding the variables.

     **Known fragility - the fix is not airtight.** `reuse_frame()`
     (last-call optimization, query.c) does:

         f_cur->initial_slots = f_cur->actual_slots = num_vars;
         f_cur->no_recov = false;          // clears the pin

     so a pinned frame can still be reused and SHRUNK. Since import_term
     grew that frame via create_vars, answer variables can end up beyond
     actual_slots, where get_slot() derives an address from a stale
     f->op - a wild pointer. Nothing reproduced this (ASAN+UBSAN,
     optimizer on and off, repeats, macOS and Linux), but the hazard is
     real and build-sensitive. If a crash ever appears near answer
     return, suspect this first.

     Sturdier fix, if needed: after unifying, walk the imported answer
     and rewrite each variable cell to point DIRECTLY at whatever caller
     variable it dereferenced to (FLAG_VAR_REF + that var's val_ctx), so
     nothing the caller retains indirects through a driver-frame slot.
     Fall back to the pin only for answer variables with no caller-side
     counterpart (eg. answer f(g(V)) unified with caller f(X): V is
     genuinely fresh and must live somewhere).

     Regression cover: tests/misc/tabling.pl checks 'answer sharing' and
     'generate mode'. Both were verified to FAIL without the fix - every
     other test in the file returns GROUND answers, which is exactly why
     this survived so long. Any new tabling test should include a
     non-ground answer with shared variables.


## 3. As-built architecture (Phase 1)

    :- table f/N        user:term_expansion (library/tabling.pl)
                        → (f(..) :- tabling:start_tabling(f(..), 'f tabled'(..)))
                        → clauses of f renamed to 'f tabled'
    start_tabling       flag check → native_start_tabling
                        → '$tbl_variant_table' (trie: canonical cells,
                          $VAR-numbered vars = variant matching free)
                        → complete: '$tbl_get_answer' (nondet, fresh copies)
                        → active:   shift(call_info(W,T))
                        → fresh:    leader? follower-activate-shift
                                    : run_leader(activate → completion)
    delim               reset(Worker) → none: '$tbl_add_answer' (FAILS on dup)
                                      → cont(C): '$tbl_add_suspension'(SrcT, dep/4)
    completion          '$tbl_pop_worklist' → '$tbl_wkl_work' pairs →
                        delim(cont) → fixpoint → '$tbl_mark_all_complete'

Tables/tries are process-global statics in tabling.c (multi-prolog
embedding shares them — known Phase-1 shortcut; move to `prolog*` if it
matters). Handles cross the Prolog boundary as raw-pointer integers —
'$tbl_*'-internal only.

## 4. Phase 1 limits (documented, deliberate)

Variant tabling, least-model semantics. No tnot/WFS, no incremental,
no answer subsumption, no shared/thread-local table split. Attvars in
tabled calls → type_error(free_variable). cstr strings vs char lists
are distinct trie paths. Suspension inside ITE *conditions*
unsupported. `test104.expected` is var-numbering sensitive; regenerate
+ eyeball if image-copying changes intermediate var counts.

**Infinite answer sets diverge** (Scryer #573; verified, we behave the
same). Tables complete before answering, so a tabled call with
unboundedly many answers never returns — and since answers are stored,
the process is OOM-killed (rc 137) rather than spinning. Canonical
case: a tabled DCG called with an unbound list, `as --> []. as -->
[a], as.` with `phrase(as, Ls)`. Bound input works
(`phrase(as,[a,a,a])` → true). Fixing it properly = restraints
(cheapest hook: '$tbl_add_answer', which already walks the answer, so
depth/count limits are nearly free → resource_error) or batched
scheduling (post answers pre-completion — reopens the continuation
capture we deliberately retreated from). Same caveat for an unbounded
chain of distinct variants: that grows SCC nesting instead.

**Tabling is single-threaded, and says so.** Every structure is a
process-global static with no locking (contrast `bif_bboard.c`, which
takes `prolog_lock` six times). Before the guard, a tabled call from a
second thread *hung* — the newcomer waits in completion on a worklist
it cannot see. Now `tbl_claim()` in `'$tbl_variant_table'` claims
ownership for the first thread to table anything, under the prolog
guard so a first-use race still has one winner, and refuses everyone
else with `resource_error(tabling_not_thread_safe)`. Ownership is
released by `abolish_all_tables/0` — deliberately *not* owner-checked,
because if the owning thread has exited, requiring ownership would lock
tabling out of the process permanently (`g_in_use` still refuses an
abolish racing a live leader). In practice sequential handover often
works without abolishing too, since the OS recycles the retired
thread's `pthread_t`; don't rely on that, it isn't guaranteed.
Covered by `test_threads` in `tests/misc/tabling.pl`, negative-control
checked (guard stubbed out → `threads: FAILED no_error`).

Trap worth knowing, since it cost an hour: **`throw_error()` returns
TRUE.** It raises by setting `q->did_throw` and the builtin returns
that value. The natural-looking `if (!check(q)) return false;` helper
idiom therefore reads backwards — the check "succeeds", control falls
through into the body, and the pending ball is lost (symptom: the call
silently *fails* in the child thread instead of throwing). Keep the
predicate pure and let the caller do `return throw_error(...)`.

### Leaks: none are ours

Under ASAN the expanded suite reports 53,102 bytes in 2,590
allocations. None of it is ours, and no frame points into
`tabling.c` — see `LEAK-string-slice-on-backtrack.md` for the full
write-up. Short version: backtracking over a goal that took a *slice*
of a compact string blob (`append/3` walking a `TAG_CSTR`) never
unshares the `strbuf`, one per iteration. It reproduces on a
**pristine checkout with no patch and no tabling module**, which is
the only form of proof worth trusting here. The AoC test inherits it
because it splits stone digits with `append/3`.

Do not accept either of the shortcuts I tried first. "Identical with
the flag on and off" proves nothing when `:- initialization(main)` has
already run `main` before `set_prolog_flag/2` fires; and "leaks with
tabling on, clean with it off" proves nothing when the flag-off run is
exponential and dies on the timeout before ASAN reports. Build
pristine and reproduce there.

The only tabling allocation with process lifetime was the `g_scc`
high-water array (still-reachable, so never ASAN-reported);
`abolish_all_tables/0` now frees it.

## 5. Phase 2, when Andrew asks

In rough order of value: **restraints** (answer/subgoal size limits so
a runaway table raises resource_error instead of being OOM-killed —
small, and currently a hang is indistinguishable from a bug),
`tnot`/well-founded semantics (delay lists, answer completion — most of
SWI's tabling complexity lives here),
incremental tabling (IDG + invalidation on assert/retract), answer
subsumption, thread-local tables, chunk-list `call_continuation` for
general nondeterministic continuations, trie-path answer reconstruction
(drop the per-answer images). NOT on the list: GMP (declined for now).

## 6. Files of record

Everything is in `native-tabling-full.patch` (one `git apply`, verified
against a fresh clone of `a2c4c67`) plus `README-native-tabling.md`
(apply/build/use instructions, benchmark numbers). Reference copies of
`src/tabling.c` and `library/tabling.pl` sit alongside. Andrew applies
on branch `native-tabling` on his Mac (full deps: editline/FFI/OpenSSL/
threads — plain `make`).

Good luck. Measure before optimizing, ASAN before celebrating, follow
the '$jump's, and if a continuation ever prints as `dummy` — it's null,
and now you know why.

## 7. Removed before merge: the trie test harness

`'$trie_test_clear/0'`, `'$trie_test_insert/2'`, `'$trie_test_lookup/1'`
and `'$trie_test_count/1'` were scaffolding from the trie work. Nothing
in `tests/`, `library/` or `samples/` ever called them, so they shipped
as four builtins with no coverage — an untested API is a liability, not
an asset. Removed, along with the three helpers that existed only to
serve them (`trie_insert/5`, `trie_lookup/4`, `trie_count_leaves/1`);
the real code goes through `trie_insert_/6` directly. ~100 lines,
tabling.c 1432 → 1343.

They never reached git history, so if Phase 2 wants to poke the trie
directly again, here they are verbatim:

```c
// ---------------------------------------------------------------------
// Test builtins. A single process-global test trie; the real variant
// trie will hang off prolog* alongside the table registry.

static bool bif_sys_trie_test_clear_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	(void)q;
	trie_free(s->test_trie);
	s->test_trie = NULL;
	return true;
}

static bool bif_sys_trie_test_insert_2(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	bool existed = false;

	if (!trie_insert(q, &s->test_trie, p1, p1_ctx, &existed))
		return false;

	cell tmp;
	make_atom(&tmp, existed ? g_true_s : g_false_s);
	return unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_sys_trie_test_lookup_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);
	return trie_lookup(q, &s->test_trie, p1, p1_ctx) != NULL;
}

static bool bif_sys_trie_test_count_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);
	cell tmp;
	make_int(&tmp, (pl_int)trie_count_leaves(s->test_trie));
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

builtins g_tabling_bifs[] =

static tnode *trie_insert(query *q, tnode **root, cell *c, pl_ctx ctx, bool *existed)
{
	return trie_insert_(q, root, c, ctx, existed, NULL);
}


// Full-term lookup: NULL when no such canonical path/leaf.

static tnode *trie_lookup(query *q, tnode **root, cell *c, pl_ctx ctx)
{
	twalk w;
	twalk_init(&w, q, root, false);
	bool ok = trie_walk(&w, c, ctx);
	tnode *leaf = w.node;
	twalk_done(&w);
	return (ok && leaf && leaf->is_leaf) ? leaf : NULL;
}


static unsigned trie_count_leaves(const tnode *n)
{
	unsigned cnt = 0;

	for (; n; n = n->sibling) {
		if (n->is_leaf)
			cnt++;

		cnt += trie_count_leaves(n->child);
	}

	return cnt;
}

```

Re-register them in `g_tabling_bifs[]` as:

```c
	{"$trie_test_clear", 0, bif_sys_trie_test_clear_0, "", false, false, BLAH},
	{"$trie_test_insert", 2, bif_sys_trie_test_insert_2, "+term,-atom", false, false, BLAH},
	{"$trie_test_lookup", 1, bif_sys_trie_test_lookup_1, "+term", false, false, BLAH},
	{"$trie_test_count", 1, bif_sys_trie_test_count_1, "-integer", false, false, BLAH},
```

and add `tnode *test_trie;` back to `tbl_state` with a `trie_free()` in
`tabling_destroy()`. If they come back, they come back **with tests**.
