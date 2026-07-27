# Handoff: compiled clause heads for Trealla

*Everything below was measured, not reasoned. Where I guessed and was
wrong, the wrong guess is recorded too — those are the expensive part.*

Companion patch: `head-plan-prototype.patch` (301 lines). It passes the
full suite — 317 passed / 1 failed, the single failure being this
sandbox's `NOSSL` crypto test, identical to baseline — and all 88
inputs of `eyereasoner/eyelet` byte-for-byte against baseline. It is
still a prototype, not a merge candidate: see §8.

**The suite passing has now twice failed to mean the patch was
correct.** Both times an external program found it (§7.1 was found by
the suite; §7.3 got through the suite entirely and was found by
eyelet). Treat §8.2 as a precondition, not a nice-to-have.

---

## 1. The one-line summary

Compiling `[H|T]` decomposition in head position is worth **9–12%** on
clause-selection-heavy code and nothing elsewhere. Everything else I
tried in this area is worth approximately nothing, and I tried a lot of
it.

Best of 3–5 runs, same compiler flags, output verified byte-identical
against baseline:

| benchmark | baseline | head plan | |
|---|---|---|---|
| `samples/chess.pl` | 5.15s | **4.70s** | **−8.7%** |
| `samples/queens11.pl` (`testq`) | 0.65s | **0.57s** | **−12%** |
| `samples/nsudoku.pl` | 0.46s | 0.46s | 0% |

Disabling list decomposition removes the entire gain. `HO_BIND` and
`HO_VAL` on their own measure −2%.

### The earlier numbers in this table were inflated by a bug

The first version of this document claimed 12%/14%/22%. Those were
measured on the 29-failure build, and the bug was that `HO_LIST`
**silently rejected clauses that should have matched** (§7). A build
that skips work it should be doing runs faster. nsudoku, the biggest
"win" at 22%, is now flat — it is clpz, so its time is in
`get_attr_/3`, `put_attr_/3` and `get_assoc/6`, not clause selection,
and it never had any business gaining 22% from a head-matching change.
I should have noticed that at the time.

**Never benchmark a build that fails its tests.** Obvious, and I did it
anyway.

---

## 2. Where the time actually goes

`statistics(profile,_)`, the mechanism documented in the README:

```
program           attempts       matched     hit%
chess.pl       128,093,722    34,480,798    26.9%
queens11         6,781,431     6,113,729    90.2%
nsudoku            779,850       492,379    63.1%
```

chess's hot predicates:

```
member_/3    43.5M -> 21.9M   50.4%
plus_one/2   40.9M ->  1.5M    3.8%   <- 14 ground facts, 31% of ALL attempts
can_step/5   12.3M ->  1.5M   12.5%
can_move/5    8.5M ->  2.2M   25.3%
```

queens11: `attack3/3` 4.6M at 92.8%, `selectq/3` 1.4M at 76.6%.
nsudoku is a different animal entirely — clpz, so the time is in
`get_attr_/3`, `put_attr_/3`, `get_assoc/6`, not clause selection.

gprof (`-O2 -pg -fno-ipa-icf`; see §5 on why that flag matters):

```
unify_interned   31.5%   128M calls   (unify_structs inlined into it)
_init            15.0%     -          unattributed, folded statics
match_head       13.2%    37.7M
unify_internal    9.2%   582M
set_var           4.7%   245M
unify             4.3%   129M
unify_var         3.2%   240M
                 -----
unify family     ~55%
```

---

## 3. The prototype

Three ops, one per head argument, decided once at load in
`process_clause()` and stored in `clause`:

- `HO_BIND` — fresh distinct variable: bind it.
- `HO_VAL` — repeat of an earlier head variable: unify against the
  binding the first occurrence made.
- `HO_LIST` — `[A|B]` where A and B are both fresh and distinct:
  decompose the caller's list directly.

`has_head_plan` is false for anything else, and the generic path runs.
At match time there are runtime bail-outs to `unify()` for attributed
variables and for `HO_LIST` against an unbound argument (which would
mean *building* a cell, i.e. put-mode, not covered).

Binding goes through `set_var()` rather than writing slots, so
trailing, `no_recov` and refcounting keep exactly the semantics
`unify()` would have produced. That is not optional — see §5.

**Coverage** (by attempts, chess/queens11): `mixed` 42.7% / 89.5%,
`ground-flat` 38.5% / 0%, `all-var` 18.8% / 10.5%.

Note that 18.8% is **wrong** as a guide to `HO_BIND` — see §4.4.

---

## 4. Negative results — please do not repeat these

### 4.1 Lowering the index threshold makes it 2× slower

`INDEX_THRESHOLD` in `assert_commit()` is 500, so nothing under 500
clauses gets an index at all — `plus_one/2` with its 14 facts is
scanned linearly, 40.9M times, to fail 96% of the time. Obvious fix,
so I tried it:

```
threshold 500:   5.14s
threshold   8:  11.56s
```

`pr->idx1` is a **skiplist**. Over 14 entries the lookup costs more
than the scan. If you want small-predicate indexing, it needs a
different structure (flat sorted array, or a small open-addressed
table on the first argument's functor), not a threshold change.

### 4.2 A first-argument filter in the match loop gains nothing

`has_next_key()` already does cheap `index_cmpkey` filtering on the
first three arguments, but only to decide whether a choice point is
needed. Adding the same filter to `match_head`'s loop, skipping
clauses before `try_me` + `unify`:

```
skipped 43M of 121M attempts     5.45s -> 5.40s
```

Nothing. `unify_structs` fails on `arity` then `val_off` — two
comparisons — so a failing attempt is already almost free. **This is
the single most important negative result here**: it means the 73% of
chess attempts that fail are not the problem, and anything aimed at
eliminating failed attempts (indexing, filtering, ground-flat head
compilation) is aimed at the wrong target.

### 4.3 `deref` branch layout is not the issue

98.5% of `deref` calls are on variables, so the `!is_var(c)` fast path
at the top is actually the *cold* path. Inverting it with
`__builtin_expect`: ~1%, inside noise. A 98.5/1.5 branch is predicted
correctly essentially always.

### 4.4 The all-var head fast path fires 12× less than predicted

I measured "all arguments are variables" at 18.8% of chess attempts
and built a fast path for it. It fired **1.92M times, 1.5%**, and
gained nothing.

The measurement classifier tested `is_var(arg)`. The implementation
additionally requires the variables to be **distinct**, because
`foo(X,X)` can fail — and `member_(_, El, El)`, the hottest clause in
the program, has `El` twice. I sized a day of work against a number
measured more loosely than the thing I then built. Check that your
classifier and your measurement agree before trusting a share.

### 4.5 Slot size is not the bottleneck

```
sizeof(cell)  24
sizeof(slot)  32   (cell + vgen + vgen2)
sizeof(frame) 72
```

739M variable derefs × 32 bytes ≈ 24 GB of slot traffic, which looks
damning. It isn't: padding the slot to 64 bytes costs only ~4%
(5.10s → 5.31s), so the accesses are mostly L1/L2-resident. Shrinking
32 → 24 would buy 1–2%, which does not justify a 69-site refactor on
its own.

---

## 5. Engine facts that cost me hours

**`unify()` does not deref its arguments.** Callers hand it derefed
cells. Passing a raw head cell to it overwrites the variable's slot
instead of following the ref that the first occurrence already put
there. Symptom: bindings silently vanish, `findall(X, member(X,[a,b,c]), L)`
returns `[_2,_3,_4]`.

**`unify()` resets `has_vars` and `no_recov` on entry.** Calling it in
the middle of a compiled plan discards what earlier `set_var` calls in
that same plan recorded. `commit_frame()` then reads stale values —
`has_vars` for determinism, `no_recov` for frame reuse — recovers a
frame it must not, and the bindings go with it. Same symptom as above,
different cause. Save and re-OR them around any `unify()` call inside
a plan.

**gprof folds identical static functions at `-O3`.** It attributed
29M calls to `set_loaded` (called twice in the whole program) and 34M
to `scan_is_chars_list_internal` (only reachable from `print.c`). Build
profiles with `-fno-ipa-icf -fno-inline-functions` or you will chase
ghosts. `perf` is better if your kernel allows it;
`perf_event_paranoid` blocked it for me.

**`classify_head()` runs on the parser's clause, not the database's.**
`process_clause()` is called on `p2->cl` by the parser and the assert
builtins; `assert_begin()` then `calloc`s a separate `rule` and copies
only `cells`, `num_vars`, `num_allocated_cells` and `cidx`. Any new
per-clause field you add in `clause` is therefore **silently zero on
every asserted clause** unless you also copy it there. That is a
convenient default for a fast path — it fails closed — but it is
invisible, so verify with a counter which clauses actually reach your
code rather than assuming from the source. See §8.1.

---

## 6. The one measured win outside head compilation

Stubbing out the vgen save/restore in `DEREF_VAR`: **5.15s → 4.73s,
about 8%**, on chess. It is compute *and* write traffic — a load and
two stores into the slot per variable argument per unification, which
dirties cache lines.

It also breaks rational trees: the suite hangs at
`tests/tests/test082.pl`, which is `L = [_,_,_|L], copy_term_nat(L,V)`
— exactly what the visit generations exist for.

So it is not a fix, it is the case for a compile-time flag
(`NORATIONAL_TREES=1` or similar) that drops `vgen`/`vgen2` from
`slot` and compiles the bookkeeping out. ~8% from the work plus ~1–2%
from the smaller slot, for a fraction of the effort of a head
instruction set. If you only do one thing from this document, do this.

---

## 7. The three bugs that were in `HO_LIST` — all now fixed

All three were in the same op, which is also where all the value is.

### 7.1 Treating a compact string as a mismatch (the 29-failure one)

This was the big one, and the diagnosis I wrote first was wrong.

```c
#define is_iso_list(c) (is_interned(c) && ((c)->arity == 2) && ((c)->val_off == g_dot_s))
#define is_string(c)   (is_cstring(c) && ((c)->flags & FLAG_CSTR_STRING))
```

`is_iso_list()` means **a real cons cell**: `TAG_INTERNED`, functor
`'.'`/2. A compact string is `TAG_CSTR`, so it fails that test — while
still being a perfectly good list. My code read:

```c
if (!is_iso_list(v) || is_string(v)) { plan = 0; break; }   // WRONG
```

`plan = 0` means *this clause does not match*. So every clause with a
`[H|T]` head argument called with a compact string was **silently
rejected**. `tests/tests/test062.pl` is a DCG under
`set_prolog_flag(double_quotes, codes)`, so every one of its
non-terminals took that path; the DCG fell through to clauses that
should never have been reached and eventually a list cell arrived in
goal position as `existence_error(procedure, '.'/2)`.

The `is_string(v)` guard was doing nothing, because `is_string` and
`is_iso_list` are mutually exclusive by construction. It read like a
correct exclusion and was dead code.

The fix: only claim a mismatch where **no list can match**, and hand
anything string-shaped to `unify()`, which knows how to walk a compact
string:

```c
if (is_var(v))     { plan = -1; break; }   // put-mode, not covered
if (is_cstring(v)) { plan = -1; break; }   // compact string: unify() handles it
if (!is_iso_list(v)) { plan = 0; break; }  // genuinely not a list
```

`is_cstring` rather than `is_string` — broader on purpose, it catches
every `TAG_CSTR` encoding rather than just the flagged-string one.

**The lesson: a fast path that can answer "no" is far more dangerous
than one that can only answer "yes".** `HO_BIND` and `HO_VAL` can only
ever succeed or bail; `HO_LIST` was the first op that could report
failure, and it reported it wrongly for a whole term representation I
had not considered. Any future op that can return `plan = 0` needs its
mismatch condition enumerated over *representations*, not over types.

Note also that this cost me a wrong diagnosis first: I blamed the
context passed with the sub-cells, fixed that, and the test still
failed. Deriving `vh_ctx`/`vt_ctx` from `deref()` is correct and worth
keeping, but it was not the bug.

### 7.2 Attributed variables in the sub-cells

The bail-out checked the top-level argument but not the list's head and
tail before binding them — `uninstantiation_error`, `get_atts/2`,
`tests/tests/test099.pl`. Fixed with a helper applied to all three:

```c
static bool hp_is_attvar(const query *q, const cell *v, pl_ctx v_ctx)
{
	if (!is_var(v)) return false;
	const slot *e = get_slot(q, GET_FRAME(v_ctx), v->var_num);
	return !is_ref(&e->c) && (e->c.val_attrs != NULL);
}
```

### 7.3 `has_vars` set on head-side binds — and my "fix" for it was the bug

I first read this line in `unify_internal`

```c
} else if (is_var(p1)) {
    if (depth > 1)
        q->has_vars = true;
    return unify_var(q, p1, p1_ctx, p2, p2_ctx, depth);
}
```

concluded that binding a head variable sets `has_vars`, and added
`q->has_vars = true;` before the binds in `HO_BIND` and `HO_LIST`. The
suite went green and I wrote it up as a fix.

It is the opposite of correct. Two lines above is:

```c
if (is_var(p2)) {
    return unify_var(q, p2, p2_ctx, p1, p1_ctx, depth);
}
```

and the call is `unify_internal(GOAL, HEAD)` — `p1` is the goal, `p2`
is the head. A head variable takes that **earlier** return and never
touches `has_vars`. The `depth > 1` line only fires when the *goal*
side has a variable that gets bound, which is the thing that actually
signals a possibly-non-determinate match. Binding a fresh clause
variable to whatever the caller passed says nothing about determinism —
every clause does that.

Setting it unconditionally made **every** plan-matched clause look
non-determinate. `commit_frame()` computes
`is_det = !q->has_vars && cl->is_unique`, so it kept choice points it
should have dropped. The suite did not care. A forward chainer did:
`eyereasoner/eyelet` re-derived facts from themselves
(`New state created: state1 from state1`), and three of its 88 inputs
stopped terminating.

The fix is to delete both assignments. The plan only ever binds
head-side cells, so it must never set `has_vars`; `HO_VAL` calls
`unify()`, which sets it correctly on its own.

**This is the second time in this file that a wrong mental model of
`unify_internal` produced a plausible comment, a green suite, and a
real bug.** The other is §7.1. Both would have been caught in minutes
by the differential harness in §8.2.

---

## 8. What still stands between this and a merge

The suite passes, but green tests are not the same as correct.

1. ~~**Reopen the `is_dynamic` guard.**~~ **Done, but not the way this
   said — the diagnosis was wrong.** The commented-out check in
   `process_predicate()`

   ```c
   if (/*pr->is_dynamic ||*/ pr->idx1)
       return;
   ```

   sits *after* the `process_clause()` loop and gates `check_unique()`
   and `compile_clause()` — **body** compilation, which predates this
   work entirely. It has nothing to do with head plans, and reopening
   it would have slowed dynamic predicates down while changing nothing
   about the hole I thought I was closing. I read a commented-out
   guard near code I was working on and assumed it was about my code.

   Head plans were in fact *already* excluded from dynamic predicates,
   by accident: `classify_head()` runs in `process_clause()` on the
   **parser's** clause, while `assert_begin()` `calloc`s a fresh `rule`
   and copies only `cells`, `num_vars`, `num_allocated_cells` and
   `cidx` — `has_head_plan` never reaches the asserted copy. Measured
   with a counter: eyelet's `quantum-darwinism.pl` executes 2575 plans,
   641 dynamic clauses reach `match_head`, and **zero** of them carry a
   plan.

   Correct by accident is not correct. The guard is now explicit, and
   hoisted out of the per-clause loop since all three conditions are
   invariant for the whole call:

   ```c
   const bool can_plan = q->pl->opt && !q->st.pr->is_dynamic
       && !q->flags.occurs_check;
   ```

   That is one test per predicate call rather than one per clause
   attempt, and it measures no slower than no guard at all. It also
   covers the case the accident does not: a predicate consulted as
   static, classified, and made dynamic afterwards — there the plans
   already exist and only a runtime check can suppress them.
2. **Build the differential harness anyway.** Run the plan, then the
   generic `unify`, assert identical success *and identical bindings*,
   behind a build flag. All three bugs in §7 would have surfaced at the
   first divergence instead of as 29 failures plus one wrong
   diagnosis. §7.1 in particular was a whole term representation I did
   not know the op could see — the harness finds that class of thing
   and reading the code does not.
3. **Audit the bail-outs against representations, not types.** §7.1
   says why. Enumerate every tag a head argument can carry (`TAG_VAR`,
   `TAG_INTERNED`, `TAG_CSTR` in its encodings, indirects, attributed
   vars, blobs) and state what the op does for each. Anything not
   enumerated should bail (`plan = -1`), never fail (`plan = 0`).
4. **Keep `HO_LIST`; `HO_BIND` and `HO_VAL` are scaffolding.**
   `HO_LIST` is the whole win. The other two measure −2% and exist
   because `HO_LIST` needs `HO_BIND`'s machinery for its sub-cells.
5. **Do not extend to `ground-flat` heads** without new evidence.
   38.5% of chess attempts, 96% of them failures, and §4.2 says
   failures are already nearly free.
6. **Run `eyereasoner/eyelet` as part of validation.** `./test-trealla`
   over its 88 inputs, diffed against a baseline build. It is a
   forward-chaining reasoner, so a spurious choice point or a wrongly
   succeeding match turns into extra derivations or non-termination
   rather than a silent wrong answer. It caught §7.3, which the whole
   Trealla suite missed. Any change to unification or determinism
   should go through it.
7. **Re-measure on all three samples, and only on a green build.**
   They disagree strongly: chess is selection-heavy, queens11 is
   execution-heavy, nsudoku is attributed-variable-heavy. Any change
   validated on one proves very little about the others — and §1 is
   the standing reminder of what a benchmark on a failing build is
   worth.

Weigh all of that against §6: the `NORATIONAL_TREES` flag is ~8% on
chess for a fraction of this effort, and it has no correctness
surface of its own beyond "the user asked for no rational trees".
