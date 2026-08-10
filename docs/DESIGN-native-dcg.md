# Native C DCGs for Trealla Prolog

Design for replacing the Prolog implementation in `library/dcgs.pl` with a
cell-level translator in C, preserving the module's exported interface exactly.

**Source basis.** Read from `trealla-prolog/trealla@main`: `library/dcgs.pl`,
`library/builtins.pl`, `src/parser.c` (the `dcg_expansion`, `term_expansion`,
`goal_expansion` region), `src/heap.c`, `src/heap.h`, `src/query.h`,
`src/builtins.h`, `src/internal.h`. Not read: `src/prolog.c`, `src/module.c`,
`src/parser.c` outside the expansion region. Assumptions that depend on those
files are marked **[verify]**.

---

## 1. What the current implementation does, and what it costs

DCG translation today happens entirely at consult time, in Prolog, driven from
C by a string round-trip.

`src/parser.c`:

```c
static bool term_expansion(parser *p)
{
    ...
    if ((c->val_off == g_dcg_s) && (c->arity == 2)) {
        dcg_expansion(p);   // FIXME: need to term_expand & may be a list?
    }
    ...
}

static bool dcg_expansion(parser *p)
{
    query *q = query_create(p->m);              // 1. whole new query
    ... make_instr(tmp, g_dcg_translate_s, ...);
    bool ok = execute(q, tmp, p->cl->num_vars+MAX_ARITY);   // 2. run Prolog
    char *src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);  // 3. print
    strcat(src, ".");
    parser *p2 = parser_create(p->m);
    p2->srcptr = src;
    tokenize(p2, false, false);                 // 4. re-parse
    process_clause(p2->m, p2->cl, NULL);
    p->cl = p2->cl;                             // 5. swap clause in
    ...
}
```

`dcg_translate/2` lives in `library/builtins.pl` and calls `dcg_rule/2` in
`library/dcgs.pl`, which is a nondeterministic meta-interpreter built on
`=..`, `append/3`, `subsumes_term/2` and `must_be/2`.

Per DCG clause that is: a query create/destroy (frames, slots, choices, trails
arrays), a full resolution over a dozen-odd clauses with `=..`/`append/3` in the
inner loop, a canonical print into a heap-allocated string, a fresh parser, a
full tokenize + operator-resolution + `assign_vars` pass, and a clause copy.

Beyond speed, the text round-trip is semantically fragile:

| Hazard | Why it bites |
|---|---|
| Operator table | The body is printed with current ops and re-read with current ops. A user `:- op(...)` between print and read is impossible, but a *body containing* a term whose canonical form depends on op priorities must round-trip exactly. |
| `double_quotes` flag | String literals in terminals print according to the flag and re-read according to the flag; the two must agree. |
| Non-textual cells | Bigints, rationals, blobs, attributed vars, and stream handles inside `{Goal}` arguments must survive print → read. Blobs cannot. |
| Variable identity | Preserved only via generated `_G<n>` names. |
| Var names | The original clause's `vartab` is discarded; `listing/1` and error messages lose user variable names. |
| Error reporting | The only channel out is `p->error = true`; DCG errors do not surface as ISO exceptions with a culprit. |
| The FIXME | `-->` terms bypass user `term_expansion/2`, and a `term_expansion/2` that returns a *list* of clauses is not handled. |

The native design's headline change: **translate cells to cells in place — no
query, no print, no re-parse.**

---

## 2. Scope

Per the decisions taken: everything except `phrase/2..5`, which stays in Prolog
as a thin wrapper because the wrapper is mostly module-stripping and
`call/1` dispatch, which C would not improve.

| Component | Where it lives after |
|---|---|
| `-->`/2 clause translation (`dcg_rule/2`) | C |
| DCG body translation (`dcg_body/4`, `dcg_cbody/4`, `dcg_constr/1`) | C |
| Terminal-list threading (`dcg_terminals/3`) | C |
| Consult-time hook (`term_expansion/2` for `-->`) | C, in `parser.c` |
| Consult-time `goal_expansion/2` for `phrase/2,3` | C, in `parser.c` |
| `phrase/2..5` | Prolog, over native `'$dcg_body'/4` |
| `seq//1`, `seqq//1`, `...//0` | Prolog (optional C fast paths, §9) |
| `(-->)/2` existence-error stub, ops, meta_predicate decls | Prolog |

The module's export list is unchanged:

```prolog
:- module(dcgs, [op(1105, xfy, '|'), phrase/2, phrase/3, phrase//2,
                 phrase//3, seq//1, seqq//1, ... //0, (-->)/2]).
```

---

## 3. Architecture

Two layers, in new files `src/dcgs.c` / `src/dcgs.h`.

### Layer 1 — the translator (no `query` dependency)

Pure cell → cell. It must run in two very different environments:

* **consult time**, against a `parser*`: fresh variables come from bumping
  `p->cl->num_vars`, output goes into a newly allocated `clause`;
* **runtime**, against a `query*`: fresh variables come from `create_vars(q, n)`,
  output goes onto the query heap via `alloc_heap` + `dup_cells`.

Rather than write it twice, parameterise it:

```c
typedef struct dcg_arena_ {
    cell *buf;            // growable scratch, reused across calls
    unsigned len, cap;
} dcg_arena;

typedef struct dcg_ctx_ {
    dcg_arena *ar;
    prolog *pl;
    module *m;
    // fresh-variable supply, supplied by the caller
    bool (*newvar)(struct dcg_ctx_ *ctx, cell *out);
    void *owner;          // parser* or query*
    bool defer_errors;    // consult time: true; runtime: true as well (see §7)
    unsigned depth;
    bool oom, error;
    cell *culprit;        // for representation_error
    const char *err_type, *err_expected;
} dcg_ctx;
```

Everything is emitted into `ar` (a flat cell array), then blitted into final
storage by the caller. This is the same idiom `heap.c` uses on the tmp heap:
record the start index, append children, then patch the parent's `num_cells`.

```c
static unsigned emit_open(dcg_ctx *c, pl_idx functor, unsigned arity);
static void     emit_close(dcg_ctx *c, unsigned at);   // patch num_cells
static bool     emit_term(dcg_ctx *c, const cell *t);  // memcpy a subterm
```

`emit_close(at)` sets `buf[at].num_cells = len - at`, exactly as
`clone_term_to_tmp_internal` does with `save_idx`.

Public surface of Layer 1:

```c
// Translate a DCG body. S0/S are single-cell values (usually vars) to thread.
// Returns index into ctx->ar of the emitted goal, or -1.
int  dcg_translate_body(dcg_ctx *ctx, const cell *body, const cell *s0, const cell *s);

// Translate a whole (H --> B) term into (Head :- Body).
int  dcg_translate_rule(dcg_ctx *ctx, const cell *rule_term);

// Is this term a DCG construct (7.14.1-7.14.12)? No allocation, no throwing.
bool dcg_is_constr(const prolog *pl, const cell *c);
```

Layer 1 never throws and never touches a query; it records error intent in
`ctx` and lets the caller raise it in the appropriate way (parser error at
consult time, `throw_error()` at runtime).

### Layer 2 — the hooks

1. `parser.c: term_expansion()` calls `dcg_translate_rule` directly and swaps in
   the new clause. `dcg_expansion()` and its query/print/reparse are deleted.
2. `parser.c: goal_expansion()` gains an early native case for `phrase/2` and
   `phrase/3` with a nonvar first argument.
3. A native builtin `'$dcg_body'/4` for `phrase/3` and friends to call at
   runtime.
4. A native builtin `'$dcg_rule'/2` so `expand_term/2` and `dcg_translate/2` in
   `library/builtins.pl` keep working; they become one-line wrappers.

---

## 4. The translation rules

`translate_body(Body, S0, S)` where `S0` and `S` are *cell values* the
translator threads through. Cases follow ISO 7.14 and mirror `dcg_constr/1` and
`dcg_cbody/4` in `library/dcgs.pl` one-for-one — the goal is bit-identical
output, so that the existing test suite and `listing/1` are unaffected.

| # | Body | Emitted goal | Native note |
|---|---|---|---|
| 1 | `Var` | `phrase(Var, S0, S)` | Deferred; never throws at translate time |
| 2 | `[]` (7.14.1) | `S0 = S` | |
| 3 | `[T\|Ts]` (7.14.2) | `S0 = [T,...\|S]` | List cells built directly with `S` as the tail — no `append/3` |
| 3b | string cell | see §6 | Trealla's compact string representation needs care |
| 4 | `(A, B)` (7.14.3) | `(A', B')` | one fresh `S1` |
| 5 | `(A ; B)` (7.14.4) | `(A' ; B')` | both branches `S0`→`S` |
| 6 | `(A \| B)` (7.14.6) | `(A' ; B')` | `'\|'`/2 *and* `;`/2 must both be recognised (§8) |
| 7 | `{G}` (7.14.7) | `(G, S0 = S)` | |
| 8 | `call(G...)` (7.14.8) | `call(G..., S0, S)` | memcpy + `arity += 2` instead of `=..`/`append`/`=..` |
| 9 | `phrase(B)` / `phrase(B,A)` / `phrase(B,A1,A2)` (7.14.9 + ext) | `phrase(..., S0, S)` | |
| 10 | `!` (7.14.10) | `(!, S0 = S)` | |
| 11 | `\+ G` (7.14.11) | `representation_error(dcg_body)` | see §5 |
| 12 | `(If -> Then)` (7.14.12) | `representation_error` at top level; `(If' -> Then')` inside `;` | see §5 |
| 13 | `M:Body` | translate `Body`, wrap as `M:Goal` | |
| 14 | other callable | nonterminal: `NT(..., S0, S)` | one alloc + memcpy + `arity += 2`; this is the hot path |
| 15 | non-callable | emit unchanged, let `call/N` throw at runtime | matches the `.pl` comment |

Head translation (`dcg_rule/2`):

* `H --> B` → `H(S0,S) :- B'(S0,S)`
* `H, PB --> B` → `H(S0,S) :- B'(S0,S1), S = <PB list with tail S1>`
  (note the argument swap in the reference: `dcg_terminals(Terminals, S, S1, _)`)
* `M:H --> B` → `M:Head :- Body`, with `M:H, PB --> B` also handled
* `H` a var → instantiation error; `H` non-callable → `type_error(callable, H)`

### A peephole worth *not* taking by default

The native translator could avoid emitting `S0 = S` goals by substituting `S0`
for `S` in the continuation. This is a real win for `{G}`-heavy grammars. But
the expansion is observable through `listing/1`, `clause/2` and ISO conformance
tests, and the standard prescribes the shape. Recommendation: implement it
behind a module-level flag (`:- set_prolog_flag(dcg_optimise, true)`), default
off, and turn it on only after the differential test harness (§10) is in place.

---

## 5. Two subtleties that a naive rewrite silently breaks

**(a) `->` and `\+` are inconsistent by design.** In `library/dcgs.pl`,
`dcg_body/4` clause 2 calls `dcg_constr(GRBody)` *before* `dcg_cbody/4`, and
`dcg_constr((If->Then))` throws `representation_error(dcg_body)`. But the
`;`-with-if-then clause of `dcg_cbody/4` calls `dcg_cbody(GRCond, ...)`
**directly**, bypassing `dcg_constr/1`. Net effect:

```prolog
a --> (b -> c).          % throws representation_error(dcg_body)
a --> (b -> c ; d).      % translates fine
```

The C translator must reproduce this asymmetry: `dcg_is_constr` reports
`->`/`\+` as constructs *and* the body entry point rejects them, while the `;`
handler dispatches to the construct handler directly.

**(b) Instantiation errors are deliberately deferred.** `error_goal/2` in the
reference swallows `instantiation_error` so the offending construct is left for
runtime, where the variable may by then be bound. The C code must emit
`phrase(Var, S0, S)` rather than throwing, and must emit
`(must_be(list, L), append(L, S, S0))` for a terminal position that is a partial
list rather than rejecting the clause. Same for `must_be/2` and `(=..)/2`
errors, which `error_goal/2` *does* re-raise.

---

## 6. Terminals, and Trealla's string cells

Case 3 is where the native version can win big or regress badly. Trealla has a
compact string representation (`TAG_CSTR` + `FLAG_CSTR_STRING`), so `"abcdef"`
in a terminal position is one cell, not twelve.

**Trealla has no partial strings** — a string cell always terminates in nil.
That constraint cuts the design in two, because the two directions of a DCG are
not symmetric here:

* **Consuming** (`S0` bound to a string, `S` unbound). The *remainder* of a
  string is itself a complete string, so a slice is representable. Matching a
  literal prefix is a `memcmp` plus a `make_slice` — O(1) cells, no
  materialisation. This path is available.
* **Generating** (`S0` unbound). `S0` must become the literal's characters
  followed by a variable tail, which by definition is not a string cell. Cons
  cells must be materialised. No way around it.

Three options for `S0 = "abc" ++ S`:

1. **Materialise to cons cells at translate time.** Simple, and matches what
   `append/3` does today. A 4 KB literal becomes ~8000 cells in the clause.
   Note this does *not* forfeit the consuming fast path: unifying
   `[a,b,c|S]` against the string `"abcdef"` has to bind `S` to `"def"`, which
   is a legal complete string, so the slice happens inside `unify.c` for free —
   **[verify]** that it does, in the string dispatch path.
2. **Emit `lists:append(Str, S, S0)`.** Clause stays small, but pays list
   traversal per call and drags a `lists` dependency into every DCG.
3. **Emit `'$string_prefix'(Str, S, S0)`, a new native builtin**, which switches
   on the mode: `memcmp` + `make_slice` when `S0` is a string, materialise when
   it is not.

Revised recommendation: **option 1 as the default**, with option 3 as a
clause-size mitigation above a threshold (~64 chars). Option 3 is no longer
about speed — assuming `unify.c` slices, option 1 is already fast on the
consuming side — it is about not embedding multi-kilobyte literals as tens of
thousands of cells in a clause.

Also handle: `[]` vs `'[]'` (Trealla distinguishes them **[verify]**), nested
strings under all three `double_quotes` settings, and improper lists.

---

## 7. Variable allocation

The translation introduces fresh variables: one `S0`, one `S`, plus one `S1`
per conjunction and per if-then. Exact count is computable in a pre-pass, so a
single right-sized allocation is possible.

**Consult time.** The clause under construction is `p->cl`, a `struct clause_`
with a flexible `cell cells[]` array of `num_allocated_cells` and a separate
`p->vartab`. Two constraints:

* The expanded clause is larger than the parsed one, so translation cannot be
  in place. Allocate a new `clause`, copy from the arena, then
  `clear_clause(p->cl); TPL_free(p->cl); p->cl = new;` — exactly the swap
  `dcg_expansion()` already performs — followed by `process_clause(p->m, p->cl, NULL)`.
* Fresh vars are `p->cl->num_vars++`, guarded against `MAX_VARS` (1024), with a
  proper `resource_error(max_vars)` rather than a bare parse failure.

The vartab question: should the fresh variables be registered by name? Two
options —

* **(a)** register `_S0`, `_S1`, ... in `p->vartab` — costs `MAX_VAR_POOL_SIZE`
  (16000 bytes) headroom and shows up in `listing/1`;
* **(b)** emit them with `FLAG_VAR_ANON | FLAG_VAR_TEMPORARY` and skip the
  vartab entirely.

Recommend (b), which also keeps singleton warnings quiet. **[verify]** against
`assign_vars()` in `parser.c` — it walks the vartab and may assume every
`var_num < num_vars` has an entry.

**Runtime.** `create_vars(q, n)` returns the base var number; emit
`make_ref(cell, base + i, q->st.cur_ctx)`, mirroring
`make_ref(tmp + …, p->cl->num_vars, 0)` in the existing `dcg_expansion`. Copy
from arena to heap with **`dup_cells`, not `copy_cells`** — the arena may hold
managed (refcounted) blobs from `{Goal}` arguments, and `heap.c`'s comments are
explicit that reference ownership must be taken before the source is released.

---

## 8. Recognising the constructs

`dcg_is_constr` is called on every body node, so it must be cheap: a switch on
`c->val_off` against the interned atom offsets, guarded by arity. Add file-local
`pl_idx` globals alongside the existing `g_dcg_s`, `g_dot_s`, `g_nil_s` etc.:

```c
static pl_idx g_dcg_bar_s, g_dcg_semi_s, g_dcg_arrow_s, g_dcg_naf_s,
              g_dcg_braces_s, g_dcg_call_s, g_dcg_phrase_s, g_dcg_colon_s;
```

interned once at module init. Watch:

* **`'|'` vs `;`.** `library(dcgs)` declares `op(1105, xfy, '|')`, and the
  reference has *separate* `dcg_constr` clauses for `(_;_)` and `(_'|'_)`. So
  `|` does **not** collapse to `;` at read time in Trealla and both functors must
  be matched. **[verify]** against the tokenizer's `double_bar` handling.
* **`{}`** is `g_braces_s`, arity 1.
* **`call/N`** is any arity ≥ 1, not just `call/1`.
* **`phrase/1..3`** as a body construct; anything else named `phrase` with a
  different arity is an ordinary nonterminal.
* **`:`/2** module qualification, which composes with all of the above.

Gate the whole thing on library(dcgs) being loaded. `struct prolog_` already
carries a `module *dcgs` field, which is the natural switch — currently `-->`
expansion fires unconditionally from `parser.c` because `dcg_translate/2` sits
in `builtins.pl`. Preserving "`-->` is an ordinary term when dcgs isn't loaded"
is a behaviour change worth checking against the test suite. **[verify]** what
sets `pl->dcgs`.

---

## 9. The Prolog side after the change

`library/dcgs.pl` shrinks to declarations plus:

```prolog
phrase(GRBody, S0) :- phrase(GRBody, S0, []).

phrase(GRBody, S0, S) :-
    strip_module(GRBody, M, B),
    (   var(GRBody)        -> instantiation_error(phrase/3)
    ;   '$dcg_body'(B, S0, S, Goal) -> call(M:Goal)
    ;   call(M:B, S0, S)
    ).
```

`'$dcg_body'/4` is native and **fails** (rather than throwing) when the term is
not a DCG construct, so the `->` fallthrough to `call/3` still works. It throws
only where ISO requires. `phrase/4` and `phrase/5` follow the same shape, with
the extra arguments appended natively instead of via `=..`/`append/3`/`=..`.

Deleted from the `.pl`: `dcg_rule/2`, `dcg_body/4`, `dcg_cbody/4`,
`dcg_constr/1`, `dcg_non_terminal/4`, `dcg_terminals/3`, `error_goal/2`,
`user:term_expansion/2`, `user:goal_expansion/2`.

Kept: the module and op declarations, the `meta_predicate` directives,
`seq//1`, `seqq//1`, `...//0`, and the `(-->)/2` stub that throws
`existence_error(procedure, (-->)/2)`.

In `library/builtins.pl`, `expand_term/2` and `dcg_translate/2` become wrappers
over native `'$dcg_rule'/2`, preserving their current behaviour.

**Optional native fast paths for `seq//1` and `...//0`.** These gain little from
C as pure translations, but they are worth a native implementation for a
different reason. This is the consuming direction of §6, where the no-partial-
strings rule does not bite: when `S0` is a string cell, each step of `...//0`
yields a *suffix*, which is a complete string, so `make_slice` advances in O(1)
instead of allocating a cons cell per character. That turns the common "skip to
a marker in a large text" idiom from O(n) cells allocated to O(1). The
generating direction falls back to the existing Prolog clauses. Treat as a
separate, later change.

---

## 10. Verification plan

The differential test is the important one, and it is cheap:

1. Keep the existing Prolog translator, renamed, in `tests/dcg_reference.pl`.
2. Generate a corpus of DCG bodies — enumerate the constructs to depth 3, plus
   the bodies appearing in `library/*.pl` and `tests/`.
3. Assert `'$dcg_rule'(T, X), dcg_reference:dcg_rule(T, Y), X =@= Y` (variant,
   not `==`, since fresh variable numbering will differ).
4. Assert error equivalence: both throw the same term, or both defer.

Plus:

* The repo's existing DCG tests must pass unchanged — that is the real contract.
* ISO 7.14 conformance cases, and the Scryer/`library(dcgs)` test set the module
  was ported from.
* Targeted cases: pushback lists; module-qualified head and body; `!` inside
  `{}` versus bare `!`; `(A->B)` at top level (must throw) versus inside `;`
  (must translate); string literals under `double_quotes` = `codes`/`chars`/`atom`;
  partial terminal lists; `[]` vs `'[]'`; a terminal list of 100k elements;
  `phrase/3` with an unbound body; a body that is a bigint.
* **C-stack safety.** `heap.c` explicitly converted its recursive clone into an
  explicit stack to survive deep terms. The DCG translator has the same
  exposure on a long left-nested conjunction spine, and a DCG body of a few
  thousand conjuncts is not exotic in a generated grammar. Use an explicit work
  stack for the conjunction/alternation spine, and add a fuzz case with a
  10,000-conjunct body.
* Run the whole suite under ASan and Valgrind — refcount handling on managed
  cells copied out of the arena is the most likely source of a leak or a
  use-after-free.

Benchmarks (measure, do not assume): consult wall time for a DCG-heavy file
(the repo's own `library/*.pl` DCG users, plus a synthetic 10k-rule grammar),
peak RSS during consult, and runtime `phrase/3` throughput on a dynamic body.
The expectation is that DCG clause expansion stops being visible in a consult
profile; the current path's cost is dominated by the tokenizer pass, which
disappears entirely.

---

## 11. Phasing

Each phase is independently revertable and independently testable.

| Phase | Change | Risk |
|---|---|---|
| 0 | Add `src/dcgs.c`, `'$dcg_rule'/2`, `'$dcg_body'/4`. `.pl` unchanged except `dcg_rule/2` delegating to the native version. Land the differential harness. | Low — new code, old path still reachable |
| 1 | Rewire `parser.c: term_expansion()` to call `dcg_translate_rule` directly. Delete `dcg_expansion()`. | Medium — this is where the speedup lands |
| 2 | Rewrite `phrase/2..5` over `'$dcg_body'/4`; delete the translator predicates from `library/dcgs.pl`. | Medium |
| 3 | Native `goal_expansion` fast path for `phrase/2,3` (the current path also prints and re-parses). | Low |
| 4 | Fix the `term_expansion` ordering FIXME: user expansion first, then DCG, then handle a list result. | Medium — behaviour change, needs its own tests |
| 5 | Optional: `'$string_prefix'`, native `seq//1` / `...//0`, `dcg_optimise` flag. | Low, opt-in |

Build integration: add `src/dcgs.c` to the Makefile source list and register
`g_dcgs_bifs[]` following the convention of the existing `bif_*.c` tables.
**[verify]** where those tables are enumerated (`prolog.c` / `module.c`).

---

## 12. Open questions

1. ~~Can a Trealla string cell carry a non-nil tail?~~ **Answered: no, there are
   no partial strings.** Terminal literals must materialise to cons cells when
   generating; the consuming direction can still slice, since a suffix of a
   string is a complete string. §6 revised accordingly. Follow-on: confirm that
   `unify.c` produces a string cell (not a cons chain) when a cons pattern with
   a variable tail is unified against a string.
2. Does `assign_vars()` require a vartab entry for every variable slot? Determines
   §7 option (a) vs (b).
3. What sets `pl->dcgs`, and should `-->` translation be gated on it? Gating is
   more correct but is a behaviour change.
4. Should the `\+ G` representation error be kept, or should `\+` translate as
   `(\+ phrase(G,S0,_), S0 = S)` (as SWI does)? Suggest keeping the error by
   default, with the translation behind the same `dcg_optimise`-style flag.
5. Does `'|'`/2 survive read as a distinct functor from `;`/2 in Trealla, given
   `op(1105, xfy, '|')` and the tokenizer's `double_bar` flag?
