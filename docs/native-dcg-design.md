# Native C DCGs for Trealla Prolog

Design for **deleting `library/dcgs.pl`** and providing `library(dcgs)` as a
native module, with a cell-level translator in C. The module's exported
interface is preserved exactly; the shared reference implementation is not.

**Source basis.** Read from `trealla-prolog/trealla@main`: `library/dcgs.pl`,
`library/builtins.pl`, `src/parser.c` (the `dcg_expansion` / `term_expansion` /
`goal_expansion` region), `src/heap.c`, `src/heap.h`, `src/query.h`,
`src/builtins.h`, `src/internal.h`. Also
[`phrase_quad.pl`](https://www.complang.tuwien.ac.at/ulrich/iso-prolog/phrase_quad.pl)
and issues #1102, #1103.

**Second pass.** The originally unread files — `src/prolog.c`, `src/module.c`,
`src/unify.c`, `src/query.c` and the rest of `src/parser.c` — have since been
worked through, and seven of the nine open questions in §13 are now answered.
Three of those answers changed the design rather than confirming it: §6 (slices
verified, option 1 safe), §8.1 (the cross-module binding, and one struck claim)
and §10 (the variable strategy, and the phase-4 dependency it creates). A
No **[verify]** markers remain. **[tested]** marks behaviour confirmed by direct
test, on v3.1.28 or on this tree as noted. What is still open in §13 is a design
decision (5) and one thing that cannot be known before the code exists (6).

---

## 1. Why

### 1.1 The consult-time path is a text round-trip

`src/parser.c` today:

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
    query *q = query_create(p->m);                          // 1. new query
    ... make_instr(tmp, g_dcg_translate_s, ...);
    bool ok = execute(q, tmp, p->cl->num_vars+MAX_ARITY);   // 2. run Prolog
    char *src = print_canonical_to_strbuf(q, c, ...);       // 3. print to text
    strcat(src, ".");
    parser *p2 = parser_create(p->m);
    p2->srcptr = src;
    tokenize(p2, false, false);                             // 4. re-parse
    process_clause(p2->m, p2->cl, NULL);
    p->cl = p2->cl;                                         // 5. swap clause in
    ...
}
```

`dcg_translate/2` (in `library/builtins.pl`) calls `dcg_rule/2` in
`library/dcgs.pl` — a nondeterministic meta-interpreter over `=..`, `append/3`,
`subsumes_term/2` and `must_be/2`.

Per DCG clause: a query create/destroy, a full resolution, a canonical print
into a heap string, a fresh parser, a complete tokenize + operator-resolution +
`assign_vars` pass, and a clause copy.

The text hop is also semantically fragile:

| Hazard | Why it bites |
|---|---|
| Operator table | The body is printed with current ops and re-read with current ops; any term whose canonical form depends on priorities must round-trip exactly. |
| `double_quotes` flag | String literals print per the flag and re-read per the flag; the two must agree. |
| Non-textual cells | Bigints, rationals, blobs, attributed vars and stream handles inside `{Goal}` must survive print → read. Blobs cannot. |
| Embedded NUL | `strcat(src, ".")` treats the printed clause as a C string. A NUL anywhere truncates it. Any term that does not round-trip faithfully corrupts the clause — cf. #1103, where `atom_codes(A,[0])` yields an answer the reader will not accept back. |
| Variable identity | Preserved only via generated `_G<n>` names. |
| Var names | The clause's `vartab` is discarded; `listing/1` and error messages lose user variable names. |
| Errors | The only channel out is `p->error = true`; no ISO exception, no culprit. |
| The FIXME | `-->` bypasses user `term_expansion/2`, and a list result is unhandled. |

**Translate cells to cells: no query, no print, no re-parse.**

### 1.2 The Prolog implementation cannot be fixed in place

`library/dcgs.pl` is a shared reference implementation tracked against Scryer
and UWN's specification work. Trealla cannot carry a local patch to it. Since
#1102 (§5) is a defect *in that file*, the only way Trealla can fix it is to
stop using the file. Hence: the reference implementation is deleted and replaced
by a much smaller, Trealla-authored `library/dcgs.pl` of the same name — same
module, same exports, but only the declarations and the handful of predicates
that are genuinely better in Prolog. Everything else becomes C. The replacement
is Trealla's own artifact and free to change; see §8.

The deleted file should be retained in-tree as `tests/dcg_reference.pl`, never
loaded, purely as the differential oracle for §9. That keeps the ability to diff
against the reference without shipping it.

---

## 2. Scope

| Component | After |
|---|---|
| `-->`/2 clause translation | C |
| DCG body translation (`dcg_body`, `dcg_cbody`, `dcg_constr`, `dcg_non_terminal`, `dcg_terminals`) | C, internal — no longer user-visible predicates |
| Consult-time `-->` hook | C, in `parser.c` |
| Consult-time `goal_expansion` for `phrase/2,3` | C, in `parser.c` |
| `phrase/2..5` | Prolog, in the simplified `library/dcgs.pl`, over native `'$dcg_body'/4` |
| `seq//1`, `seqq//1`, `...//0` | Prolog, in the simplified `library/dcgs.pl` |
| `op(1105, xfy, '|')`, `meta_predicate` decls, `(-->)/2` stub | Prolog, in the simplified `library/dcgs.pl` |
| `use_module(library(dcgs))` | unchanged — the module still exists |
| `error_goal/2`, `user:term_expansion/2`, `user:goal_expansion/2` | gone — no longer needed |

Exported interface, unchanged:

```prolog
op(1105, xfy, '|'), phrase/2, phrase/3, phrase//2, phrase//3,
seq//1, seqq//1, ... //0, (-->)/2
```

---

## 3. Architecture

New `src/bif_dcgs.c`, following the existing `bif_*.c` convention: bif
prototypes declared alongside the others in `src/query.h`, the registration table
`g_dcgs_bifs[]` at the foot of the file, and no private header — the translator's
structs and helpers stay `static`. The two entry points `parser.c` needs are the
exception; declare those in `src/parser.h`. Two layers.

### Layer 1 — the translator (no `query` dependency)

Pure cell → cell, needed in two environments: consult time (fresh vars from
`p->cl->num_vars`, output into a new `clause`) and runtime (fresh vars from
`create_vars(q,n)`, output onto the query heap). Parameterise rather than
duplicate:

```c
typedef struct { cell *buf; unsigned len, cap; } dcg_arena;

typedef struct dcg_ctx_ {
    dcg_arena *ar;
    prolog *pl;
    module *m;
    bool (*newvar)(struct dcg_ctx_ *ctx, cell *out);  // parser or query supply
    void *owner;
    unsigned depth;
    bool oom;
    // pending error, raised by the caller in its own idiom
    const char *err_type, *err_expected;
    cell *culprit;
} dcg_ctx;
```

Emit into `ar`, then blit into final storage. Same idiom as `heap.c`: record the
start index, append children, patch `num_cells` on the way out.

```c
static unsigned emit_open(dcg_ctx *c, pl_idx functor, unsigned arity);
static void     emit_close(dcg_ctx *c, unsigned at);   // buf[at].num_cells = len - at
static bool     emit_term(dcg_ctx *c, const cell *t);
```

Public surface:

```c
int  dcg_translate_body(dcg_ctx *, const cell *body, const cell *s0, const cell *s);
int  dcg_translate_rule(dcg_ctx *, const cell *rule_term);
bool dcg_is_constr(const prolog *, const cell *);   // no alloc, no throw
```

Layer 1 never throws and never touches a query; it records error intent in
`ctx` and the caller raises it — a parser error at consult time, `throw_error()`
at runtime.

### Layer 2 — the hooks

1. `parser.c: term_expansion()` calls `dcg_translate_rule` and swaps the clause
   in. `dcg_expansion()` and its query/print/reparse are deleted.
2. `parser.c: goal_expansion()` gains a native case for `phrase/2,3` with a
   nonvar first argument.
3. `'$dcg_body'/4` — native, drives `phrase/2..5` at runtime.
4. `'$dcg_rule'/2` — native, so `expand_term/2` and `dcg_translate/2` in
   `library/builtins.pl` (Trealla's own file) keep working.

---

## 4. The translation

`translate_body(Body, S0, S)`, threading `S0`/`S` as cell values. Cases follow
ISO 7.14.

| # | Body | Emitted | Note |
|---|---|---|---|
| 1 | `Var` | `phrase(Var, S0, S)` | deferred; never throws at translate time |
| 2 | `[]` (7.14.1) | `S0 = S` | |
| 3 | `[T\|Ts]` (7.14.2) | `S0 = [T,...\|S]` | list built directly with `S` as tail — no `append/3` |
| 3b | string cell | §6 | |
| 4 | `(A, B)` (7.14.3) | `(A', B')` | one fresh `S1` |
| 5 | `(A ; B)` (7.14.4) | `(A' ; B')` | both `S0`→`S` |
| 6 | `(A \| B)` (7.14.6) | `(A' ; B')` | `'\|'`/2 *and* `;`/2 both matched (§7) |
| 7 | `{G}` (7.14.7) | `(G, S0 = S)` | contents never inspected |
| 8 | `call(G...)` (7.14.8) | `call(G..., S0, S)` | memcpy + `arity += 2` |
| 9 | `phrase(B[,A[,A2]])` (7.14.9+) | `phrase(..., S0, S)` | |
| 10 | `!` (7.14.10) | `(!, S0 = S)` | |
| 11 | `\+ G` (7.14.11) | `representation_error(dcg_body)` | §5 |
| 12 | `(If -> Then)` (7.14.12) | error at top level; `(If' -> Then')` inside `;` | §5 |
| 13 | `M:Body` | translate, wrap as `M:Goal` | |
| 14 | other callable | `NT(..., S0, S)` | alloc + memcpy + `arity += 2`; the hot path |
| 15 | nonvar, non-callable | **raise `type_error(callable, T)` during translation** | §5.3 — never emit the bare term, never defer |

Head translation:

* `H --> B` → `H(S0,S) :- B'(S0,S)`
* `H, PB --> B` → `H(S0,S) :- B'(S0,S1), S = <PB with tail S1>`
  (note the argument swap: the reference has `dcg_terminals(Terminals, S, S1, _)`)
* `M:H --> B`, and `M:H, PB --> B`
* `H` var → instantiation error; `H` non-callable → `type_error(callable, H)`

**A peephole worth *not* taking by default.** The translator could drop `S0 = S`
goals by substituting `S0` into the continuation. Real win for `{G}`-heavy
grammars, but the expansion is observable via `listing/1`, `clause/2` and
conformance tests. Put it behind a flag (`dcg_optimise`), default off, and only
after §9 is in place.

---

## 5. Semantics: what to preserve, what to fix

### 5.1 The `->` / `\+` asymmetry — preserve

In the reference, `dcg_body/4` calls `dcg_constr/1` *before* `dcg_cbody/4`, and
`dcg_constr((If->Then))` throws. But the `;`-with-if-then clause of `dcg_cbody/4`
calls `dcg_cbody(GRCond, ...)` **directly**, bypassing `dcg_constr/1`. So:

```prolog
a --> (b -> c).          % representation_error(dcg_body)
a --> (b -> c ; d).      % translates fine
```

**The quads do not require this — an earlier draft said they did.** Now that
`phrase_quad.pl` is vendored (`tests/phrase_quad.pl`), quad 22 reads:

```
22 ?- phrase('|'(([x]->[y]),[z]),L).
      representation_error(dcg_body)
   |  L=[x,y].
```

Both answers are accepted. Only quad 23 (`;` form → `L=[x,y]`) constrains
anything, and it constrains the *permissive* side. So the asymmetry is
**permitted, not mandated**. Preserving it is a compatibility choice with the
reference implementation — a good reason, and the one to state — not a
conformance obligation. Anyone later deciding to drop it is not breaking the
standard, only the reference alignment.

**[tested]** on this tree, via `dcgs:dcg_rule/2` directly:

```prolog
a --> \+ b        →  error(representation_error(dcg_body), [culprit-(\+b)])
a --> (b -> c)    →  error(representation_error(dcg_body), [culprit-(b->c)])
a --> (b -> c;d)  →  a(A,B) :- b(A,C) -> c(C,B) ; d(A,B)
```

Note the **context is `[culprit-X]`, a list**, not a conventional error context.
"Reproduce it exactly" includes that shape — reproducing only the formal class
will silently change any test that matches on the whole ball.

### 5.2 Instantiation errors are deliberately deferred — preserve

The reference's `error_goal/2` swallows `instantiation_error` so the construct is
left for runtime, where the variable may be bound by then. Emit
`phrase(Var,S0,S)` rather than throwing; emit `(must_be(list,L), append(L,S,S0))`
for a partial list in terminal position. Quads 6, 7, 8, 17, 18, 38 depend on this.

### 5.3 #1102 — fix

`1` in a non-terminal position is neither variable nor callable, so 8.18.1.3 b
requires `type_error(callable, 1)`. The reference's `dcg_non_terminal/4` computes
`GoalUniv = [1,S0,S]` and then throws it away:

```prolog
(  callable(NonTerminal) -> Goal =.. GoalUniv
;  Goal = NonTerminal   % let call/N throw an error instead of throwing one here
).
```

The extra arguments are dropped and a non-terminal `1//0` becomes a goal `1/0`.
The three failing quads each admit exactly **one** answer:

```prolog
13 ?- phrase(({fail},1),L).     type_error(callable,1).
46 ?- phrase((1,{2}),[]).       type_error(callable,1).
47 ?- phrase(({2},1),[]).       type_error(callable,1).
```

Neighbouring quads 19, 20, 21 and 48 accept alternatives, which is why they pass
today. The single-answer form is hard because ISO makes `call/1` report the
**whole body** as culprit, eagerly:

```prolog
c2 ?- call((1,fail)).     type_error(callable,(1,fail)).
c3 ?- call((fail,1)).     type_error(callable,(fail,1)).
```

So quad 46's bare-term expansion becomes `call(((1,(2,[]=_))))` and yields
`type_error(callable,(1,(2,[]=_)))` — right class, wrong culprit. Quad 15
confirms the whole-term culprit *is* correct when the non-callable sits inside
`{}`, since `{}` contents go to `call/1` unexpanded:

```prolog
15 ?- phrase({fail,1},L).   type_error(callable,((fail,1),[]=_A)).
```

The standard's distinction: a non-callable inside `{}` is `call/1`'s problem and
gets the whole-term culprit; one **in a non-terminal position** is the
*expansion's* problem and gets the bare term. Deferring conflates the two.

**Native behaviour.** A nonvar non-callable in non-terminal position is a
permanent condition — unlike a variable, `1` can never become callable — so it is
decidable at translation time and must be raised there:

* **`'$dcg_body'/4` (runtime, under `phrase/2..5`):** throw
  `type_error(callable, T)` on encountering the node, `T` the bare subterm. This
  is what fixes 13, 46, 47.
* **`goal_expansion` (compile time):** must *not* throw — compile-time expansion
  may not raise an error at a different moment than the runtime would. If
  translation would throw, decline and emit the ordinary `phrase(Body,S0,S)`
  call. With the `.pl` gone there is no `error_goal/2` to fight, so this is
  simply a branch in our own code.
* **`-->` translation (consult time):** no conformance constraint — quads 24–26
  cover only `-->` as a predicate. A consult-time `type_error(callable, T)`
  against the offending clause is the friendlier choice.

**Scope of the eager check — narrower than it looks.** It applies *only* to rows
14–15. It must not reach into `{Goal}` (quads 10, 15, 37), `call/N` arguments
(quad 32: `phrase(call([]),[])` permits `existence_error(procedure,[]/2)`), or
`phrase/1..3` constructs (quad 45: `phrase(([a],phrase(2)),[])` expects `false`,
because `[a]` fails against `[]` before the nested `phrase(2)` is reached —
checking through it would break a passing quad).

The invariant: **the translator never emits a goal that silently drops `S0`/`S`.**
Every non-terminal either threads both or raises.

**[tested]** on v3.1.28: `call(1,"abc",[])`, `X=1,call(X)` and
`phrase(1,"abc")` all report `type_error(callable,1)`. `call/N` is not
implicated; quad 2 already passes. The defect is confined to non-callables
*inside a compound body*.

**[tested]** on this tree, the three failing quads as they stand today:

```prolog
13  error(type_error(callable,((fail,_=_),1)),call/1)
46  error(type_error(callable,(1,2,_=[])),call/1)
47  error(type_error(callable,((2,[]=_),1)),call/1)
```

Right class, whole-body culprit, `call/1` as context — exactly the analysis
above, confirmed rather than assumed.

**There is already a test for this, and it is too weak to catch the fix.**
`tests/issues/test832.pl` is literally `phrase(({fail},1),_)` — quad 13 — and
`tests/issues/test832.expected` is the single line `Error: main`, which passes
on *any* error whatsoever. That is a large part of why the defect survived: git
history shows the reference realignment (`60f1811b`) landing and being reverted
the next morning (`1cf402bf`), with the test files kept. Phase 3 must tighten
that `.expected` to the specific term, or the one existing regression test for
#1102 will pass both before and after.

### 5.4 Divergence is now permanent

Because the reference `.pl` keeps the bug until UWN and Scryer change it
upstream, the native implementation is deliberately more conformant than the
oracle in §9. That divergence entry is permanent, not transitional — if the
reference is ever fixed, the entry starts failing, which is the signal to delete
it.

---

## 6. Terminals and string cells

Trealla has a compact string representation (`TAG_CSTR` + `FLAG_CSTR_STRING`), so
`"abcdef"` is one cell, not twelve. **There are no partial strings** — a string
cell always terminates in nil. That splits the two directions:

* **Consuming** (`S0` a string, `S` unbound): the *suffix* of a string is itself
  a complete string, so a slice is representable. Prefix matching is a `memcmp`
  plus `make_slice` — O(1) cells.
* **Generating** (`S0` unbound): `S0` must become the literal followed by a
  variable tail, which is not a string cell. Cons cells must be materialised.

Options for `S0 = "abc" ++ S`:

1. **Materialise at translate time.** Matches today's `append/3`. A 4 KB literal
   becomes ~8000 cells. Does *not* forfeit the consuming fast path: unifying
   `[a,b,c|S]` against `"abcdef"` binds `S` to `"def"`, a legal complete string,
   with the slice happening inside `unify.c`. **Confirmed** — see below.
2. `lists:append(Str, S, S0)` — small clause, traversal per call, `lists`
   dependency in every DCG.
3. `'$string_prefix'(Str, S, S0)`, a native builtin switching on mode: `memcmp` +
   `make_slice` when `S0` is a string, materialise otherwise.

**`unify.c` does slice — verified, so option 1 is safe.** The chain:

* `unify_internal` dispatches string × iso-list to `unify_string_to_list`
  (`unify.c` line 686).
* That walks with `LIST_TAIL`, and `list_tail` on a string is a pure offset bump
  — `parser.c` lines 74–113 handle the slice, strbuf and small-string cases and
  return a string cell in every one. No allocation, no cons.
* When the list side runs out into a variable, the closing `unify_internal`
  binds it through `set_var`, which for a non-compound does
  `e->c = *v; share_cell(v)` (`unify.c` line 279) — copied by value, refcount
  bumped. The slice is stored directly in the slot.

**[tested]**: 100,000 successive prefix peels off a 200,000-character literal
run in 0.06s. Materialising the tail would be quadratic.

**Recommendation: option 1 by default**, option 3 above ~64 chars as a
clause-size mitigation. Option 3 is therefore *not* a speed optimisation — it is
only about not embedding multi-kilobyte literals as tens of thousands of cells.

On `[]` vs `'[]'`: **[tested]** Trealla does *not* distinguish them — `[] == '[]'`
succeeds, both print as `[]`, and `atom([])` is true. So there is no second nil to
handle, unlike Scryer. Still handle nested strings under all three
`double_quotes` settings, and improper lists (quad 5: `phrase([a|b],L)` →
`type_error(list,[a|b])`).

---

## 7. Recognising constructs

`dcg_is_constr` runs on every body node, so: a switch on `c->val_off` against
interned atom offsets, guarded by arity. File-local `pl_idx` globals interned
once at init, alongside the existing `g_dcg_s`, `g_dot_s`, `g_nil_s`.

Watch:

* **`'|'` vs `;`.** The module declares `op(1105, xfy, '|')` and the reference has
  *separate* `dcg_constr` clauses for `(_;_)` and `(_'|'_)`, so `|` does not
  collapse to `;` at read time. Quad 12 (`phrase('|'([],[a]),[a])` → `true`) and
  quad 22 both exercise it. **[tested]** — both `'|'(a,b)` and the operator form
  `(a|b)` read back with functor `(|)/2`, and unification against `';'(_,_)`
  fails. No collapse; the two clauses are genuinely needed.
* `{}` is `g_braces_s`/1; `call/N` is any arity ≥ 1; `phrase/1..3` only (other
  arities are ordinary non-terminals); `:`/2 composes with all of the above.
* **`g_braces_s` is registered twice** — `prolog.c` line 570 as `"braces"` and
  line 577 as `"{}"`, the second winning. §7 depends on it meaning `{}`/1, which
  it does, but by ordering accident rather than intent. Worth not disturbing, and
  worth not relying on silently.

---

## 8. The simplified `library/dcgs.pl`

The module survives; only its contents change. Keeping it means
`use_module(library(dcgs))` needs no special-casing in `module.c`,
`op(1105, xfy, '|')` stays scoped to the module exactly as it is today rather
than becoming a global operator, and `dcgs:`-qualified calls keep resolving. The
whole file is Trealla-authored, so the §1.2 constraint does not apply to it.

(An earlier draft also cited `prolog_`'s `module *dcgs` field. That field
— `internal.h` line 937 — is **declared and never assigned or read anywhere in
`src/`**. It is vestigial and is not a reason for anything. Left alone here;
deleting it is someone else's tidy-up.)

In full, more or less:

```prolog
:- module(dcgs, [op(1105, xfy, '|'), phrase/2, phrase/3, phrase//2,
                 phrase//3, seq//1, seqq//1, ... //0, (-->)/2]).

:- meta_predicate(phrase(2, ?)).
:- meta_predicate(phrase(2, ?, ?)).
:- meta_predicate(phrase(3, ?, ?, ?)).
:- meta_predicate(phrase(4, ?, ?, ?, ?)).

phrase(GRBody, S0) :- phrase(GRBody, S0, []).

phrase(GRBody, S0, S) :-
    strip_module(GRBody, M, B),
    (   var(GRBody)                 -> instantiation_error(phrase/3)
    ;   '$dcg_body'(B, S0, S, Goal) -> call(M:Goal)
    ;   call(M:B, S0, S)
    ).

% phrase/4, phrase/5 likewise, extra args appended by '$dcg_body'

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).
seqq([])       --> [].
seqq([Es|Ess]) --> seq(Es), seqq(Ess).
... --> [] | [_], ... .

(_-->_) :- throw(error(existence_error(procedure,(-->)/2),(-->)/2)).
```

`'$dcg_body'/4` **fails** for non-constructs, so the `->` falls through to
`call(M:B, S0, S)` and quad 2 keeps working; it throws only where ISO requires,
including the §5.3 `type_error(callable, T)`.

Gone from the file, into C: `dcg_rule/2`, `dcg_body/4`, `dcg_cbody/4`,
`dcg_constr/1`, `dcg_non_terminal/4`, `dcg_terminals/3`, `error_goal/2`,
`user:term_expansion/2`, `user:goal_expansion/2` — roughly 150 lines of
meta-interpretation replaced by a table-driven walk.

Two details carried over from the reference that are easy to lose:

* `seq//1`'s first clause is not a DCG rule but a hand-written `seq/3` guarding
  `var(Xs), Cs0 == []`, and `...//0` likewise has a hand-written `.../2` clause.
  Both exist to terminate generation; keep them verbatim.
* `...//0` is written with `|`, which is the module's own exported operator — the
  file depends on its own `op/3` export being in effect while it is consulted.

**A load-order knot unties itself.** `seq//1` and friends are ordinary DCG rules
inside the very module that used to define DCG translation — today that works
only because `dcg_translate/2` in `builtins.pl` is already loaded and calls back
into `dcgs.pl` as it is being consulted. With translation native, the file is
just a consumer like any other. **Confirmed**: `load_builtins(pl)` runs at
`prolog.c` line 793, the `g_libs` bootstrap consult at line 847, same function —
bifs are registered first. Note also that only `builtins` is bootstrapped there;
`dcgs` arrives later and transitively, which matters in §8.1.

### 8.1 `expand_term/2` and `dcg_translate/2`

`library/builtins.pl` currently carries the other half of the Prolog surface:

```prolog
expand_term((H --> B), Out) :-
	dcg_translate((H --> B), Out), !.

dcg_translate(TermIn, Term) :-
	nonvar(TermIn),
	dcg_rule(TermIn, Term).
```

Both survive, rewired one level down:

```prolog
expand_term((H --> B), Out) :-
	'$dcg_rule'((H --> B), Out), !.

dcg_translate(TermIn, Term) :-
	nonvar(TermIn),
	'$dcg_rule'(TermIn, Term).
```

`expand_term/2` keeps matching only `-->` and cutting, so a non-DCG term still
fails rather than passing through — preserve that; widening it to SWI's
"unchanged if no expansion applies" is a separate decision, not a consequence of
this work.

Three things change around them.

* **`parser.c` stops calling `dcg_translate/2`.** Phase 1 deletes
  `dcg_expansion()`, which is the only C caller, so `g_dcg_translate_s` becomes
  unused and its atom registration can go with it. `expand_term/2` and
  `dcg_translate/2` cease to be load-bearing and become ordinary user-callable
  utilities — their behaviour can no longer break a consult.
* **A silent cross-module dependency disappears — and it is worse than a
  fallback.** `dcg_translate/2` calls `dcg_rule/2` *unqualified*, and `dcg_rule/2`
  is defined in the `dcgs` module and **is not in its export list**. Three things
  are now established:

  - `search_predicate` has **no** search-all-modules fallback. It tries the
    given module, then `pl->user_m`, and stops (`module.c` lines 308–322).
  - **[tested]** unqualified `dcg_rule/2` from `user` raises
    `existence_error(procedure, dcg_rule/2)`; `user:dcg_rule` and
    `builtins:dcg_rule` both raise; only `dcgs:dcg_rule` resolves.
  - **[tested]** defining `dcg_rule/2` in `user` does **not** shadow it —
    `dcg_translate/2` still reaches the `dcgs` one.

  So the binding is neither lexical nor shadowable, and `search_predicate` alone
  cannot account for it. The mechanism in the code capable of producing this is
  the per-cell `c->match` cache (`query.c` lines 1978–1986), fixed at whatever
  module happened to be current on the first successful resolution and never
  revisited. That makes this an order-dependent cached binding to a non-exported
  predicate in another module — a stronger argument for the bif than "a fallback
  we should look up". As a bif, `'$dcg_rule'/2` is globally visible by
  construction and the whole question stops existing.
* ~~**They start working before `library(dcgs)` is loaded.**~~ **Struck — this is
  wrong.** **[tested]** `expand_term((a-->b), X)` and `dcg_translate((a-->b), X)`
  both already succeed in a bare `tpl` with no `use_module(library(dcgs))`,
  because `dcgs` is loaded transitively at startup: `format`, `pio`, `dif` and
  `freeze` all `use_module(library(dcgs))`, and they are loaded anyway. There is
  no behaviour change here and no test to check.

**Sequencing.** Make this edit in **phase 0**, not phase 3. `'$dcg_rule'/2`
exists from phase 0, `dcg_rule/2` is still there as the fallback if it needs
reverting, and it puts the native translator on a real code path early — cheap
exercise well before the switchover.

---

## 9. Verification

The differential test is the important one, built so it cannot enforce the
reference's bugs.

1. Keep the deleted implementation as `tests/dcg_reference.pl` (never loaded).
   *Not needed until phase 3* — through phases 0–2 `library/dcgs.pl` is still
   live, so the oracle is just `dcgs:dcg_rule/2`. Both tests below call it that
   way and need a one-line change at the switchover.
2. Corpus, in two halves — **both built**:
   - `tests/misc/dcg_differential.pl`, 37 hand-written cases covering ISO 7.14
     alone and nested, plus the head forms.
   - `tests/misc/dcg_corpus.pl`, every `-->` rule actually in the tree. It reads
     `library/` and `tests/` with the real reader rather than generating a
     corpus file, so it cannot go stale. Currently **829 rules across 375
     files** — clpz alone contributes 414, then abnf 86, json 75, format 70,
     clpb 57.
   - `tests/misc/dcg_consult.pl`, the **consult** path. Both of the above drive
     `'$dcg_rule'/2`, which is the *runtime* path; from phase 1 the consult path
     is a separate one (named variables registered by `assign_vars`, plain
     `dup_cells`), so 829 rules could agree while consult was broken — and in
     phase 1 they did, with every regression surfacing through unrelated tests.
     These 24 rules are consulted for real and read back with `clause/2`. Three
     are asserted to *differ*, because the consult pipeline does more than
     translate: `phrase/3` gets inlined by `goal_expansion` and meta-arguments
     get module-qualified by `expand_meta_predicate`.
3. Assert variant equality, not `==`, since fresh variable numbering differs.
4. Assert error equivalence: both throw the same term, or both defer.
5. **Divergence list, checked first.** For a listed case the reference is not the
   oracle: assert the required behaviour directly, and assert that native and
   reference outputs **differ**, so the entry fails loudly if the two ever agree
   again. Currently one entry: #1102 (§5.3, §5.4).

**Two things learned building these.** The hand-written corpus passed 24 cases
before the tree corpus existed, and the *first* run of the fuller harness found
two real bugs — one of them the `'|'`-vs-`;` distinction in §5.1, which no
conformance test could have caught because quad 22 accepts either answer. A
hand-built corpus is necessary and nowhere near sufficient.

And `dcg_corpus.pl` executes no directives from the files it reads except `op/3`
and `set_prolog_flag/2`, without which a file's own operators and
`double_quotes` setting are not in effect and its terms either fail to read or
read as something else. Unreadable terms are counted, not dropped silently. The
count is currently 157, and essentially all of it is `tests/tests/*` — files
that are deliberately malformed to test the reader, and contain no DCG rules.
Library files read clean apart from 22 in `clpb.pl` and 9 in `when.pl`.

Counts go to **stderr**, which `tests/run.sh` does not capture, so stdout stays
stable as files come and go; a `Rules < 100` guard on stdout catches the
silent-zero failure mode where the scan stops finding anything.

Steps 3–4 make the reference a baseline, not an authority. Without step 5 the
harness converts every known bug into a regression test.

**ISO conformance gate.** Now vendored at `tests/misc/phrase_quad.txt`, from the
URL in the source basis. The extension is deliberately not `.pl`: `tests/run.sh`
globs `tests/misc/*` and executes everything matching `*.pl` or `*.sh`, and this
file is not executable Prolog (see below). `skiplist.c` in the same directory is
the existing precedent for a support file that lives there without being run.

Reading it changes this section in two ways.

**It is not executable Prolog.** It is a specification in a `<id> ?- Query.` /
expected-answer notation:

```
19 ?- phrase(([a|L],1),[]).
      type_error(callable,1)
   |  instantiation_error.
```

`|` separates *acceptable alternative* answers, and `...` appears as a literal
wildcard (quads 10, 15, 37). "Wire it in" is therefore not a consult: it needs a
reader for this notation plus a driver that runs each query and matches the
result against a set of permitted answers with wildcard support.

**Built, as `tests/misc/dcg_quads.pl`.** Three notes for whoever touches it:

- Alternatives are split on *lines whose first non-blank character is* `|`, not
  on the `|` character — answers legitimately contain it (`type_error(list,[a|b])`
  in quad 21).
- `...` parses as an ordinary atom, so wildcarding is just a walk replacing it
  with fresh variables, then `subsumes_term/2`.
- Answers describing *bindings* rather than an error or `true`/`false` are
  checked only as far as "did it succeed", and counted separately as `shallow`
  so the weaker check stays visible instead of inflating the pass count.

**Current state: 55 of 58 acceptable — 43 full, 12 shallow, 0 unreadable.** The
three failures are quads 13, 46 and 47, which is #1102 exactly and nothing else.
They still fail because the quads exercise `phrase/2,3` at *runtime*, which goes
through the reference's `dcg_body/4` until phase 3 swaps the module; the native
fix so far only covers `-->` translation. **Closing those three is the phase 3
gate**, and it is now a measurement rather than an aspiration.

**The 54/57 from #1102 does not correspond to the file as it stands.** It holds
58 entries: 48 numbered phrase cases, 7 `c`-prefixed (`call/1`), 3 `f`-prefixed
(`functor/3`), all 58 of which the driver reads. #1102 was probably counting an
older revision. The gate is now stated as the driver measures it — 55 of 58
today, 58 of 58 after phase 3 — rather than a number that cannot be reconciled.

(The file is placed directly in `tests/`, which `tests/run.sh` does not glob, so
it is inert until the driver exists. Also: do not confuse it with the RDF quads
work — `library/quads.pl`, `tests/misc/quads.pl`, `docs/quads-proposal.md` — an
unrelated in-progress feature that happens to share the word.)

Guard especially against over-eager checking:

| Quad | Query | Required | Guards against |
|---|---|---|---|
| 45 | `phrase(([a],phrase(2)),[])` | `false` | checking through a `phrase/1..3` construct |
| 32 | `phrase(call([]),[])` | `existence_error(procedure,[]/2)` | checking `call/N` arguments |
| 15 | `phrase({fail,1},L)` | `type_error(callable,((fail,1),[]=_))` | checking inside `{}`; whole-term culprit there |
| 10, 37 | `phrase(([a],{1}),[])` | `type_error(callable,(...,...))` | as above |
| 23 | `;` with `->` | `L=[x,y]` | breaking the permissive side of §5.1 (quad 22 accepts *either* answer and constrains nothing) |
| 41–44 | `phrase([], non_list)`, `phrase([], L, [a\|non_list])` | `false` or `type_error(list, …)` | the S0/S arguments themselves being non-lists — not covered anywhere else in this design, and not currently checked |
| 19, 20, 21, 48 | non-callable + partial list | several accepted | over-constraining; the eager check must not preempt `instantiation_error` where the quad allows it |

Plus:

* The repo's existing DCG tests, unchanged — the real contract. **One exception:**
  `tests/issues/test832.expected` must be tightened (§5.3). As it stands it
  asserts only `Error: main` and would pass both before and after the fix, so
  leaving it alone would mean shipping #1102's fix with no test that can fail.
* Targeted: pushback lists; module-qualified head and body; `!` inside `{}` vs
  bare; `double_quotes` = `codes`/`chars`/`atom`; `[]` vs `'[]'`; a 100k-element
  terminal list; `phrase/3` with an unbound body; a body that is a bigint.
* **C-stack safety.** `heap.c` converted its recursive clone into an explicit
  stack for exactly this reason. A long left-nested conjunction spine has the
  same exposure, and a few-thousand-conjunct body is not exotic in a generated
  grammar. Use an explicit work stack for the conjunction/alternation spine; fuzz
  with a 10,000-conjunct body.
* ASan and Valgrind over the suite — refcounts on managed cells copied out of the
  arena are the likeliest leak or use-after-free.

Benchmarks (measure, don't assume): consult wall time for a DCG-heavy file and a
synthetic 10k-rule grammar; peak RSS during consult; runtime `phrase/3`
throughput on a dynamic body. Expectation: DCG expansion stops being visible in a
consult profile, since the tokenizer pass disappears entirely.

---

## 10. Variable allocation

Fresh variables: one `S0`, one `S`, one `S1` per conjunction and per if-then.
Exact count is computable in a pre-pass, so a single right-sized allocation is
possible.

**Consult time.** The expanded clause is larger than the parsed one, so
translation cannot be in place: allocate a new `clause`, copy from the arena,
`clear_clause(p->cl); TPL_free(p->cl); p->cl = new;` — the swap `dcg_expansion()`
already performs — then `process_clause(p->m, p->cl, NULL)`. Fresh vars are
`p->cl->num_vars++`, guarded against `MAX_VARS` (1024) with a proper
`resource_error`, not a bare parse failure.

Vartab: either (a) register `_S0`, `_S1`, … — costs `MAX_VAR_POOL_SIZE` headroom
and pollutes `listing/1`; or (b) emit them `FLAG_VAR_ANON | FLAG_VAR_TEMPORARY`
and skip the vartab.

**Option (b) does not work. Built, and it is (a).** Three separate reasons, in
increasing order of how long each took to find:

* `FLAG_VAR_TEMPORARY` breaks head-argument sharing outright. `greet --> [h,i]`
  gave `phrase(greet,L)` → `[h,i|_]`: the head's `S` and the body's `S` stopped
  being the same variable.
* `FLAG_VAR_ANON` makes `listing/1` print these as `_` where the old round trip
  produced ordinary positional names, which several tests pin.
* **The vartab entry is not optional.** `goal_expansion()` prints a goal and
  *re-parses* it (`parser.c` ~2894 and ~2996), inheriting `p2->vartab = p->vartab`
  with `reuse = true`, so variable identity across that boundary is carried **by
  name**. A variable with no vartab entry comes back from that round trip as a
  *different* variable. Symptom: `format_//2` expanded to
  `format_cells(...,_,_)` with the S0/S thread silently dropped, and the grammar
  just failed. This is what the old expansion's generated `_G<n>` names were
  quietly doing, and it is why the hook must run **before** `assign_vars()` —
  letting it register the names is the whole mechanism.

So the ordering dependency flagged earlier is real but inverted: the translator
must run *ahead* of `assign_vars`, not after it, and phase 4 must preserve that.

**The clause must arrive with slack.** `expand_meta_predicate()`,
`goal_expansion()` and `insert_call_here()` all grow the clause in place. A
clause sized exactly to fit makes `make_room()` realloc on the first growth,
which exposed two pre-existing stale-pointer bugs in `parser.c` (both fixed in
phase 1): `expand_meta_predicate()` memmoves through a `k` invalidated by its own
`make_room()`, and `term_to_body_conversion()` reads `c->arity` after calling it.
Neither fired before because a parsed clause carries `make_room()`'s 3/2 slack.

**A note on finding that.** It presented as a segfault that *disappeared under
lldb* — heap layout differs enough there that the corrupted read landed on
mapped memory. `-fsanitize=address` (`make debug`) named it in one run. Section 9
predicted this seam; it just predicted the wrong direction, expecting leaks from
arena refcounts rather than clause-growth invariants.

**Runtime.** `create_vars(q,n)` returns the base var number; emit
`make_ref(cell, base+i, q->st.cur_ctx)`, mirroring the existing
`make_ref(tmp+…, p->cl->num_vars, 0)`. Copy arena → heap with **`dup_cells`, not
`copy_cells`**: the arena may hold managed blobs from `{Goal}` arguments, and
`heap.c` is explicit that references must be taken before the source is released.

---

## 11. `seq//1` and `...//0` — the premise was wrong

**The diagnosis here was incorrect, and the fix turned out to need no C at all.**

The reasoning was: consuming via `...//0` costs a cons cell per character, so a
native implementation using `make_slice` would turn O(n) cells into O(1). The
first half is false. Plain recursion over an 80k-character string is already
linear and fast (0.04s), because `list_tail()` slices strbuf-backed strings in
place — the same property §6 verified for `unify.c`. Cells were never the cost.

The actual cost was `... --> [] | [_], ... .` compiling to an **in-body
disjunction**, which is quadratic under deep recursion in this engine. Measured
with the same predicate written three ways, n=40000:

| form | time |
|---|---|
| `(A=B ; A=[_\|C], dots(C,B))` | 4.66s |
| two clauses | 0.04s |
| head unification | 0.04s |

Rewriting `...//0` as two rules instead of one `\|` rule took skip-to-marker on
an 80k string from 19.16s to 0.05s, and made it linear. Same solutions, same
order.

**That leaves a general engine finding worth its own investigation**, unrelated
to DCGs: an in-body disjunction appears to cost O(depth) per backtrack where
clause indexing costs O(1). Any recursive predicate written with `;` pays it.
Working around it in `...//0` is not a fix for that.

`seq//1` has no disjunction and needs nothing.

---

## 12. Phasing

| Phase | Change | Risk |
|---|---|---|
| 0 | `src/bif_dcgs.c` with the translator, `'$dcg_rule'/2`, `'$dcg_body'/4`. Point `expand_term/2` and `dcg_translate/2` in `builtins.pl` at `'$dcg_rule'/2` (§8.1). The reference `library/dcgs.pl` still live. Land the differential harness so both run side by side, and **vendor `phrase_quad.pl`** (§9) — the phase 3 gate depends on it. | Low — new code, one small live path, easily reverted |
| 1 | **Done.** Hook moved ahead of `assign_vars()` (not into `term_expansion()` — see §10); `dcg_expansion()` and both `g_dcg_translate_s` registrations deleted. clpz consult 0.78s → 0.31s; a 2000-rule synthetic grammar 0.22s → 0.12s. Also fixed two pre-existing stale-pointer bugs in `parser.c` that an exactly-sized clause exposes. | Medium — and the medium was in the parser's realloc invariants, not the translator |
| 2 | Native `goal_expansion` for `phrase/2,3`. **Measured, and the case is weaker than this row implies:** ~200 `phrase/2,3` goals exist tree-wide and the expansion costs ~35µs each, so ~7ms across the whole library — against the 470ms phase 1 saved on clpz alone. The real reason to do it is that phase 3 deletes the Prolog hook, and without a replacement that regresses *runtime* inlining. Doing it natively also needs fresh variables registered in the vartab at a point after `assign_vars` has run, which is the exact hazard §10 got wrong. **Folded into phase 3**, where the hook was being rewritten anyway — kept in Prolog over `'$dcg_body'/4` rather than moved into `parser.c`, since ~7ms does not justify vartab surgery. | Low value alone; done as part of 3 |
| 3 | **Done.** `library/dcgs.pl` replaced; reference frozen as `tests/dcg_reference.pl`; module, exports, op and `meta_predicate` declarations unchanged. **58 of 58 quads**, up from 55; #1102/#832 closed and `test832.expected` tightened from "any error" to the specific term. Three things the design missed, all recorded in §8: synthesized cells need `bif_ptr`/OP resolution before they can be *called*; `'$dcg_body'/4` declining for non-terminals pushes them onto a fallback that does not work, so `phrase/N` appends the arguments itself; and the `goal_expansion` hook must **decline** on a throwing translation rather than fall back to appending. | High — and the height was in what happens to a synthesized goal, not in the translation |
| 4 | **Half of this is already done, and the other half is bigger than "medium".** A user `term_expansion/2` returning a **list** already works — verified: the expansion is asserted and the original term is replaced. What remains is that a `-->` term never reaches a user hook. Swapping the order is not small: `term_expansion()` builds a fully processed clause through its *own* print-and-reparse, so it cannot move ahead of `assign_vars()`, and translation cannot move after it without losing the variable registration `goal_expansion` needs (§10). Doing it properly means giving `term_expansion()` the phase-1 treatment first. **Value is low** — nothing in the tree intercepts `-->`, and `library/tabling.pl` works *because* its rename runs after translation. | Was mis-sized; and note §10's dependency is the **opposite** of what an earlier draft said — translation must run BEFORE `assign_vars`, and phase 4 must preserve that |
| 5 | **Two of three done.** `'$string_prefix'/3` lands: 200 rules with a 4 KB literal drop from 2.36s / 100 MB RSS to 0.04s / 14 MB. `...//0` is 380x faster — but *not* for §11's reason, see below. `dcg_optimise` not done. | Low, opt-in — and §11's premise was wrong |

Phases 0–2 are independently revertable and leave the `.pl` in charge. Phase 3
is the commitment.

---

## 13. Open questions

Resolved earlier: `use_module(library(dcgs))` needs no special-casing now the
module survives; `op(1105, xfy, '|')` stays module-scoped rather than becoming a
global operator; `call/N` reports the right culprit and is not implicated in
#1102; Trealla has no partial strings.

**Resolved against the source since.** Recorded here because several of these
changed the design, not just confirmed it:

1. **String slices — yes.** `unify.c` binds the tail to a slice, O(1), refcounted.
   §6 option 1 is safe and option 3 is a clause-size mitigation only. See §6 for
   the chain and the timing evidence.
2. **`assign_vars()` — the question was mis-framed.** It builds the vartab rather
   than requiring it, but it runs *before* `term_expansion`, so the translator's
   fresh vars are never renumbered. Option (b), with an ordering caveat that
   phase 4 must respect. See §10.
3. **`'|'` vs `;` — distinct.** Both the canonical and operator forms read back as
   `(|)/2`. See §7.
4. **Bif registration precedes the `g_libs` consult — yes.** `prolog.c` 793 vs
   847. See §8.
7. **#832 *is* #1102.** `tests/issues/test832.pl` is `phrase(({fail},1),_)` —
   quad 13 exactly. Not "closely related": the same defect, with a test already
   in the tree whose `.expected` is too weak to fail. See §5.3.
8. **No other dependents.** `library/builtins.pl` line 88 is the only reference to
   the removed internals anywhere in `library/`, `tests/` or `samples/`.
   `clpz.pl`'s `duodcg_body` is its own predicate, not a dependency.
9. **The unqualified `dcg_rule/2` binding is not a fallback.** It is not lexical,
   not shadowable, and `search_predicate` cannot account for it. See §8.1 — and
   this one does matter beyond this design.

Still open:

5. Should `\+ G` keep the `representation_error`, or translate as
   `(\+ phrase(G,S0,_), S0 = S)` as SWI does? Quads 27–30 accept both. Suggest
   keeping the error by default, behind the `dcg_optimise`-style flag. (A design
   decision, not a fact to look up. Current behaviour is recorded in §5.1.)
6. Do quads 19, 20, 21 and 48 still pass once the eager check lands? They accept
   several answers and the fix changes *which* one Trealla gives. Both are listed,
   so it should hold — but it is the likeliest place to move a passing result.
   Genuinely cannot be answered before the implementation exists; it is the first
   thing to check when it does.
