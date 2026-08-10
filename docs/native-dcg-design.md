# Native C DCGs for Trealla Prolog

Design for **deleting `library/dcgs.pl`** and providing `library(dcgs)` as a
native module, with a cell-level translator in C. The module's exported
interface is preserved exactly; the shared reference implementation is not.

**Source basis.** Read from `trealla-prolog/trealla@main`: `library/dcgs.pl`,
`library/builtins.pl`, `src/parser.c` (the `dcg_expansion` / `term_expansion` /
`goal_expansion` region), `src/heap.c`, `src/heap.h`, `src/query.h`,
`src/builtins.h`, `src/internal.h`. Also
[`phrase_quad.pl`](https://www.complang.tuwien.ac.at/ulrich/iso-prolog/phrase_quad.pl)
and issues #1102, #1103. Not read: `src/prolog.c`, `src/module.c`, `src/unify.c`,
the rest of `src/parser.c`. Assumptions resting on those are marked **[verify]**.

Behaviour confirmed on v3.1.28 by direct test is marked **[tested]**.

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

Quad 22 (`phrase('|'(([x]->[y]),[z]),L)` → `representation_error(dcg_body)`) and
quad 23 (`;` form → `L=[x,y]`) pin this down. Reproduce it exactly.

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
   `[a,b,c|S]` against `"abcdef"` must bind `S` to `"def"`, a legal complete
   string, so the slice happens inside `unify.c` — **[verify]** that it does.
2. `lists:append(Str, S, S0)` — small clause, traversal per call, `lists`
   dependency in every DCG.
3. `'$string_prefix'(Str, S, S0)`, a native builtin switching on mode: `memcmp` +
   `make_slice` when `S0` is a string, materialise otherwise.

**Recommendation: option 1 by default**, option 3 above ~64 chars as a
clause-size mitigation. Assuming `unify.c` slices, option 3 is not a speed
optimisation — it is about not embedding multi-kilobyte literals as tens of
thousands of cells.

Also handle `[]` vs `'[]'` (**[verify]** that Trealla distinguishes them), nested
strings under all three `double_quotes` settings, and improper lists (quad 5:
`phrase([a|b],L)` → `type_error(list,[a|b])`).

---

## 7. Recognising constructs

`dcg_is_constr` runs on every body node, so: a switch on `c->val_off` against
interned atom offsets, guarded by arity. File-local `pl_idx` globals interned
once at init, alongside the existing `g_dcg_s`, `g_dot_s`, `g_nil_s`.

Watch:

* **`'|'` vs `;`.** The module declares `op(1105, xfy, '|')` and the reference has
  *separate* `dcg_constr` clauses for `(_;_)` and `(_'|'_)`, so `|` does not
  collapse to `;` at read time. Quad 12 (`phrase('|'([],[a]),[a])` → `true`) and
  quad 22 both exercise it. **[verify]** against the tokenizer's `double_bar`.
* `{}` is `g_braces_s`/1; `call/N` is any arity ≥ 1; `phrase/1..3` only (other
  arities are ordinary non-terminals); `:`/2 composes with all of the above.

---

## 8. The simplified `library/dcgs.pl`

The module survives; only its contents change. Keeping it means
`use_module(library(dcgs))` needs no special-casing in `module.c`,
`op(1105, xfy, '|')` stays scoped to the module exactly as it is today rather
than becoming a global operator, `dcgs:`-qualified calls keep resolving, and
`prolog_`'s `module *dcgs` field keeps its meaning. The whole file is
Trealla-authored, so the §1.2 constraint does not apply to it.

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
just a consumer like any other. **[verify]** bif registration precedes the
`g_libs` consult.

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
* **A silent cross-module dependency disappears.** `dcg_translate/2` currently
  calls `dcg_rule/2` *unqualified*, and `dcg_rule/2` is defined in the `dcgs`
  module and **is not in its export list**. That resolves only by whatever
  fallback Trealla uses for unqualified cross-module calls, and it means
  `builtins.pl` quietly depends on `library(dcgs)` having been loaded first. As a
  bif, `'$dcg_rule'/2` is globally visible by construction and the dependency
  becomes explicit. **[verify]** how the current call resolves — if it relies on a
  search-all-modules fallback, that is worth knowing about independently.
* **They start working before `library(dcgs)` is loaded**, since a bif is always
  present. Today `expand_term((a-->b), X)` presumably errors or fails without
  dcgs; afterwards it works. A behaviour change, and an improvement, but check no
  test asserts the old behaviour.

**Sequencing.** Make this edit in **phase 0**, not phase 3. `'$dcg_rule'/2`
exists from phase 0, `dcg_rule/2` is still there as the fallback if it needs
reverting, and it puts the native translator on a real code path early — cheap
exercise well before the switchover.

---

## 9. Verification

The differential test is the important one, built so it cannot enforce the
reference's bugs.

1. Keep the deleted implementation as `tests/dcg_reference.pl` (never loaded).
2. Corpus: constructs enumerated to depth 3, plus every DCG body in
   `library/*.pl` and `tests/`.
3. Assert `'$dcg_rule'(T,X), dcg_reference:dcg_rule(T,Y), X =@= Y` — variant, not
   `==`, since fresh variable numbering differs.
4. Assert error equivalence: both throw the same term, or both defer.
5. **Divergence list, checked first.** For a listed case the reference is not the
   oracle: assert the required behaviour directly, and assert that native and
   reference outputs **differ**, so the entry fails loudly if the two ever agree
   again. Currently one entry: #1102 (§5.3, §5.4).

Steps 3–4 make the reference a baseline, not an authority. Without step 5 the
harness converts every known bug into a regression test.

**ISO conformance gate.** #1102 reports 54/57 phrase quads passing. Wire
`phrase_quad.pl` in and require 57/57 before the module switches over. Guard
especially against over-eager checking:

| Quad | Query | Required | Guards against |
|---|---|---|---|
| 45 | `phrase(([a],phrase(2)),[])` | `false` | checking through a `phrase/1..3` construct |
| 32 | `phrase(call([]),[])` | `existence_error(procedure,[]/2)` | checking `call/N` arguments |
| 15 | `phrase({fail,1},L)` | `type_error(callable,((fail,1),[]=_))` | checking inside `{}`; whole-term culprit there |
| 10, 37 | `phrase(([a],{1}),[])` | `type_error(callable,(...,...))` | as above |
| 22, 23 | `'\|'`/`;` with `->` | `representation_error` / `L=[x,y]` | losing the §5.1 asymmetry |
| 19, 20, 21, 48 | non-callable + partial list | several accepted | over-constraining; the eager check must not preempt `instantiation_error` where the quad allows it |

Plus:

* The repo's existing DCG tests, unchanged — the real contract.
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
and skip the vartab. Recommend (b), which also keeps singleton warnings quiet.
**[verify]** against `assign_vars()`, which may assume every `var_num <
num_vars` has an entry.

**Runtime.** `create_vars(q,n)` returns the base var number; emit
`make_ref(cell, base+i, q->st.cur_ctx)`, mirroring the existing
`make_ref(tmp+…, p->cl->num_vars, 0)`. Copy arena → heap with **`dup_cells`, not
`copy_cells`**: the arena may hold managed blobs from `{Goal}` arguments, and
`heap.c` is explicit that references must be taken before the source is released.

---

## 11. Optional: native `seq//1` and `...//0`

Little gain as pure translations, but worth native implementations for a
different reason. This is the consuming direction of §6, where the
no-partial-strings rule does not bite: with `S0` a string cell, each step of
`...//0` yields a suffix — a complete string — so `make_slice` advances in O(1)
instead of allocating a cons cell per character. Turns "skip to a marker in a
large text" from O(n) cells to O(1). The generating direction falls back to the
Prolog clauses. Separate, later change.

---

## 12. Phasing

| Phase | Change | Risk |
|---|---|---|
| 0 | `src/bif_dcgs.c` with the translator, `'$dcg_rule'/2`, `'$dcg_body'/4`. Point `expand_term/2` and `dcg_translate/2` in `builtins.pl` at `'$dcg_rule'/2` (§8.1). The reference `library/dcgs.pl` still live. Land the differential harness so both run side by side. | Low — new code, one small live path, easily reverted |
| 1 | Rewire `parser.c: term_expansion()` to call `dcg_translate_rule` directly; delete `dcg_expansion()` and the now-unused `g_dcg_translate_s`. | Medium — where the speedup lands |
| 2 | Native `goal_expansion` for `phrase/2,3`, replacing the print-and-reparse path there too. | Low |
| 3 | **Replace `library/dcgs.pl`** with the simplified version (§8); move the reference to `tests/dcg_reference.pl`. Module, exports, op and `meta_predicate` declarations unchanged. Gate on 57/57 quads. | High — the switchover; #1102 closes here |
| 4 | Fix the `term_expansion` ordering FIXME: user expansion first, then DCG, then handle a list result. | Medium — behaviour change, own tests |
| 5 | Optional: `'$string_prefix'`, native `seq//1` / `...//0`, `dcg_optimise` flag. | Low, opt-in |

Phases 0–2 are independently revertable and leave the `.pl` in charge. Phase 3
is the commitment.

---

## 13. Open questions

Resolved along the way, kept for the record: `use_module(library(dcgs))` needs no
special-casing now the module survives; `op(1105, xfy, '|')` stays module-scoped
rather than becoming a global operator; `call/N` reports the right culprit and is
not implicated in #1102; Trealla has no partial strings.

Still open:

1. Does `unify.c` bind the tail to a string *slice* (not a cons chain) when
   `[a,b,c|S]` meets `"abcdef"`? Decides whether §6 option 1 keeps the consuming
   fast path. Highest priority — it is the difference between a large literal
   costing one `memcmp` or thousands of cells per call.
2. Does `assign_vars()` require a vartab entry for every slot? Decides §10 (a) vs (b).
3. Does `'|'`/2 survive read as a distinct functor from `;`/2, given
   `op(1105,xfy,'|')` and the tokenizer's `double_bar` flag? Quads 12 and 22
   exercise it.
4. Does bif registration precede the `g_libs` consult, so the simplified
   `dcgs.pl` can contain DCG rules (§8)?
5. Should `\+ G` keep the `representation_error`, or translate as
   `(\+ phrase(G,S0,_), S0 = S)` as SWI does? Quads 27–30 accept both. Suggest
   keeping the error by default, behind the `dcg_optimise`-style flag.
6. Do quads 19, 20, 21 and 48 still pass once the eager check lands? They accept
   several answers and the fix changes *which* one Trealla gives. Both are listed,
   so it should hold — but it is the likeliest place to move a passing result.
7. Does #832 overlap with #1102? Cited as closely related; not read.
8. Does anything else in the tree depend on `dcgs:dcg_rule/2`, `dcgs:dcg_body/4`
   or the other removed internals by name? `library/builtins.pl` is handled in
   §8.1; there may be others.
9. How does `builtins.pl`'s unqualified call to `dcg_rule/2` currently resolve,
   given it is neither local nor exported from `dcgs`? See §8.1 — the answer
   matters beyond this design.
