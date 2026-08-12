# Native DCG translation — design

> **Provenance.** `src/bif_dcgs.c` refers to this document by section
> five times ("section 5.1", "5.2", "5.3", "Section 6 picked ~64"), but
> the file was not in the repository. This is a **reconstruction from
> the code**, written so those references resolve. It records what the
> implementation does and the reasoning its comments preserve; where the
> original intent cannot be recovered from the code that is marked
> *(inferred)*. Correct anything guessed wrong.

## 1. Purpose

Translate DCG rules to ordinary clauses in C rather than in Prolog.
`library/dcgs.pl` did this with `=..`, `append/3` and `subsumes_term/2`;
`bif_dcgs.c` is a cell -> cell rewrite of the same translation.

Nothing in the translator creates a query, prints a term, or re-parses
one. Output is built in a plain cell arena and blitted into final
storage by the caller.

## 2. Architecture

Two layers.

**Layer 1** is everything above the bif table. It never throws and
never touches the query heap. On failure it records error *intent* in
the `dcg_ctx` and returns `DCG_ERROR`; the caller decides what that
becomes. The one concession to "no query dependency" is `ctx->q`, used
for `deref()` and fresh variables, and only when non-NULL - at consult
time the clause cells carry no bindings and the parser supplies its own
variables.

**Layer 2** is the bifs, which turn intent into a thrown ball.

### 2.1 Three return codes, not two

    typedef enum { DCG_OK, DCG_DECLINE, DCG_ERROR } dcg_rc;

A caller must distinguish "this is not a construct I handle, fall
through to an ordinary non-terminal call" from "this is a construct and
it is malformed". `'$dcg_body'/4` turns `DECLINE` into failure, which
is what lets `phrase/3`'s last branch reach `call(M:B, S0, S)` so that
`phrase(1, L)` still reports `type_error(callable, 1)` from `call/3`.

### 2.2 The arena

Cells are appended in prefix order and `num_cells` is patched on the way
out - the idiom `heap.c` uses. Managed cells copied in from the source
are shared on the way in (`dup_cells_by_ref`) and released by
`arena_release()` if translation is abandoned. On success the buffer's
references transfer wholesale to the heap copy, so that path frees the
buffer *without* unsharing.

Ownership at the three exits of `bif_dcg_rule_2`:

| outcome | arena |
|---|---|
| `DCG_DECLINE` | `arena_release()`, bif fails |
| `DCG_ERROR` | `dcg_raise()` owns and releases it |
| `DCG_OK` | `arena_to_heap()`; release only if that fails |

## 3. Exported predicates

    '$dcg_rule'(+Rule, -Clause)            translate a whole H --> B rule
    '$dcg_body'(+Body, ?S0, ?S, -Goal)     translate one body; FAILS on DECLINE
    '$string_prefix'(+Str, ?Tail, ?S0)     S0 = Str ++ Tail

`library/dcgs.pl` calls `'$dcg_body'/4` from `phrase/3,4,5` and
`'$dcg_rule'/2` from `dcg_rule/2`.

## 4. Translation

The ISO 7.14 constructs, in the order `xlate_body()` tests them:

| section | construct | translation |
|---|---|---|
| - | var | `phrase(B, S0, S)` - deferred, never an error at translate time |
| 7.14.1 | `[]` | `S0 = S` |
| 7.14.2 | `[T\|Ts]`, string | `S0 = <Ts ++ S>`, or `'$string_prefix'/3` (section 6) |
| 7.14.3 | `(A, B)` | conjunction, threading S0 -> S1 -> S |
| 7.14.4/6 | `(A ; B)`, `(A \| B)` | both emitted as `;` |
| 7.14.7 | `{G}` | `(call(G), S0 = S)`; contents never inspected |
| 7.14.8/9 | `call//N`, `phrase//1..3` | arguments not inspected |
| 7.14.10 | `!` | `(!, S0 = S)` |
| 7.14.11 | `\+ G` | **error** - see 5.1 |
| 7.14.12 | `(If -> Then)` at top level | **error** - see 5.1 |
| 7.14.13/14 | `M:Body`, any other callable | ordinary non-terminal |

An if-then-else *inside* `;` is handled by `xlate_alt()` and does not
reach the top-level `->` test.

`MAX_DCG_DEPTH` (2000) bounds the conjunction/alternation spine and the
module-qualification nest. The terminal-list walk is iterative and
deliberately not covered by it, because list length is the one thing
that is genuinely unbounded.

## 5. Errors

### 5.1 Constructs that are errors

`\+ G` and a top-level `(If -> Then)` raise
`representation_error(dcg_body)` with a `[culprit-Term]` context.

This is not an omission - it reproduces the reference exactly:

    dcg_constr(\+ G_0) :- % 7.14.11 - not (existence implementation def.)
        throw(error(representation_error(dcg_body), [culprit- (\+ G_0)])).
    dcg_constr((If->Then)) :- % 7.14.12 - if-then (existence implementation def.)
        throw(error(representation_error(dcg_body), [culprit- (If->Then)])).

The standard marks both "existence implementation defined". Note the
mechanism, because it is what makes the rejection total: the throw is in
`dcg_constr/1`, which `dcg_body/4` calls as a *guard*, so an offending
construct anywhere in a body aborts the whole expansion rather than just
that subterm.

Note for users porting code: **SWI and Scryer disagree here.** SWI's
`boot/dcg.pl` translates `\+ G` to `(\+ G'(S0,_), S0 = S)` and accepts
`->` and `*->` at top level; Scryer rejects both, and Trealla follows
Scryer. This predates the native translator.

`throw_error3()` composes from a type/expected pair and its context is
always `Name/Arity`, so it cannot produce these two shapes:
`[culprit-Term]` for `\+` and `->`, and `must_be/2` for a partial
terminal list. `bif_dcgs.c` builds them directly. Matching the
reference's exact error terms keeps the differential harness a tight
net, so the divergence list stays deliberate semantic choices rather
than incidental formatting.

### 5.2 Deferred conditions

A **var body** is not an error at translation time - it becomes
`phrase(B, S0, S)` and is decided at runtime, when B may be bound.

A **partial terminal list** (`[x|_]`) is a `must_be(list, _)` condition.
The reference's `goal_expansion` path swallows the instantiation error
and defers to runtime, where the tail may be bound by then.

*That deferral is not implemented.* Until it is, `'$dcg_body'/4` raises
where `dcg_body/4` under `goal_expansion` would have deferred. This is
the one known incompleteness in the translator.

### 5.3 Why intent is recorded rather than thrown

Layer 1 records intent instead of throwing because **compile-time
expansion and runtime translation want to raise at different moments**.
At consult time a malformed body should become a parser error attached
to the source position; at runtime the same body should raise from the
call. A single function that threw immediately could only serve one of
those. Returning `DCG_ERROR` with the intent in the context lets each
caller choose. *(inferred from the layer split and `dcg_raise()`)*

## 6. String terminals and the 64-byte crossover

A string terminal is normally materialised into the clause as two cells
per character. Above `DCG_STRING_INLINE_MAX` (64 bytes) it is emitted as

    '$string_prefix'(Str, S, S0)

instead. **The cost being avoided is clause size, not speed.** A long
literal would otherwise put thousands of cells into every clause that
mentions it.

64 is a judgement, not a measurement *(inferred - the source says
"Section 6 picked ~64 as the crossover" without giving a derivation)*.

## 7. Deliberate divergence from the reference

One, and it is permanent. A nonvar non-callable in non-terminal
position raises `type_error(callable, T)` here, reporting the bare
subterm. The reference drops the S0/S arguments and lets `call/1`
report the whole body.

That is issue #1102 (== #832). The condition is permanent, so the
native translator decides it at translation time. The reference cannot
be fixed in place - it is shared with Scryer and UWN - so this is not
transitional, and it is why `'$dcg_rule'/2` is not a drop-in oracle
match for `dcg_rule/2`.

### 7.1 Considered and rejected: translating `\+`

Following SWI and translating `\+ G` to `(\+ G'(S0,_), S0 = S)` was
tried and reverted. It works and is cheap - `xlate_body()` case 11
recursing instead of calling `set_ball_repr()`, and `dcg_is_constr()`
already answers true for `\+`/1, so `phrase/3` needs no change - but it
costs more than it looks:

- It is a **second** permanent divergence, and the two compound. Once
  `\+` is translated, the argument reaches non-terminal position, so
  `phrase(([a],\+1),[])` lands on #1102 and raises
  `type_error(callable,1)` where `phrase_quad.txt` lists only
  `representation_error(dcg_body)` or `false`. That is two new
  deviations from the ISO conformance spec (quads 29 and 30), not a
  relocation of the existing one. SWI answers those two the same way.
- The differential harness needs a divergence entry whose native side
  *succeeds*, which the `err(Formal)`-only entry shape does not express.
- It leaves Trealla in neither camp anyway, since a bare `->` would
  still raise while SWI accepts `->` and `*->`.

If it is revisited, the missing piece on the SWI side is `dcg_no_extend/1`:
SWI raises `permission_error(define, dcg_nonterminal, \+x)` for
`(\+ x) --> Body`, and neither `xlate_nonterminal()` nor the reference
has any such guard.

## 8. Testing

Five files under `tests/misc/`: `dcg_consult`, `dcg_corpus`,
`dcg_differential`, `dcg_quads`, `dcg_tabling`.

`dcg_differential` compares native output against
`tests/dcg_reference.pl`, a frozen copy of the shared implementation's
translation core, loaded only by the tests. Its divergence list is
checked **first**, and for a listed case the reference is not the
oracle: the required behaviour is asserted directly *and* the two are
asserted to differ, so the entry fails loudly if they ever agree again.
Without that, a harness like this quietly converts every known defect
into a regression test.

A divergence entry states the required native answer as a formal error
term, because every current entry is one. A divergence where the native
side *succeeds* - as translating `\+` would be (7.1) - needs a second
entry shape, pinning the emitted clause with `variant/2`.

`dcg_quads` runs `tests/misc/phrase_quad.txt`, the ISO conformance
notation. That file is a specification and is **not** edited; a
deliberate deviation from it would need its own list in the driver, on
the same "asserted, not quietly passing" terms.

## 9. Known limits

- The `goal_expansion` deferral of 5.2 is not implemented.
- 64 bytes (section 6) is unmeasured.
- Atom offsets are cached in file statics. That is sound - `g_symtab`
  is process-global behind `g_symtab_guard` - but see the comment on
  `dcg_init_atoms()` for why the "already done" fast path was removed.
