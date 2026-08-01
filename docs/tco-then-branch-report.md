# TCO in the THEN branch of if-then-else

Against `trealla-prolog/trealla` @ `b932785` ("Tidy up parser.c a bit").

## The symptom

`(C -> T ; E)` at the end of a clause: a recursive call ending `E` is
tail-call optimised, the same call ending `T` is not.

```prolog
c_else(N) :- ( N =:= 0 -> true ; M is N-1, c_else(M) ).
c_then(N) :- ( N > 0  -> M is N-1, c_then(M) ; true ).
```

At 200,000 iterations, stock `tpl`:

| | active frames | TCOs |
|---|---|---|
| `c_else` | 3 | 200,000 |
| `c_then` | 200,003 | 0 |
| `c_soft_then` (`*->`) | 200,003 | 0 |

## Why

`commit_frame()` will only reuse a frame for a goal carrying
`FLAG_INTERNED_RECURSIVE_CALL`. That flag is set in `process_cell()`
(`src/module.c`) by one test:

```c
if (!is_directive && ((c + c->num_cells) >= (body + cl->cidx-1))) {
        c->flags |= FLAG_INTERNED_TAIL_CALL;
        if (parent && same functor/arity)
                c->flags |= FLAG_INTERNED_RECURSIVE_CALL;
}
```

"Does this cell end where the clause's cells end?" — pointer arithmetic
over the **source term**, applied by a flat loop over every cell. In
`p(N) :- ( C -> T ; E )` the term's last cell is the last cell of `E`,
so only `E`'s final goal is ever marked. `T` is followed in the term by
the entire else branch and never qualifies, however trivial `E` is.

Nothing else is in the way. `compile_term()` lays the construct out as

```
$succeed_on_retry(V,N1), <C>, !, $drop_barrier(V), <T>, $jump(N2), <E>, true
```

so the last goal of `T` is followed only by a jump to the landing that
ends the clause — and `is_last_call()` in `src/query.c` already walks
exactly that: it skips `$jump` with a positive offset and `true`
landings before concluding the clause is over. The run-time half of the
machinery was ready; the compile-time hint never arrived.

## The fix

**1. Mark tail positions structurally** (`src/module.c`)

A new `mark_tail_positions()` walks the control skeleton of the body
instead of reading the term's last cell, and marks every branch that
ends the clause:

- `(A , Tail)` → `Tail`
- `(Tail ; Tail)` → both
- `(_ -> Tail)`, `(_ *-> Tail)` → `Tail`
- `if(_, Tail, Tail)` → both
- anything else → the goal itself

It runs at the end of `process_clause()` and mirrors `process_cell()`'s
rules about which goals may be marked (builtins are left alone; only a
functor matching the predicate being loaded becomes a recursive call).

The mark is a hint, not a promise: `commit_frame()` still calls
`is_last_call()`, which checks the instruction stream that actually
follows the goal at run time. A mark that `compile_term()` lays out
differently than assumed costs a failed test, not correctness.

**2. Ask `commit_any_choices()` the right question** (`src/query.c`)

Enabling the branches exposed a live soundness bug that had to be fixed
alongside them. `commit_frame()` asked `ch->gen > f->chgen` — a
comparison of choice *generations*, which is not the same question as
"does a choicepoint need this frame" and got it wrong in both
directions.

*Too loose.* A choicepoint pushed by an earlier goal of the same clause
carries `gen == f->chgen` exactly, so it was invisible:

```prolog
p(0) :- !.
p(N) :- between(1,2,_), M is N-1, p(M).
```

`findall(x, p(3), L)` must yield 8 solutions. Stock `tpl` yields **4**
(`tpl -O0` yields 8) — the tail call recycled the frame `between/3`'s
choicepoint needed. The branches now eligible for TCO sit behind more of
these than a plain body does, so this had to go.

*Too tight*, had it simply been tightened to `>=` (which is what
`resume_frame()` uses). Generations do not order frames:
`commit_frame()` stamps a pending clause choice with `q->chgen`, the
generation of the frame the call goes on to create, and `$drop_barrier`
hands a frame back a generation it held earlier. So an ancestor's
choicepoint can carry `gen == f->chgen` while having nothing to do with
this frame. `samples/chess.pl` lost ~31k of its 20.7M tail calls that
way, every one of them `can_move/5` recursing under an outer clause
choice five frames up.

The frames answer it directly. `ch->st.fp` is the frame count when the
choicepoint was pushed, so `ch->st.fp > q->st.cur_ctx` means this frame
was already live and a retry restores into it; anything pushed earlier
belongs to an ancestor, and retrying that throws this frame away whole.
`commit_frame()` only reaches the test with `q->st.fp == cur_ctx + 1`,
so it reads:

```c
return ch->st.fp >= q->st.fp;
```

`skip` counts the choicepoints `commit_frame()` drops itself — the
in-progress clause choice, plus the `call/N` barrier when
`is_last_call()` found one, which is why `p(N) :- M is N-1, call(p, M)`
keeps its TCO.

## Result

At 200,000 iterations, patched:

| | active frames | TCOs |
|---|---|---|
| `c_then` | 3 | 200,000 |
| `c_soft_then` | 3 | 200,000 |
| nested if-then-else | 3 | 200,000 |

3,000,000 iterations of `then_loop/1`: **591 MB → 5.5 MB**, 0.36 s → 0.28 s.

## Verification

- `tests/run.sh`: 329 passed / 1 failed, identical to the unpatched
  build. The one failure is `tests/issues-OLD/test056.pl`, which wants
  `crypto_data_hash/3` — an artefact of building `NOSSL=1` in this
  sandbox, and it fails the same way on stock.
- The same suite under `make debug` (`-fsanitize=address`): no
  AddressSanitizer reports across the ~327 programs it completed.
- `tco-then-branch-tests.pl` (attached): deep recursion in `->`, `*->`
  and nested branches, plus solution counts for the nondeterministic
  cases that must *not* be optimised.
- No TCO lost on other shapes: `catch/3`, `findall/3`, `\+`, `once/1`,
  `call/N`, a preceding if-then-else, and a preceding builtin all still
  reuse frames exactly as before.
- `samples/chess.pl` (`tpl -q -g 'time(main),statistics,halt' -f
  samples/chess.pl`): every counter identical to stock — 20,705,572
  TCOs, 3,567,334 backtracks, 97,171,492 retries, 2,932,211 frame
  recovs, 128,081,142 matches — at 5.38 s vs 5.40 s.
  `samples/queens11.pl`: 1,513,160 TCOs on both. nrev: 1.06 s on both.
- `eyereasoner/eyelet` (`test-trealla`): all 88 inputs byte-identical to
  the committed `output-trealla/`, none slower than stock.

## 3. The `// FIXME: memory waste`

`push_succeed_on_retry_with_barrier()` set `f->no_recov = true` on the
current frame and never took it back, so a single `\+`, `ignore/1`, `\=`
or if-then-else anywhere in a body pinned that clause's frame for the
rest of the query. The unrecovered frame keeps `q->st.fp` above the
caller's `cur_ctx + 1`, so it costs the **caller** its TCO too:

```prolog
foo(N) :- ( N > 0 -> true ; true ).
loop(0) :- !.
loop(N) :- foo(N), M is N-1, loop(M).
```

`loop/1` runs in constant frames if `foo/1`'s body is anything else.
With the if-then-else: 600,002 frames per 300,000 iterations, 0 TCOs.
Same for `\+`, `ignore/1` and `\=`. (`once/1` escapes — it uses the
fail-on-retry barrier, which never set the pin.)

### What it was actually protecting

Not the choicepoint. `push_barrier()` stamps that one with `gen ==
f->chgen`, so `resume_frame()` and `commit_frame()` both see it while
it is live, and it is always gone — dropped by `$drop_barrier`, or
consumed by the retry it exists for — before the clause ends.

Removing it makes `tests/issues/test338.pl` (clpb) lose solutions, so I
went looking for what breaks. Instrumenting every frame recovery, the
first divergence from the pinned run is `bdd_restriction/4`'s frame
being reclaimed. Disabling `trim_frame()`'s three effects one at a time:

| what | test338 |
|---|---|
| don't clear the slots | still fails |
| don't lower `q->st.sp` | still fails |
| don't lower `q->st.fp` | **passes** |

So it is not the slot contents — it is the **frame index being
recycled**. Scanning every live frame, attribute list, the heap and the
trail for references to the frame at the moment it is reclaimed finds
exactly one class of holder: **12 trail entries**, no heap or attribute
references at all.

A trail entry names a variable as `(val_ctx, var_num)` — a frame index
and a slot number. `undo_me()` walks entries by index and clears the
slot each one names. Recycle the index and a later frame lands on it;
the next retry then unbinds a variable belonging to a completely
different predicate. In clpb that variable is a goal, hence
`instantiation_error`.

`trim_trail()` exists for precisely this — its own comment says a stale
entry against a recycled frame is the hazard — but it is only ever
called from `commit_frame()`. The **return path recycles frame indices
without ever cleaning the trail**, and `f->no_recov` was the plug.

### The fix that worked, and why it is not in the patch

`drop_frame_trail()`, called on the recovery path just before
`trim_frame()`: compact the trail, dropping entries that name the frame
being reclaimed. Only entries above the newest choicepoint's `tp` can
name it — below it the frame did not exist yet — and those are dead once
the frame is, because recovery only happens when no choicepoint resumes
into it. I asserted that directly (`ch->st.fp <= cur_ctx` at every call):
**0 violations** across the suite and chess.pl. `trim_frame()` unshares
the slots immediately after, so nothing leaks.

It works. The pin goes, and every `\+` / if-then-else clause recovers
its frame:

| 300,000 iterations | pinned | cleaned |
|---|---|---|
| `( N > 0 -> true ; true )` | 600,002 frames | 3 |
| `\+ N < 0` | 600,002 | 3 |
| `ignore(N >= 0)` | 600,002 | 3 |
| `N \= foo` | 600,002 | 3 |

At 1,000,000 iterations, 451 MB → 5.5 MB. test338 passes, the suite is
328/2, ASan clean including the attributed-variable tests (clpb, clpz,
freeze, dif, when).

**But the scan is unbounded, and it lands on a path that used to be
free.** `samples/takeuchi.pl` — via `eyereasoner/eyelet`, which is where
this surfaced — goes from 0.7 s to not finishing: few choicepoints means
the window between the newest choicepoint's `tp` and the top of the
trail is the whole trail, and the recursion pays it per frame. Capping
the scan only trades the work for lost recoveries, which that benchmark
feels just as badly (0.93 s at any cap, versus 0.69 s stock).

Doing it properly needs a **per-frame count of live trail entries**, so
the common case — no entry names this frame — costs nothing and the scan
only runs when there is something to find. That count has to be
maintained across `add_trail()`, `undo_me()`, `trim_trail()`, retry, and
the attributed-variable hook's saved trail window, and a missed
decrement is silent corruption. That is its own change, so this patch
documents the finding in place of the bare FIXME and leaves the pin
alone.

### Two other things the dive turned up

- `tests/tests/test104.pl`'s expected output hardcodes variable numbers
  (`freeze:freeze(_398,true)`). Anything that changes how many frames get
  recovered renumbers them, so that test will fail on any future work
  here for cosmetic reasons.
- `once/1` escapes the pin entirely — it compiles to the fail-on-retry
  barrier, which never set it. So `once(G)` already costs its caller
  nothing, while `ignore(G)` and `\+ G` cost it everything. That
  asymmetry is invisible from the Prolog side.

## Files

- `trealla-then-branch-tco.patch` — against `b932785`, `src/module.c`
  (+85) and `src/query.c` (+94/-7). The two changes are independent and
  can be split: tail-position marking, and `commit_any_choices()`.
- `tco-then-branch-tests.pl` — regression tests
