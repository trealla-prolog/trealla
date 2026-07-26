# Two leak fixes — the `trim_trail` one, done properly this time

`leak-fix-trim-trail.patch` (src/query.c) and
`leak-fix-findall-queue.patch` (src/builtins.h, src/bif_predicates.c).
Both apply clean to `271703b`, independent of the tabling patch and of
each other.

## What `trim_trail()` is actually for

My first attempt at this was withdrawn after it double-freed in
Logtalk. Retaining trail entries is not free: `trim_frame()` lowers
`q->st.sp`, a later frame reuses those slot indices, and a retained
entry naming `(ctx, var)` then points at somebody else's live binding.
`undo_me()` unshares a reference it does not own → double free →
`Trace/BPT trap: 5` on macOS.

**So `trim_trail()` is not an optimization. It is the invariant that
makes frame recovery safe:** no live trail entry may name a frame that
`trim_frame()` might recycle.

Three experiments pinned that down:

| build | iso8601 (ASAN) | `append` leak |
|---|---|---|
| stock | clean | 4600 B / 200 |
| stock, `-O0` (no TCO, no recovery) | clean | **4600 B / 200 — still leaks** |
| retain all entries | **double free** | fixed |
| retain all entries, `-O0` | clean | fixed |
| **retain only for `no_recov` frames** | **clean** | **fixed** |

The `-O0` rows are the informative ones: the leak survives with
recovery off, so recovery does not cause it — but retaining entries is
only *unsafe* when recovery is on. Those are two different mechanisms,
which is why the first patch looked fine on the Linux suite and blew up
in Logtalk.

## The fix

Retain the entry only when the frame is marked `no_recov`.

`set_var()` sets `no_recov` when a binding escapes to another frame —
which is exactly the frame that `trim_frame()` will refuse to recycle.
So there is no stale-entry hazard, and it is precisely the population
that leaks: a managed cell sitting in a frame nothing will ever clean
up. Recoverable frames keep being trimmed as before, so the recovery
invariant holds. Entries after `reuse_frame()` are always dropped —
that path already unshared the old contents and moved the new cells in
by plain copy, so undoing against them would unshare the transfer.

    $ ./tpl -g "( between(1,200,_), atom_codes(abcdef,Cs), append([_],_,Cs), fail ; true ), halt"
    before: 4600 byte(s) leaked in 200 allocations
    after:  clean

## The findall queue fix (unchanged, was already good)

Solutions are shared into the queue by `alloc_queuen()` →
`dup_cells()`; the result list is rebuilt by `end_list()`, which shares
again; the buffer was then freed raw, so its reference was never
released. `free_solns()` unshares before freeing, **after**
`end_list()` — before it, the queue holds the last reference and the
list gets built over freed memory. `bif_sys_list_1()` deliberately
transfers ownership via an unsafe copy, so that path still frees raw.

## Validation — on the thing that caught the last attempt

- **Logtalk contributions under ASAN**, all five test sets: iso8601
  113 tests / 106 passed, pddl_parser 8 / 7 passed, xml_parser 2 / 2,
  flags, verdi_neruda. **No ASAN errors anywhere.** This is the run
  that found the previous bug on the first try.
- **Full 251-file ASAN sweep** of `tests/tests`, `tests/issues`,
  `tests/misc` looking for use-after-free / double-free / SEGV: **zero
  corruption**. (Last time I swept 44 files. This time all of them.)
- `make test`: 313 tests, output identical to baseline — the single
  failure is this sandbox building `NOSSL`.
- `tests/misc/tabling.pl`: 16/16.
- Leak checks: append-split loop 0, findall of strings 0,
  findall of slices 0, findall+number_codes 0.

## Caveat worth your judgement

`no_recov` frames now keep their trail entries until the next
backtrack, so the trail can be longer in code that binds strings or
bignums into escaping frames. No measurable cost in anything I ran, but
it is a real trade and you know the workloads.
